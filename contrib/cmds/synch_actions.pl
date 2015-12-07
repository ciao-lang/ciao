:- module(_, [main/1], [assertions]).

:- use_package(assertions).

:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, reverse/2]).
:- use_module(library(sets), [ord_subtract/3]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(system), [
        directory_files/2,
        file_property/2,
        file_exists/1
                               ]).

:- data new_base/1.
:- data old_base/1.

msg("

Usage: synch_actions <output_format> <old_dir> <new_dir>

Determine which actions and on which files must be done in order to update
<old_dir> to be <new_dir>.  The actions are expressed in terms of
additions and removals of files and directories.  Special files (such as
soft link, named sockets, and devices are skipped.  Files are not
examined: only their names (i.e., their presence) and relative type is
checked.  The format of the output is determined by <output_format>, which
can be one of:

   symbols : output a term for every change to be performed
   shell:    output a shell command for every change; executing all shell
             commands will bring old_dir in sync with new_dir
   cvs:      out shell commands and cvs commands to bring old_dir in sync
             with new_dir and to propagate these changes to a CVS repository.
   diff:     output actions in a more human readable format.
").


main([Output, OldDir, NewDir]):-
% If the files inside a directory are sorted, we will have
% much better results in terms of speed when it comes to
% comparing the contents of two directories.
        (
            file_exists(OldDir),
            file_exists(NewDir),
            file_property(OldDir, type(directory)),
            file_property(NewDir, type(directory)) ->
            asserta_fact(new_base(NewDir)),
            asserta_fact(old_base(OldDir)),
            display('# Reading first directory'), nl,
            read_tree_top(OldDir, directory(_, _, OldTree)),
            display('# Reading second directory'), nl,
            read_tree_top(NewDir, directory(_, _, NewTree)),
            display('# Comparing directories'), nl,
            compare_trees(OldTree, NewTree, ListOfActions-[]),
            display('# Outputting differences'), nl,
            output(Output, ListOfActions),
            display('# Done'), nl, nl

        ;
            format("~nBoth arguments *must* be existing directories~n~n", [])
        ).

% No arguments or wrong formats
main(_):-
        msg(M),
        format(M, []).


output(symbols, ListOfActions):-
        print_actions(ListOfActions).
output(shell, ListOfActions):-
        print_shell_actions(ListOfActions).
output(cvs, ListOfActions):-
        print_cvs_actions(ListOfActions).
output(diffs, ListOfActions):-
        print_diffs(ListOfActions).
output(diff, ListOfActions):- 
        output(diffs, ListOfActions).



%% Create a simbolic tree, labeled with leave/2, directory/3, and
%% unknown/2 nodes.  Directories have a list of entries, sorted by
%% entry name.


read_tree_top(FullPath, Tree):-
        path_components(FullPath, Path, File),
        read_tree(Path, File, Tree).

% Do not treat simbolic links, even if they exist.  Issue a warning.
read_tree(Path, File, unknown(Path, File)):-
        path_components(WholePath, Path, File),
        file_exists(WholePath),
        file_property(WholePath, linkto(_)),
        !.
read_tree(Path,  File, leave(Path, File)):-
        path_components(WholePath, Path, File),
        file_exists(WholePath),
        file_property(WholePath, type(regular)),
        !.
read_tree(Path, File, directory(Path, File, Descendants)):-
        path_components(WholePath, Path, File),
        file_exists(WholePath),
        file_property(WholePath, type(directory)),
        !,
        sorted_files_from_dir(WholePath, SortedFiles),
        traverse_descendants(SortedFiles, WholePath, Descendants).
% Here if everything else fails
read_tree(Path, File, unknown(Path, File)).


traverse_descendants([], _WP, []).
traverse_descendants([F|Fs], WholePath, [D|Ds]):-
        read_tree(WholePath, F, D),
        traverse_descendants(Fs, WholePath, Ds).


%% compare_trees(OldTree, NewTree): issue the necessary commands to
%% convert OldTree into NewTree.  The commands will all change OldTree
%% with file removals, directory creations, and file copies from
%% NewTree to OldTree.

%% End of the comparison
compare_trees([], [], Act-Act).
%% Finish one of the subtres
compare_trees([], [N|Ns], InAct-OutAct):-
        add_to_old([], N, InAct-MidAct),
        compare_trees([], Ns, MidAct-OutAct).
compare_trees([O|Os], [], InAct-OutAct):-
        remove_from_old(O, InAct-MidAct),
        compare_trees(Os, [], MidAct-OutAct).
%% The rest of the comparisons are driven by the {file,directory} names
compare_trees([O|Os], [N|Ns], InAct-OutAct):-
        arg(2, O, OldFile),
        arg(2, N, NewFile),
        OldFile = NewFile,
        same_names(O, N, InAct-MidAct),  %% The types might be different
        compare_trees(Os, Ns, MidAct-OutAct).
compare_trees([O|Os], [N|Ns], InAct-OutAct):-
        arg(2, O, OldFIle),
        arg(2, N, NewFile),
        OldFIle @< NewFile,   % O has dissapeared from the new file
        remove_from_old(O, InAct-MidAct),
        compare_trees(Os, [N|Ns], MidAct-OutAct).
compare_trees([O|Os], [N|Ns], InAct-OutAct):-
        arg(2, O, OldFile),
        arg(2, N, NewFile),
        NewFile @< OldFile,   % N appears in the new directory
        add_to_old(O, N, InAct-MidAct),
        compare_trees([O|Os], Ns, MidAct-OutAct).

%% Same name, two different types: change
same_names(Old, New, InAct-OutAct):-
        (
            (Old = directory(_, _, _), New = leave(_, _))
        ;
            (Old = leave(_, _), New = directory(_, _, _))
        ),
        !,
        remove_from_old(Old, InAct-MidAct),
        add_to_old(Old, New, MidAct-OutAct).
%% Directories: recurse into them
same_names(directory(_, _, OldCont), directory(_, _, NewCont), Actions):- 
        !,
        compare_trees(OldCont, NewCont, Actions).
same_names(leave(_OldPath, File), leave(NewPath, File), [copy(Full)|I]-I):-
        !,
        path_components(Full, NewPath, File).
same_names(_, _, I-I).


remove_from_old(unknown(Path, File), ['DONOTKNOW-REMOVE'(Full)|I]-I):-
        path_components(Full, Path, File).
remove_from_old(leave(Path, File), [remove(Full)|I]-I):-
        path_components(Full, Path, File).
remove_from_old(directory(_Path, _Dir, Contents), Actions):-
        remove_components(Contents, Actions).

remove_components([], A-A).
remove_components([C|Cs], A0-A2):-
        remove_from_old(C, A0-A1),
        remove_components(Cs, A1-A2).

add_to_old(_Old, New, Actions):-
        ato(New, Actions).

ato(leave(Path, File), [add(Full)|I]-I):-
        path_components(Full, Path, File).
ato(directory(Path, Dir, Contents), [mkdir(Full)|I0]-I1):-
        path_components(Full, Path, Dir),
        add_components(Contents, I0-I1).
ato(unknown(Path, File), ['DONOTKNOW-ADD'(Full)|I]-I):-
        path_components(Full, Path, File).

add_components([], I-I).
add_components([C|Cs], I0-I2):-
        ato(C, I0-I1),
        add_components(Cs, I1-I2).


sorted_files_from_dir(Dir, FDir):-
        directory_files(Dir, UnorderedFiles),
        sort(UnorderedFiles, OrdFiles),
        ord_subtract(OrdFiles, ['.','..','CVS'], FDir).


cannot_handle_file(F):-

        \+ file_exists(F),
        !.
cannot_handle_file(F):-
        file_exists(F),
        file_property(F, type(T)),
        T \== regular,
        T \== directory.

unslash(Path, RawPath):-
        (
            atom_concat(RawPath, '/', Path) ->
            true
        ;
            Path = RawPath
        ).

%slash(Path,

path_components(RawPath, Dir, File):-
        atom(RawPath),
        !,
        unslash(RawPath, Path),
        atom_codes(Path, PathCodes),
        reverse(PathCodes, PCR),
        first_component(PCR, FCR, DCR),
        reverse(DCR, DC),
        reverse(FCR, FC),
        atom_codes(Dir, DC),
        atom_codes(File, FC).
path_components(Path, RawDir, File):-
        atom(RawDir),
        atom(File),
        !,
        unslash(RawDir, Dir),
        atom_codes(Dir, DirCodes),
        atom_codes(File, FileCodes),
        reverse(DirCodes, DCR),
        reverse(FileCodes, FCR),
        first_component(PCR, FCR, DCR),
        reverse(PCR, PC),
        atom_codes(Path, PC).

%first_component([0'/|Rest], [], [0'/|Rest]).
first_component([0'/|Rest], [], Rest).
first_component([Code|RestCodes], [Code|RC], Rest):-
        Code \== 0'/,
        first_component(RestCodes, RC, Rest).


print_tree(leave(Path, File)):-
        path_components(Whole, Path, File),
        display('  '),
        display(Whole),
        nl.
print_tree(directory(Path, File, Contents)):-
        path_components(Whole, Path, File),
        display(Whole),
        display(':'),
        nl,
        print_tree_contents(Contents).
print_tree(unknown(Path, File)):-
        path_components(Whole, Path, File),
        display('Unknown: '),
        display(Whole),
        nl.

print_tree_contents([]).
print_tree_contents([C|Cs]):-
        print_tree(C),
        print_tree_contents(Cs).

print_actions([]).
print_actions([A|As]):-
        display(A),
        nl,
        print_actions(As).

print_shell_actions([]).
print_shell_actions([A|As]):-
        print_shell_action(A),
        nl,
        print_shell_actions(As).

print_shell_action(add(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        format("cp ~w ~w~n", [F, OF]).
print_shell_action(copy(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        format("cp ~w ~w~n", [F, OF]).
print_shell_action(remove(F)):-
        !,
        format("rm ~w~n", [F]).
print_shell_action(mkdir(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        format("mkdir ~w~n", [OF]).
print_shell_action(Other):-
        arg(1, Other, File),
        format("Unknown: ~w~n", [File]).


print_diffs([]).
print_diffs([A|As]):-
        print_diff(A),
        print_diffs(As).

print_diff(add(F)):-
        !,
        format("To add: ~w ~n", [F]).
print_diff(copy(_F)).
print_diff(remove(F)):-
        !,
        format("To remove: ~w~n", [F]).
print_diff(mkdir(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        format("To make directory: ~w~n", [OF]).
print_diff(Other):-
        arg(1, Other, File),
        format("Unknown: ~w~n", [File]).


print_cvs_actions([]).
print_cvs_actions([A|As]):-
        print_cvs_action(A),
        nl,
        print_cvs_actions(As).

print_cvs_action(add(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        format("cp ~w ~w~n", [F, OF]),
        path_components(OF, Dir, File),
        format("cd ~w~n", [Dir]),
        format("cvs -q add ~w~n", [File]).
print_cvs_action(copy(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        format("cp ~w ~w~n", [F, OF]).
print_cvs_action(remove(F)):-
        !,
        path_components(F, Dir, File),
        format("cd ~w~n", [Dir]),
        format("cvs -q remove -f ~w~n", [File]).
print_cvs_action(mkdir(F)):-
        !,
        new_base(NB),
        old_base(OB),
        atom_concat(NB, Relative, F),
        atom_concat(OB, Relative, OF),
        path_components(OF, Dir, DirName),
        format("mkdir ~w~n", [OF]),
        format("cd ~w~n", [Dir]),
        format("cvs -q add ~w~n", [DirName]).
print_cvs_action(Other):-
        arg(1, Other, File),
        format("Unknown: ~w~n", [File]).
