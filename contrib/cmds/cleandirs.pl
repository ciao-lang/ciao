:- module(cleandirs, [main/1], []).

:- use_package(assertions).


:- doc(title,"A Program to Help Cleaning your Directories ").

:- doc(author,"Manuel Carro").

:- doc(module,"A simple program for traversing a directory tree
and deciding which files may be deleted in order to save space and
not to loose information. 

   @section{Usage (cleandirs)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}


 Invoking the program with no arguments will return an up-to-date
information on the options.").

:- doc(bug, "Recursive removal of subdirectories relies on the
existence of a recursive /bin/rm command in your system.").

:- use_module(library(iso_char), [
        get_char/1
                                      ]).
:- use_module(library(aggregates), [
        setof/3, (^)/2
                                   ]).
:- use_module(library(format), [
        format/2
                               ]).
:- use_module(library(system), [
        modif_time/2,
        datime/9,
        working_directory/2,
        system/1,
        delete_file/1,
        cd/1,
        directory_files/2,
        file_exists/1,
        file_property/2
                               ]).
:- data silent/0.

main(Arg):- 
        (
            Arg = [Directory, Action, Backups]
        ;
            Arg = ['--silent', Directory, Action, Backups],
            asserta_fact(silent)
        ),
        !,
        (
            check_right_use(Directory, Action, Backups)
        ->
        directory_files(Directory, Files),
        cd(Directory),
        catch(recurse_all_entries(Files, action(Action, Backups)), 
              Ball, exit(Ball))
        ;
            usage_message
        ).


main(_):-
        format("Wrong number of arguments~n", []),
        usage_message.

exit(exit):- !,
        format("~nExiting application after user request~n",[]).
exit(Ball):- !,
        format("~nExiting application after exception:~n~w",[Ball]).


check_right_use(Directory, Action, Backups):-
        second_arg(Second),
        third_arg(Third),
        (
            file_property(Directory, type(directory)) ->
            true
        ;
            format(
          "first argument (~w) not valid: must be a directory~n", [Directory]),
          Fail = yes
        ),
        (
            member(Action, Second) ->
             true
        ;
            format(
          "Second argument not valid: must be one of ~w~n", [Second]),
          Fail = yes
        ),
        (
            member(Backups, Third) ->
             true
        ; 
            format(
          "Third argument not valid: must be one of ~w~n", [Third]),
          Fail = yes
        ),
        Fail = no.


second_arg(['--list','--ask','--delete']).
third_arg(['--includebackups','--excludebackups','--onlybackups']).

usage_text("
cleandirs [--silent] <initial_dir> <delete_options> <backup_options>
cleandirs explores <initial_dir> (which should be an absolute path)
and looks for backup files and files which can be generated from other
files, using a plausible heuristic aimed at retaining the same amount
of information while recovering some disk space.  The heuristic is
based on the extension of the filename.

Delete options is one of:
   --list: just list the files/directories which are amenable to be deleted,
           but do not delete them. SAFE.
    --ask: list the files/directories and ask for deletion. UNSAFE if you
           make a mistake.
 --delete: just delete the files/directories without asking.  I envy your
           brave soul if you choose this option.

Backup options is one of:
 --includebackups: include backup files in the list of files to check.
 --excludebackups: do not include backup files in the list of files
                   to check.
    --onlybackups: include only backup files in the list of files to check.

Symbolic links are not traversed.  Special files are not checked.

").

usage_message:-
        usage_text(Usage),
        format(Usage, []),
        say_backups,
        say_generators.
        
say_backups:-
        format("The extensions considered to be backups are:~n", []),
        oldstuff(Suff),
        format("   ~w~n", [Suff]),
        fail.
say_backups.

say_generators:-
        format("
The following extensions are taken into account when looking for files
which can be generated from other files:~n", []),
        setof(X, 
              Y^((generates(X, Y) ; generates(Y, X)), X \== ''), 
              ListOfSuff),
        member(Suf, ListOfSuff),
        format("   ~w~n", [Suf]),
        fail.
say_generators:-
        format("
Combinations of these extensions (e.g., .tar.gz) are also sought for~n", []).


recurse_all_entries([], _Action).
recurse_all_entries([File|Files], Action):-
        check_file_property(File, regular), !,
        check_what_to_do(File, Action, _Erased),
        recurse_all_entries(Files, Action).
recurse_all_entries([Directory|Files],Action):-
        check_file_property(Directory, directory), !,
        ( 
            Directory \== '..', Directory \== '.' -> 
            check_what_to_do(Directory, Action, Erased),
            ( 
                Erased = no -> 
                working_directory(Old, Old),
                ( 
                    file_property(Directory, linkto(_)) ->
                    (
                        silent -> 
                        true
                    ;
                        format(
                        "*** Skipping directory '~w/~w' (symbolic link)~n",
                               [Old,Directory])
                    )
                ; 
                    ( 
                        directory_files(Directory, DirFiles),
                        cd(Directory) -> 
%                        display(in(Directory)), nl,
                        recurse_all_entries(DirFiles, Action),
                        cd(Old)
                    ; 
                        (
                            silent ->
                            true
                        ;
                            format(
                 "*** Skipping directory '~w/~w' (insuficient permissions)~n",
		           [Old,Directory])
                        )
                    )
                )
	    ; 
                true
            )
        ; 
            true
        ),
	recurse_all_entries(Files, Action).
recurse_all_entries([_File|Files], Action):-
        recurse_all_entries(Files, Action).

check_file_property(File, Prop):-
        file_exists(File) ->
        file_property(File, type(Prop))
 ;
        ignore_file(Prop, File).

ignore_file(regular, File):-
	working_directory(Old, Old),
        (
            silent ->
            true
        ;
            format("*** Could not access: '~w/~w'~n",[Old,File])
        ),
	fail.

%% Files with a generator
check_what_to_do(File, action(AskRemList, CheckBackups), Erased):-
        CheckBackups \== '--onlybackups',
        can_generate_file(GeneratorFile, File),
        file_exists(GeneratorFile), !,
        decide_action_generator(File, GeneratorFile, AskRemList, Erased).
%% Backup files, and we want to check them
check_what_to_do(File, action(AskRemList, CheckBackups), Erased):-
        CheckBackups \== '--excludebackups',
        oldstuff(Suffix),
        atom_concat(_Basename, Suffix, File), !,
        decide_action_backup(File, AskRemList, Erased).
%% None of the above
check_what_to_do(_, _, no).

decide_action_generator(File, GeneratorFile, AskRemList, Erased):-
        last_modif_time(File, TFile, TimeFile),
        last_modif_time(GeneratorFile, TGen, TimeGenerator),
        working_directory(Dir, Dir),
        format("~w: ~n", [Dir]),
        format("  ~w (~w)~n", [GeneratorFile, TimeGenerator]),
        format("  ~w (~w)~n", [File, TimeFile]),
        (
            TGen > TFile ->
            (
                silent ->
                true
            ;
                format("(~w has been modified or created after ~w)~n",
                       [GeneratorFile, File])
            ),
            erase_or_not(AskRemList, File, Erased)
        ;
            (
                silent ->
                true
            ;
                format("(~w has been modified or created after ~w)~n",
                         [File, GeneratorFile])
            ),
            erase_or_not(AskRemList, File, Erased)
        ).


decide_action_backup(File, AskRemList, Erased):-
        working_directory(Dir, Dir),
        format("File ~w found in ~w~n", [File, Dir]),
        erase_or_not(AskRemList, File, Erased).


%% In case of forced deletion: just 

erase_or_not('--list', _File, no).
erase_or_not('--ask',  File, Perhaps):-
        Answers = [y,n,q],
        format("Do you want to erase ~w ~w?~n", [File, Answers]),
        prompt(Old, '--> '),
        get_answer(Answer, Answers),
        prompt(_New, Old),
        (
            Answer = q ->
            throw(exit)
        ;
            (
                Answer = y ->
                erase_it(File),
                Perhaps = yes
            ;
                Perhaps = no
            )
        ).
erase_or_not('--delete', File, yes):- erase_it(File).


erase_it(File):-
        (
            file_property(File, type(directory)) ->
            atom_concat('/bin/rm -rf ', File, Command),
            system(Command)
        ;
            (
                delete_file(File) ->
                true
            ;
                format("*** Could not delete ~w (insuficient permissions?)~n", 
                [File])
            )
        ).

get_answer(X, ListOfValidAnswers):-
        get_char(Y),
        get_char(_),  %%% return
        (
            member(Y, ListOfValidAnswers)->
            X = Y
        ;
            format("Please answer one of ~w~n", [ListOfValidAnswers]),
            get_answer(X, ListOfValidAnswers)
        ).

last_modif_time(File, Time, Year/Month/Day-Hour*Min*Sec):-
        modif_time(File, Time),
        datime(Time, Year, Month, Day, Hour, Min, Sec, _, _).

 %% These are ad-hoc suffixes which usually mean we are leaving a trail
 %% of old garbage behind us.  Sometimes we do not want to remove
 %% backups (specially, emacs backups); that is the reason why I have
 %% a special option to avoid ever telling the user about them.

oldstuff('.aux').
oldstuff('.toc').
oldstuff('.log').
oldstuff('.old').
oldstuff('.OLD').
oldstuff('.bak').
oldstuff('.BAK').
oldstuff('-old').
oldstuff('-OLD').
oldstuff('-bak').
oldstuff('-BAK').
oldstuff('~').
oldstuff('#').


 %% generate(S1, S2): I can use <file>S1 to generate <file>S2, and
 %% either <file>S1 is smaller (and thus it is preferred), or <file>S1
 %% cannot be generated from <file>S2.
 %% Sometimes <file>S1 and <file>S2 can be generated from each other,
 %% and the sizes are comparable.  I have both options, in that case.

generates('.c', '.o').
generates('.adb', '.o').
generates('.ads', '.ali').
generates('.pl', '.po').
generates('', '.tar').
generates('.tar', '').
generates('.ltx', '.dvi').
generates('.tex', '.dvi').
generates('.tex', '.pdf').
generates('.ps', '.pdf').
generates('.tex', '.html').
generates('.dvi', '.ps').
generates('.tgz', '.tar').
generates('.zip', '').
generates('.gz', '').


 %% And I can chain the steps above: use a .tex to generate a .dvi,
 %% then compress the .tex into a .tex.gz , for example.  I am just
 %% chaining two levels: probably this is enough.



generate_file(FileX, FileY):-
        generates(X, Y),
        atom_concat(Base, Y, FileY),
        atom_concat(Base, X, FileX).

:- true pred can_generate_file(Generator, Generated) : var * atm => atm * atm.

can_generate_file(X, Y):-
        can_generate(X, Y),
        X \== Y.

can_generate(FileX, FileY):-
        generate_file(FileX, FileY).
can_generate(FileX, FileZ):-
        generate_file(FileY, FileZ),
        generate_file(FileX, FileY).
