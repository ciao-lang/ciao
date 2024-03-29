:- module(_, [], [assertions, regtypes, isomodes, hiord, regexp]).

:- doc(title, "Additional operating system utilities").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module groups some extensions to library
   @lib{system} that have been found convenient, but which are still in
   development, their interface has not been fixed, etc.").

:- doc(bug, "Much of this should probably end up eventually in
   @lib{system}, but once we have worked out the best interface and,
   in some cases, the proper implementation (the implementations in
   here are in some cases just calls to Unix shell primitives or
   commands).").

:- doc(bug, "All these predicates (including @lib{system} too) use
   atoms to represent file names. This pollutes the atom table. We
   need atom garbage collection and/or native strings.").

:- use_module(engine(stream_basic)).
:- use_module(library(system),
    [working_directory/2,
     file_exists/1,
     file_property/2,
     chmod/2, fmode/2,
     copy_file/2, copy_file/3,
     delete_file/1,
     delete_directory/1,
     make_directory/2,
     rename_file/2,
     directory_files/2,
     using_windows/0,
     copy_options/1]).
%
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [list_concat/2, append/3]).
:- use_module(library(stream_utils),
    [file_to_string/2, string_to_file/2]).

:- use_module(library(process), [process_call/3]).

% ===========================================================================

:- doc(section, "Exception handling").

:- use_module(library(port_reify), [once_port_reify/2]).

:- export(warn_on_nosuccess/1).
:- pred warn_on_nosuccess(G)
# "Call @var{G} (cutting solutions, i.e., as @pred{once/1}) and show
   warning messages if something went wrong (failure and
   exceptions).".
:- meta_predicate warn_on_nosuccess(goal).

warn_on_nosuccess(G) :-
    once_port_reify(G, Port),
    ( Port = success -> true
    ; warning_message("goal ~w did not succeed", [G])
    ).

:- export(ignore_nosuccess/1).
:- pred ignore_nosuccess(G) # "Call @var{G} and ignore if something went wrong
   (failure and exceptions).".
:- meta_predicate(ignore_nosuccess(goal)).

ignore_nosuccess(G) :- once_port_reify(G, _).

% ===========================================================================

:- doc(section, "Operations on files and directories").

% TODO: Improve this implementation
:- export(del_dir_if_empty/1).
:- pred del_dir_if_empty(D) : atm
# "Delete @var{D} if it is an empty directory.".
del_dir_if_empty(Dir) :-
    ( empty_dir(Dir) ->
        delete_directory(Dir)
    ; true
    ).

:- export(empty_dir/1).
:- pred empty_dir(D) : atm(D)
# "@var{D} is an empty directory".
empty_dir(D) :-
    \+ file_property(D, linkto(_)),
    file_exists(D),
    file_property(D, type(directory)),
    %
    directory_files(D, Fs),
    F1 = '..', F2 = '.',
    ( Fs = [F1,F2] ; Fs = [F2,F1] ),
    !.

:- export(move_files/2).
:- pred move_files(Files, Dir) : list(atm) * atm
# "Move @var{Files} to directory
    @var{Dir} (note that to move only one file to a directory,
    @pred{rename_file/2} can be used).".

%% Need to do this better of course...

move_files(Files, Dir0) :-
    path_concat(Dir0, '', Dir), % (adds trailing '/')
    move_files_(Files, Dir).

move_files_([],           _Dir).
move_files_([File|Files], Dir) :-
    move_file(File, Dir),
    move_files_(Files, Dir).

:- export(move_file/2).
:- pred move_file(File, Dir) : atm * atm
# "Move @var{File} to directory
   @var{Dir}".
move_file(File, Dir) :-
    atom_concat(Dir, File, Target),
    rename_file(File, Target).

:- export(copy_files/2).

:- pred copy_files(Files, Dir) : list(atm) * atm
# "Like @pred{copy_files/3}, with empty options list.".

%% Need to do this better of course...
copy_files(Files, Dir) :-
    copy_files(Files, Dir, []).

:- export(copy_files/3).
:- pred copy_files(Files, Dir, Opts) : list(atm) * atm * copy_options

# "Copy @var{Files} to directory
   @var{Dir}, using @var{Opts} as the option list for copy. See
   @pred{copy_file/3} for the list of options. Note that to move only
   one file to a directory, @pred{rename_file/2} can be used.".

copy_files([],           _DestDir, _CopyOptions).
copy_files([File|Files], DestDir,  CopyOptions) :-
    copy_file(File, DestDir, CopyOptions),
    copy_files(Files, DestDir, CopyOptions).

:- export(copy_files_nofail/3).
:- pred copy_files_nofail(Files, Dir, Opts) : list(atm) * atm * copy_options
# "Like @pred{copy_files/3}, but do not fail in case of errors.".

copy_files_nofail([],           _DestDir, _CopyOptions).
copy_files_nofail([File|Files], DestDir,  CopyOptions) :-
    ignore_nosuccess(copy_file(File, DestDir, CopyOptions)),
    copy_files_nofail(Files, DestDir, CopyOptions).

:- export(del_file_nofail/1).
:- pred del_file_nofail(File) : atm
# "Like @pred{delete_file/1}, but do not fail in case of errors.".
del_file_nofail(File) :-
    ignore_nosuccess(delete_file(File)).

:- export(del_files_nofail/1).
:- pred del_files_nofail(Files) : list(atm)
# "Like @pred{del_file_nofail/1}, but takes list of files in
   @var{Files}.".
del_files_nofail([]).
del_files_nofail([File|Files]) :-
    del_file_nofail(File),
    del_files_nofail(Files).

% :- export(cat/2).
% % TODO: isn't it just copy?
% cat(Sources, Target) :-
%       ( file_exists(Target) ->
%           delete_file(Target)
%       ; true
%       ),
%       cat_append(Sources, Target).

% :- export(cat_append/2).
% % TODO: isn't it something like file_append?
% cat_append(Sources, Target) :-
%       open(Target, append, O),
%       ( cat_append_stream(Sources, O) ->
%           close(O)
%       ; close(O)
%       ).

% cat_append_stream([], _O) :-
%       !.
% cat_append_stream([Source|Sources], O) :-
%       !,
%       cat_append_stream_one(Source, O),
%       cat_append_stream(Sources, O).
% cat_append_stream(Source, O) :-
%       cat_append_stream_one(Source, O).

% cat_append_stream_one(Source, O) :-
%       atom(Source),
%       Source \== [],
%       !,
%       open(Source, read, I),
%       copy_stream(I, O),
%       close(I).

% copy_stream(I, O) :-
%       get_byte(I, Code),
%       ( Code = -1 -> true
%       ; put_byte(O, Code), copy_stream(I, O)
%       ).

:- export(file_to_line/2).
:- pred file_to_line(File, Str) : list(atm) * string.

file_to_line(File, Str) :-
    file_to_string(File, Str0),
    no_tr_nl(Str0, Str).

no_tr_nl(L, NL) :-
    append(NL, [0'\n], L),
    !.
no_tr_nl(L, L).

:- export(replace_strings_in_file/3).
:- pred replace_strings_in_file(Ss, F1, F2) : list(string) * atm * atm
# "Like @pred{replace_strings/3} but from file @var{F1} to file @var{F2}.".
replace_strings_in_file(Ss, F1, F2) :-
    file_to_string(F1, F1S),
    replace_strings(Ss, F1S, F2S),
    string_to_file(F2S, F2).

:- export(backup_file/1).
:- pred backup_file(FileName) : atm
# "Save a backup copy of file @var{FileName}".
backup_file(FileName) :-
    ( file_exists(FileName) ->
        get_backup_filename(FileName, I, B),
        ( I > 0,
          I0 is I - 1,
          compose_backup_filename(FileName, I0, B0),
          file_to_string(FileName, FileNameS),
          file_to_string(B0, BS),
          BS = FileNameS ->
            true
        ; del_file_nofail(B),
          copy_file(FileName, B)
        )
    ; true
    ).

% TODO: Simplify?
get_backup_filename(FileName, I, B) :-
    get_backup_filename_(FileName, 0, I, B).

get_backup_filename_(FileName, I, I, B) :-
    compose_backup_filename(FileName, I, B),
    \+ file_exists(B),
    !.
get_backup_filename_(FileName, I0, I, B) :-
    I1 is I0 + 1,
    get_backup_filename_(FileName, I1, I, B).

compose_backup_filename(FileName, I, B) :-
    atom_number(IA, I),
    atom_concat([FileName, '.bak~', IA, '~'], B).

% TODO: merge with backup_file/1?
:- export(move_if_diff/3).
:- pred move_if_diff(From, To, NewOrOld) 
    : ( atm(From), atm(To) )
       => atm(NewOrOld)

# "If @var{To} does not exist, or its contents are different from
   @var{From}, delete @var{To} and rename @var{From} to
   @var{To}. @var{NewOrOld} is unified with @tt{new} or @tt{old}
   depending on whether the new or the old file is preserved.".

move_if_diff(From, To, NewOrOld) :-
    % note: NewOrOld is unified at the end to ensure side-effects
    ( file_exists(To), same_files(From, To) ->
        del_file_nofail(From),
        NewOrOld = old
    ; del_file_nofail(To),
      % NOTE: do not use rename_file/2 since it does not work
      %   across partitions (some Linux systems mount /tmp 
      %   in a different partition)
      copy_file(From, To),
      del_file_nofail(From),
      NewOrOld = new
    ).

same_files(A, B) :-
    file_to_string_or_empty(A, AStr),
    file_to_string_or_empty(B, BStr),
    AStr = BStr.

file_to_string_or_empty(File, Str) :-
    ( catch(file_to_string(File, Str0), _, fail) -> Str = Str0
    ; Str = "" % TODO: weird, distinguish errors?
    ).

% ===========================================================================

:- doc(section, "File attributes (owner, group, permissions)").

% TODO: missing get_file_owner
% TODO: missing get_file_perms

:- export(set_file_owner/2).
:- pred set_file_owner(+File, +Owner)
# "Set user/group of a file.".

set_file_owner(File, Owner) :-
    ( var(Owner) ->
        throw(error(uninstantiation_error(Owner), set_file_owner/2-2))
    ; ( Owner = owner(User, Group) ->
          atom_concat([User, ':', Group], UserGrp)
      ; atom(Owner) ->
          UserGrp = Owner
      ; % TODO: fix error?
        throw(error(domain_error(owner, Owner), set_file_owner/2-2))
      ),
      process_call(path(chown), [UserGrp, File], [])
    ).

:- export(set_file_perms/2).
:- pred set_file_perms(File, Perms) : ( perms_term(Perms),atm(File) )
# "Set permissions of @var{File} to @var{Perms}.".

% (File can be a path)
set_file_perms(File, Perms) :-
    execmask(File, ExecMask),
    perms_to_mode(ExecMask, Perms, Mode),
    chmod_if_needed(File, Mode).

:- export(perms_term/1).
:- regtype perms_term(Perms)

# "@var{Perms} is a term providing modes for User, Group, and
   Others.".

:- doc(perms_term(Perms), "@var{Perms} is a term providing valid
   permissions (``modes'') for User, Group, and Others. These are all
   @pred{valid_mode/1}s.  Defined as follows:
   @includedef{perms_term/1}").

perms_term(perms(U, G, O)) :- 
    valid_mode(U),
    valid_mode(G),
    valid_mode(O).

:- export(valid_mode/1).
:- regtype valid_mode(Mode)

# "@var{Mode} is a file permissions mode.".

:- doc(valid_mode(Mode), "@var{Mode} is an atom that provides a valid
   set of file permissions (a valid ``mode'').  Defined as follows:
   @includedef{valid_mode/1}").

valid_mode( '' ).
valid_mode( 'X').
valid_mode(  x ).
valid_mode( w  ).
valid_mode( wX ).
valid_mode( wx ).
valid_mode( r  ).
valid_mode( rX ).
valid_mode( rx ).
valid_mode(rw  ).
valid_mode(rwX ).
valid_mode(rwx ).


% If File is a regular file, get a mask with current execution bits
% for user,group,other. If File is a directory, get a mask with all
% execution bits turned on.
execmask(File, ExecMask) :-
    ( file_property(File, type(directory)) ->
        ExecMask = 0o111
    ; file_property(File, mode(OrigMode)),
      ExecMask is OrigMode /\ 0o111
    ).

% TODO: is set_exec_perms/2 really needed?

:- export(set_exec_perms/2).
:- pred set_exec_perms(File, Perms) # "Set file permissions, but treat
   regular files as directories w.r.t. 'X' flag".
% (File can be a path)
set_exec_perms(File, Perms) :-
    perms_to_mode(0o111, Perms, Mode),
    chmod_if_needed(File, Mode).

% TODO: really needed?
chmod_if_needed(File, Mode) :-
    file_property(File, mode(OrigMode)),
    ( Mode == OrigMode -> % same mode, do nothing
        true
    ; chmod(File, Mode)
    ).

:- export(mkpath/1).
:- pred mkpath(Path): sourcename
   # "Creates the directories necessary to access the given @var{Path}
     (which can be absolute or relative).".
mkpath(Dir) :-
    mkpath_mode(Dir, 0o777).

:- export(mkpath/2).
:- pred mkpath(Path, Perms): sourcename * term
   # "Like @pred{mkpath}, but sets permissions of new directories as
      @var{Perms}.".

mkpath(Path, Perms) :-
    perms_to_mode(0o111, Perms, Mode),
    mkpath_mode(Path, Mode).

:- export(mkpath/3).
:- pred mkpath(Path, Perms, Owner): sourcename * term * term
   # "Like @pred{mkpath}, but sets permissions and owner of new
     directories to @var{Perms} and @var{Owner}.".

mkpath(Path, Perms, Owner) :-
    perms_to_mode(0o111, Perms, Mode),
    mkpath_mode(Path, Mode, Owner).

% TODO: reimplement (use library(pathnames))
:- export(mkpath_mode/3).
% We should take care of correct instantiation modes, types, etc. here.
% We are however delegating it to make_directory/2 and absolute_file_name/7
% (called below).
mkpath_mode(Path, Mode, Owner) :-
%       absolute_file_name(Path, '', '', '.', AbsolutePath, _, _),
%       atom_codes(AbsolutePath, AbsPathCodes),
    atom_codes(Path, PathCodes),
    ( % If relative, transform it into absolute
      ( PathCodes = "/"||_ ; drive_selector(PathCodes-_, _) ) ->
        AbsPathCodes = PathCodes
    ; working_directory(CurrentDir, CurrentDir),
      atom_codes(CurrentDir, CurrentDirCodes),
      append(CurrentDirCodes, "/"||PathCodes, AbsPathCodes)
    ),
    make_abs_dir0(AbsPathCodes, '', Mode, Owner).

% Making the intermediate directories: instead of cd'ing to
% directories (which is something which can break and modify the
% program state), we construct incrementally the intermediate
% directories.  The idea here is to traverse the absolute path and
% construct partial paths, which are appended to an atom which
% represents the (initially empty) path.  Depending on the
% implementation of atom_concat/3 (and of the atoms), this can be done
% quite fast if the first argument does not need to be completely
% traversed.

% End of path
make_abs_dir0("", _, _Mode, _Owner):- !.
% The recursive case: perform a step in the recursion and construct the
% intermediate directory.
make_abs_dir0(Path, IncPath, Mode, Owner):-
    decompose0(Path, RestPath, Component-[]),
    make_abs_dir2(RestPath, Component, IncPath, Mode, Owner).

make_abs_dir2(RestPath, Component, IncPath, Mode, Owner):-
    % Transform into atom and add to partial path
    atom_codes(PartComp, Component),
    atom_concat(IncPath, PartComp, NewPath),
    ( file_exists(NewPath) ->
        true
    ; make_directory(NewPath, Mode),
      ( var(Owner) ->
          true
      ; set_file_owner(NewPath, Owner)
      )
    ),
    make_abs_dir(RestPath, NewPath, Mode, Owner).

make_abs_dir("", _, _Mode, _Owner):- !.
% The recursive case: perform a step in the recursion and construct the
% intermediate directory.
make_abs_dir(Path, IncPath, Mode, Owner):-
    decompose(Path, RestPath, Component-[]),
    make_abs_dir2(RestPath, Component, IncPath, Mode, Owner).

% decompose("//"||PathWOSlash, RestPath, Queue):- !, 
%         decompose("/"||PathWOSlash, RestPath, Queue).
decompose0(PathWOSlash, RestPath, Queue-TailQ):-
    using_windows, drive_selector(PathWOSlash-PathWOSlash0, Queue-Queue0), 
    !,
    decompose_aux(PathWOSlash0, RestPath, Queue0-TailQ).
decompose0("/"||PathWOSlash, RestPath, "/"||Queue-TailQ):-
    decompose_aux(PathWOSlash, RestPath, Queue-TailQ).

decompose("/"||PathWOSlash, RestPath, "/"||Queue-TailQ):-
    decompose_aux(PathWOSlash, RestPath, Queue-TailQ).

decompose_aux("", "", Q-Q).
decompose_aux("/"||P, "/"||P, Q-Q):- !.
decompose_aux([P|Ps], RestP, [P|RestQ]-TailQ):- 
    decompose_aux(Ps, RestP, RestQ-TailQ).

% A windows drive letter
drive_selector([D, 0':, 0'/|Path]-Path, [D, 0':, 0'/|Ds]-Ds) :-
    ( D >= 0'a, D =< 0'z -> true
    ; D >= 0'A, D =< 0'Z -> true
    ).

%% mkpath_mode(Path, Mode) :-
%%      working_directory(CurrentDir, CurrentDir),
%%      mkpath_mode_aux(Path, Mode),
%%      working_directory(_, CurrentDir).
%% 
%% mkpath_mode_aux(Path, Mode) :-
%%      atom_concat(Head, Tail, Path),
%%      atom_concat('/', SubTail, Tail), !,
%%      (Head = '' ->
%%       working_directory(CurrentDir, '/')
%%      ;
%%       ((file_exists(Head), file_property(Head, type(directory)))
%%        ;
%%         make_directory(Head, Mode) 
%%       ),
%%       working_directory(CurrentDir, Head)),
%%      mkpath_mode_aux(SubTail, Mode).
%% mkpath_mode_aux(Dir, Mode) :-
%%      make_directory(Dir, Mode).
%%       

:- export(mkpath_mode/2).
:- pred mkpath_mode(+sourcename,+int)
   # "Equivalent to @tt{mkpath_mode(Path,Mode,_)}.".
      
mkpath_mode(Path, Mode) :-
    mkpath_mode(Path, Mode, _).

:- export(mkpath_mode/1).
:- pred mkpath_mode(+sourcename)
   # "Equivalent to @tt{mkpath_mode(Path,0o777,_)}.".
      
mkpath_mode(Path) :-
    mkpath_mode(Path, 0o777, _).

% ---------------------------------------------------------------------------

% (Note: ExecMask is applied to 'X' execution permissions)
perms_to_mode(ExecMask, Perms, Mode) :-
    ( var(Perms) ->
        throw(error(uninstantiation_error(Perms), perms_to_mode/3-2))
    ; execute_permissions(Perms, Exec),
      convert_permissions(Perms, Perms2) ->
        Mode is Perms2 \/ (Exec /\ ExecMask)
    ; % TODO: fix error
      throw(invalid_permissions(Perms))
    ).

execute_permissions(perms(U, G, O), E) :-
    exec_mask_perms(U, NU),
    exec_mask_perms(G, NG),
    exec_mask_perms(O, NO),
    E is NU << 6 + NG << 3 + NO.

convert_permissions(perms(U, G, O), P) :-
    mode_symb_bin(U, NU),
    mode_symb_bin(G, NG),
    mode_symb_bin(O, NO),
    P is NU << 6 + NG << 3 + NO.

% Meaning of uppercase X in permissions:
%  - if the file is a directory, set executable attribute
%  - otherwise, do not modify the executable attribute

exec_mask_perms( '' , 0).
exec_mask_perms(  x , 0).
exec_mask_perms( 'X', 1).
exec_mask_perms( w  , 0).
exec_mask_perms( wx , 0).
exec_mask_perms( wX , 1).
exec_mask_perms( r  , 0).
exec_mask_perms( rx , 0).
exec_mask_perms( rX , 1).
exec_mask_perms(rw  , 0).
exec_mask_perms(rwx , 0).
exec_mask_perms(rwX , 1).

mode_symb_bin( '' , 2'000).
mode_symb_bin( 'X', 2'000).
mode_symb_bin(  x , 2'001).
mode_symb_bin( w  , 2'010).
mode_symb_bin( wX , 2'010).
mode_symb_bin( wx , 2'011).
mode_symb_bin( r  , 2'100).
mode_symb_bin( rX , 2'100).
mode_symb_bin( rx , 2'101).
mode_symb_bin(rw  , 2'110).
mode_symb_bin(rwX , 2'110).
mode_symb_bin(rwx , 2'111).

% ---------------------------------------------------------------------------
% Create and remove a temporary directory

:- use_module(library(pathnames), [path_split/3, path_concat/3]).
%:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(system),
    [mktemp_in_tmp/2, delete_file/1, touch/1, file_exists/1]).
:- use_module(library(source_tree), [remove_dir/1]).

:- export(mktempdir_in_tmp/2).
:- pred mktempdir_in_tmp(Template, Path)
   # "Create a directory in the temporary directory using
      @var{Template} (see @pred{mktemp_in_tmp/2}). An empty file
      @tt{CREATED_WITH_MKTEMPDIR} is created inside @var{Path} as a
      safety check for @pred{rmtempdir/1}.".

mktempdir_in_tmp(Template, Path) :-
    % Create a temporary file and use the name for the dir
    atom_concat('f_', Template, TemplateF),
    mktemp_in_tmp(TemplateF, TmpF),
    path_split(TmpF, TmpFB, TmpFN),
    atom_concat('f_', TmpFN2, TmpFN),
    path_concat(TmpFB, TmpFN2, Path),
    mkpath(Path),
    delete_file(TmpF),
    tempdir_mark(Path, Mark),
    touch(Mark).

:- export(rmtempdir/1).
:- pred rmtempdir(Path)
   # "Remove the temporary directory @var{Path} (recursively) created
      with @pred{mktempdir_in_tmp/2}. As a safety check, this
      predicate throws an exception if the @tt{CREATED_WITH_MKTEMPDIR}
      file is not in @var{Path}.".

rmtempdir(Path) :-
    tempdir_mark(Path, Mark),
    file_exists(Mark),
    !,
    remove_dir(Path).
rmtempdir(Path) :-
    throw(error(not_created_by_mktempdir_in_tmp(Path), rmtempdir/1)).

tempdir_mark(Path, Mark) :-
    path_concat(Path, 'CREATED_WITH_MKTEMPDIR', Mark).

% ---------------------------------------------------------------------------
% Symlinks, relative symlinks, and relative paths

% :- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(system), [copy_file/3]).
:- use_module(library(system), [using_windows/0]).
:- use_module(library(pathnames), [path_dirname/2]).

:- export(create_rel_link/2).
:- pred create_rel_link(From, To) # "Create a @em{relocatable} symlink
   (computing relative paths) (e.g., @tt{/a/b/c (symlink) -> /a/d/e}
   becomes @tt{/a/b/c (symlink) -> ../d/e}".

create_rel_link(From, To) :-
    path_dirname(To, ToDir),
    relpath(ToDir, From, RelFrom),
    create_link(RelFrom, To).

:- export(create_link/2).
:- pred create_link(From, To) # "Create a symlink from @var{From} to
   @var{To}. On platforms where symlinks are not supported (Windows)
   the file is copied instead. The file @var{To} is removed if it
   existed before.".

create_link(From, To) :-
    del_file_nofail(To),
    ( using_windows ->
        copy_file(From, To, [overwrite]) % TODO: better solution? windows lacks proper symlinks
    ; copy_file(From, To, [overwrite, symlink])
    ).

:- use_module(library(pathnames), [path_split_list/2, path_concat_list/2]).

:- export(relpath/3).
:- pred relpath(A, B, C) # "@var{C} is a path to @var{B} relative to
   @var{A} (using @tt{..} if needed) (e.g., @tt{/a/b/c -> /a/d/e}
   becomes @tt{/a/b/c -> ../../d/e}.  Assume both are absolute,
   otherwise just return @var{B}.".

relpath(A, B, C) :-
    path_split_list(A, As),
    path_split_list(B, Bs),
    As = ['/'|As0],
    Bs = ['/'|Bs0],
    !,
    relpath_(As0, Bs0, Cs),
    path_concat_list(Cs, C).
relpath(_, B, B).

% Consume common part
relpath_([A|As], [A|Bs], Cs) :- !, relpath_(As, Bs, Cs).
relpath_(As, Bs, Cs) :- relpath__(As, Bs, Cs).

% Add '..' for each A component (go back)
relpath__([_|As], Bs, ['..'|Cs]) :- !, relpath__(As, Bs, Cs).
relpath__([], Bs, Bs). % Finish with rest

% =========================================================================
% TODO: preds from the SICStus lib that probably need to be implemented 
% =========================================================================

%% Needs to be added 
%% :- doc(delete_file(FileName,Options), "@var{FileName} is the
%%    name of an existing file or directory.  @var{Options} is a list of
%%    options. Possible options are @tt{directory}, @tt{recursive} or
%%    @tt{ignore}.
%%    If @var{FileName} is not a directory it is deleted, otherwise if
%%    the option @tt{directory} is specified but not @tt{recursive}, the
%%    directory will be deleted if it is empty. If @tt{recursive} is
%%    specified and @var{FileName} is a directory, the directory and all
%%    its subdirectories and files will be deleted.  If the operation
%%    fails, an exception is raised unless the @tt{ignore} option is
%%    specified.").
%% 
%% :- pred delete_file(+atm,+list(delete_file_option)).
%% 
%% delete_file(FileName,[recursive]) :- delete_file(FileName,[recursive])
%% 
%% 
%% 
%% :- doc(delete_file(FileName), "Equivalent to
%%    @tt{delete_file(FileName,[recursive])}.").
%% 
%% :- pred delete_file(+atm).
%% 
%% delete_file(FileName) :- delete_file(FileName,[recursive])
%% 
%% :- regtype delete_file_option(X) # "@var{X} is an option controlling
%%    file deletion".
%% 
%% delete_file_option(directory).
%% delete_file_option(recursive).
%% delete_file_option(ignore).

%% `tmpnam(-FILENAME)'
%%      Interface to the ANSI C function tmpnam(3).  A unique file name is
%%      created and unified with FILENAME.

%% Note name and type change, and it enumerates.
%% `environ(?VAR, ?VALUE)'
%%      VAR is the name of an environment variable, and VALUE is its
%%      value.  Both are atoms.  Can be used to enumerate all current
%%      environment variables.

%% Note atom-based options:
%% `file_exists(+FILENAME, +PERMISSIONS)'
%%      FILENAME is the name of an existing file or directory which can be
%%      accessed according to PERMISSIONS.  PERMISSIONS is an atom, an
%%      integer (see access(2)), or a list of atoms and/or integers.  The
%%      atoms must be drawn from the list `[read,write,search,exists]'.
%% 

%% These, somewhat incompatible
%% `host_id(-HID)'
%%      HID is the unique identifier, represented by an atom, of the host
%%      executing the current SICStus Prolog process.
%% 
%% `host_name(-HOSTNAME)'
%%      HOSTNAME is the standard host name of the host executing the
%%      current SICStus Prolog process.

% ===========================================================================

:- doc(section, "Terminal Tools").

% TODO: write in C
:- export(istty/1).
:- pred istty(FD) # "Check if the file descriptor is associated with a terminal.".
istty(FD) :-
    number_codes(FD, Cs),
    atom_codes(FDa, Cs),
    process_call(path(test), ['-t', FDa], [status(0)]).

% ===========================================================================

:- doc(section, "Date Formatting").

:- use_module(library(system), [datime/9]).

:- export(datime_atom/1).
datime_atom(T) :-
    datime_string(S),
    atom_codes(T, S).

:- export(datime_atom/2).
datime_atom(D, T) :-
    datime_string(D, S),
    atom_codes(T, S).

:- export(datime_string/1).
% datime, as a string
datime_string(S) :- datime_string(_, S).

:- export(datime_string/2).
% datime, as a string
datime_string(T, S) :-
    datime(T, Year, Month, Day, Hour, Min, Sec, _WeekDay, _YearDay),
    datime_to_string(datime(Year, Month, Day, Hour, Min, Sec), S).

:- export(datime_to_string/2).
% From datime to string representation
datime_to_string(datime(Year, Month, Day, Hour, Min, Sec), S) :-
    number_codes(Day,  DayS), number_codes(Month, MonthS),
    number_codes(Year, YearS), number_codes(Hour, HourS),
    number_codes(Min,  MinS), number_codes(Sec, SecS),
    list_concat([DayS, "/", MonthS, "/", YearS, " ", HourS, ":",
            MinS, ":", SecS], S).

%% =========================================================================

:- doc(section, "String manipulation").

:- export(replace_strings/3).
% TODO: This implementation does several passes over the string,
%       see replace_params/3.
% TODO: Move to some string related module?
replace_strings([], O, O).
replace_strings([[S1, S2]|Ss], I, O) :-
    replace_string(I, S1, S2, TO),
    replace_strings(Ss, TO, O).

replace_string(_I, S1, _S2, _TO) :-
    atom(S1),
    !,
    throw(error(domain_error(string, atom), replace_string/4 -2)).
replace_string(I, S1, "", TO) :-
    !,
    do_replace_string(I, S1, "", TO).
replace_string(_I, _S1, S2, _TO) :-
    atom(S2),
    !,
    throw(error(domain_error(string, atom), replace_string/4 -3)).
replace_string(I, S1, S2, TO) :-
    do_replace_string(I, S1, S2, TO).

do_replace_string([], _S1, _S2, []) :- !.
do_replace_string(I,  S1,  S2,  O) :-
    match(S1, I, RI),
    !,
    append(S2, NO, O),
    do_replace_string(RI, S1, S2, NO).
do_replace_string([H|RI], S1, S2, [H|RO]) :-
    do_replace_string(RI, S1, S2, RO).

match([],    I,      I).
match([H|T], [H|IT], RI) :-
    match(T, IT, RI).

