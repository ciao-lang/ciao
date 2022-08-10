:- module(system, [], [noprelude, assertions, nortchecks, isomodes, regtypes]).

:- doc(title, "Operating system utilities").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").

:- doc(module, "This module contains predicates for invoking
   services which are typically provided by the operating system.
   Note that the predicates which take names of files or directories
   as arguments in this module expect atoms, not @concept{path
   alias}es. I.e., generally these predicates will not call
   @pred{absolute_file_name/2} on names of files or directories taken
   as arguments.").

:- doc(bug, "We use atoms to represent file names. This pollutes the
   atom table. We need atom garbage collection and/or native
   strings.").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_compare)).
:- use_module(engine(term_typing)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(exceptions)).
:- use_module(engine(arithmetic)).
%
:- use_module(engine(stream_basic)).
:- use_module(engine(runtime_control), [prolog_flag/3, set_prolog_flag/2]).
:- use_module(engine(system_info), [get_exec_ext/1]).
:- if(defined(optim_comp)).
:- else.
:- use_module(library(lists), [member/2]).
:- endif.
:- use_module(library(pathnames), [path_concat/3, path_split/3]).

:- if(defined(optim_comp)).
:- '$native_include_c_source'(engine(system)). % TODO: rename to eng_system?
:- endif.

% ---------------------------------------------------------------------------
:- export(now/1).
:- doc(now(Time), "@var{Time} is unified with the number of seconds
     elapsed since January, 1, 1970 (UTC).").
:- trust pred now(?int).
:- if(defined(optim_comp)).
:- '$props'(now/1, [impnat=cbool(prolog_now)]).
:- else.
:- impl_defined(now/1).
:- endif.

% ---------------------------------------------------------------------------
%% :- doc(walltime(Time),"@var{Time} is unified with the time in
%%      milliseconds elapsed in the real world since the last call to
%%      @pred{walltime/1}. The first call returns a meaningless number.").
%% 
%% :- pred walltime(?int).

% ---------------------------------------------------------------------------
:- export(datime/1).
:- doc(datime(Datime), "@var{Datime} is unified with a term of the
     form @tt{datime(Year,Month,Day,Hour,Minute,Second)} which contains
     the current date and time.").
:- pred datime(?datime_struct).

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
    datime(_, Year, Month, Day, Hour, Min, Sec, _, _).

% ---------------------------------------------------------------------------
:- export(datime_struct/1).
:- prop datime_struct/1 + regtype.

datime_struct(datime(Year,Month,Day,Hour,Min,Sec)) :-
    int(Year), int(Month), int(Day), int(Hour), int(Min), int(Sec).

% ---------------------------------------------------------------------------
:- export(datime/9).
:- doc(datime(Time,Year,Month,Day,Hour,Min,Sec,WeekDay,YearDay),
    "@var{Time} is as in @pred{now/1}. @var{WeekDay} is the number
    of days since Sunday, in the range 0 to 6.  @var{YearDay} is the
    number of days since January 1, in the range 0 to 365.").

:- trust pred datime(+int,?int,?int,?int,?int,?int,?int,?int,?int)
    # "If @var{Time} is given, the rest of the arguments are unified
    with the date and time to which the @var{Time} argument refers.".

:- trust pred datime(?int,+int,+int,+int,+int,+int,+int,?int,?int) #
    "Bound @var{Time}, @var{WeekDay} and @var{YearDay} as
    determined by the input arguments.".

:- trust pred datime(-int,-int,-int,-int,-int,-int,-int,?int,?int)
    # "Bound @var{Time} to current time and the rest of the
    arguments refer to current time.".

:- if(defined(optim_comp)).
:- '$props'(datime/9, [impnat=cbool(prolog_datime)]).
:- else.
:- impl_defined(datime/9).
:- endif.

% ---------------------------------------------------------------------------

%:- trust pred errno(?atm).
%:- trust pred strerrno(?atm).

:- export(copy_option/1).
:- regtype copy_option(Opt)
# "@var{Opt} is a file copy option. @includedef{copy_option/1}".

copy_option(overwrite).  % overwrite
copy_option(timestamp).  % preserve time stamp
copy_option(symlink).    % create a symbolic link (in windows a shorcut)
copy_option(append).     % If the target file exists, append the source to it

:- export(copy_options/1).
:- regtype copy_options(Opts) # "@var{Opts} is a list of file copy options.".

copy_options(X) :- list(copy_option, X).

copy_option_flag(overwrite, 1).
copy_option_flag(timestamp, 2).
copy_option_flag(symlink,   4).
copy_option_flag(append,    8).

copy_options_flag(Options, Flag) :-
    copy_options_flag_(Options, 0, Flag).

copy_option_flag_(Option, F0, F) :-
    copy_option_flag(Option, F1),
    F is F0 \/ F1.

copy_options_flag_([], F, F).
copy_options_flag_([Option|Options], F0, F) :-
    copy_option_flag_(Option, F0, F1),
    copy_options_flag_(Options, F1, F).

% ---------------------------------------------------------------------------
:- export(copy_file/2).
:- doc(copy_file(Source,Destination), "Copies the file @var{Source} to
    @var{Destination}.").
:- pred copy_file(+atm, +atm).

copy_file(Source, Target) :-
    copy_file(Source, Target, []).

% ---------------------------------------------------------------------------
:- export(copy_file/3).
:- pred copy_file(+atm, +atm, +copy_options).

copy_file( Source, _Target, _CopyOptions) :-
    ( \+atom(Source) ),
    !,
    throw(error(domain_error(atom,Source),copy_file/3-1)).
copy_file(_Source,  Target, _CopyOptions) :-
    ( \+atom(Target) ),
    !,
    throw(error(domain_error(atom,Target),copy_file/3-2)).
copy_file(_Source, _Target,  CopyOptions) :-
    \+ copy_options(CopyOptions),
    !,
    throw(error(domain_error(copy_options,CopyOptions),copy_file/3-3)).
copy_file(Source, Target, CopyOptions) :-
    copy_options_flag(CopyOptions, CopyFlag),
    ( file_exists(Target),
      \+ file_property(Target, linkto(_)),
      file_property(Target, type(directory)) ->
        path_split(Source, _Dir, Name),
        path_concat(Target, Name, T1),
        c_copy_file(Source, T1, CopyFlag)
    ; c_copy_file(Source, Target, CopyFlag)
    ).

:- trust pred c_copy_file(+atm,+atm,+int).
:- if(defined(optim_comp)).
:- '$props'(c_copy_file/3, [impnat=cbool(prolog_c_copy_file)]).
:- else.
:- impl_defined(c_copy_file/3).
:- endif.

% ---------------------------------------------------------------------------
:- export(getenvstr/2).
:- doc(getenvstr(Name, Value), "The environment variable @var{Name}
    has @var{Value}.  Fails if variable @var{Name} is not defined.").
:- pred getenvstr(+atm, ?string).

getenvstr(Name, _Value) :- \+ atom(Name), !,
    throw(error(domain_error(atom,Name),getenvstr/2-1)).
getenvstr(Name, Value) :-
    c_get_env(Name, Value2),
    atom_codes(Value2, Value).

:- trust pred c_get_env(+atm,?atm).
:- if(defined(optim_comp)).
:- '$props'(c_get_env/2, [impnat=cbool(prolog_c_get_env)]).
:- else.
:- impl_defined(c_get_env/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(setenvstr/2).
:- doc(setenvstr(Name, Value), "The environment variable @var{Name}
    is assigned @var{Value}.").
:- pred setenvstr(+atm, +string).

setenvstr(Name, _Value) :- \+ atom(Name), !,
    throw(error(domain_error(atom,Name),setenvstr/2-1)).
setenvstr(_Name, Value) :- \+ ( Value = [_|_] ; Value = [] ), !,
    throw(error(domain_error(character_code_list,Value),setenvstr/2-2)).
setenvstr(Name, Value) :-
    atom_codes(Value2, Value),
    c_set_env(Name, Value2).

:- trust pred c_set_env(+atm,+atm).
:- if(defined(optim_comp)).
:- '$props'(c_set_env/2, [impnat=cbool(prolog_c_set_env)]).
:- else.
:- impl_defined(c_set_env/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(set_env/2).
:- doc(set_env(Name, Value), "The environment variable @var{Name}
    is assigned @var{Value}.").
:- pred set_env(+atm, +atm).

set_env(Name, _Value) :- \+ atom(Name), !,
    throw(error(domain_error(atom,Name),set_env/2-1)).
set_env(_Name, Value) :- \+ atom(Value), !,
    throw(error(domain_error(atom,Value),set_env/2-2)).
set_env(Name, Value) :-
    c_set_env(Name, Value).

% ---------------------------------------------------------------------------
:- export(del_env/1).
:- doc(del_env(Name), "The environment variable @var{Name} is
   removed.").
:- pred del_env(+atm).

del_env(Name) :- \+ atom(Name), !,
    throw(error(domain_error(atom,Name),del_env/1-1)).
del_env(Name) :-
    c_del_env(Name).

:- trust pred c_del_env(+atm).
:- if(defined(optim_comp)).
:- '$props'(c_del_env/1, [impnat=cbool(prolog_c_del_env)]).
:- else.
:- impl_defined(c_del_env/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(current_env/2).
:- doc(current_env(Name,Value), "If @var{Name} is an atom, then
   unifies the environment variable @var{Name} with its value. Note
   that this predicate can be used to enumerate all the environment
   variables using backtracking.").
:- pred current_env(?atm, ?atm).

current_env(Name, _Value) :- \+ ( var(Name) ; atom(Name) ), !,
    throw(error(domain_error(var_or_atom,Name),current_env/2-1)).
current_env(_Name, Value) :- \+ ( var(Value) ; atom(Value) ), !,
    throw(error(domain_error(var_or_atom,Value),current_env/2-2)).
current_env(Name, Value) :-
    ( atom(Name) ->
        c_get_env(Name,Value)
    ; current_env_(0, Name, Value)
    ).

current_env_(I, Name, Value) :-
    c_current_env(I, Name2, Value2),
    ( Name=Name2, Value=Value2
    ; J is I + 1,
      current_env_(J, Name, Value)
    ).

:- trust pred c_current_env(+int,?atm,?atm).
:- if(defined(optim_comp)).
:- '$props'(c_current_env/3, [impnat=cbool(prolog_c_current_env)]).
:- else.
:- impl_defined(c_current_env/3).
:- endif.

% ---------------------------------------------------------------------------
:- export(extract_paths/2).
:- doc(extract_paths(PathList, Paths), "Split @var{PathList} atom into
   the list of paths @var{Paths}. Paths in @var{String} are separated
   by the @concept{path list separator character} (colons in
   POSIX-like systems, semicolons in Windows). Empty paths are removed
   from @var{Paths}. @var{Paths} is empty if @var{PathList} is the
   empty atom.").
:- trust pred extract_paths(+atm, ?list(atm)).
:- if(defined(optim_comp)).
:- '$props'(extract_paths/2, [impnat=cbool(prolog_extract_paths)]).
:- else.
:- impl_defined(extract_paths/2).
:- endif.

% % (reference implementation which does not ignore empty paths)
%
% extract_paths('', []) :- !.
% extract_paths(PathList, [Path|Paths]) :-
%       atom_codes(PathList, [C|Cs]),
%         extract_path(C, Cs, "", PathStr, Cs_),
%       atom_codes(Path, PathStr),
%         extract_paths1(Cs_, Paths).
% 
% extract_paths0([], ['']).
% extract_paths0([C|Cs], [Path|Paths]) :-
%         extract_path(C, Cs, "", PathStr, Cs_),
%       atom_codes(Path, PathStr),
%         extract_paths1(Cs_, Paths).
% 
% extract_paths1([], []).
% extract_paths1([_|Cs], Paths) :- % skip path list separator character
%         extract_paths0(Cs, Paths).
% 
% extract_path(C, Cs, Path, Path, [C|Cs]) :- pathlistsep(C), !.
% extract_path(C, [], _, [C], []) :- !.
% extract_path(C, [D|Cs], _, [C|Path], Cs_) :-
%         extract_path(D, Cs, [], Path, Cs_).
% 
% % Separator for lists of paths
% pathlistsep(C) :- using_windows, !, C = 0';.
% pathlistsep(0':).

% % TODO: bug in unittests?
% :- test extract_paths(A, B) : (A = '') => (B = []) # "Empty path list".
% :- test extract_paths(A, B) : (A = 'a:b') => (B = ['a','b']) # "Two paths".
% % :- test extract_paths(A, B) : (A = ':b:') => (B = ['', 'b', '']) # "Empty paths".
% :- test extract_paths(A, B) : (A = ':b:') => (B = ['b']) # "Ignore empty paths".

% ---------------------------------------------------------------------------
:- export(current_host/1).
:- doc(current_host(Hostname), "@var{Hostname} is unified with the
    fully qualified name of the host.").
:- trust pred current_host(?atm).
:- if(defined(optim_comp)).
:- '$props'(current_host/1, [impnat=cbool(prolog_current_host)]).
:- else.
:- impl_defined(current_host/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(current_executable/1).
:- doc(current_executable(Path), "Unifies @var{Path} with the path to
   the current Ciao executable (which may be a standalone binary or
   bytecode executable)").
% TODO: what happens for ciao-shell scripts?
:- trust pred current_executable(?atm).
:- if(defined(optim_comp)).
:- '$props'(current_executable/1, [impnat=cbool(prolog_current_executable)]).
:- else.
:- impl_defined(current_executable/1).
:- endif.

% TODO: add predicate to obtain base directory from scripts? (using
% current_executable+path_split). This should be equivalent to the
% following code used in scripts:
%
% # Physical directory where the script is located
% _base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
%         cd "$d";done;cd "$(dirname "$e")";pwd -P)

% ---------------------------------------------------------------------------
:- export(umask/2).
:- trust pred umask(OldMask, NewMask):int(NewMask) => int(OldMask) #
    "The process file creation mask was @var{OldMask}, and it is changed to @var{NewMask}.".

:- trust pred umask(OldMask, NewMask)
    : (var(OldMask), var(NewMask), OldMask == NewMask)
       => (int(OldMask), int(NewMask))
    # "Gets the process file creation mask without changing it.".
:- if(defined(optim_comp)).
:- '$props'(umask/2, [impnat=cbool(prolog_unix_umask)]).
:- else.
:- impl_defined(umask/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(working_directory/2).
:- doc(working_directory(OldDir, NewDir),"Unifies current working
     directory with @var{OldDir}, and then changes the working
     directory to @var{NewDir}. Calling
     @tt{working_directory(Dir,Dir)} simply unifies @tt{Dir} with the
     current working directory without changing anything else.").
:- trust pred working_directory(?atm, +atm) # "Changes current working directory.".
:- trust pred working_directory(OldDir, NewDir)
    : (var(OldDir), var(NewDir), OldDir == NewDir) => atm * atm
    # "Gets current working directory.".
:- if(defined(optim_comp)).
:- '$props'(working_directory/2, [impnat=cbool(prolog_unix_cd)]).
:- else.
:- impl_defined(working_directory/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(cd/1).
:- doc(cd(Path), "Changes working directory to @var{Path}.").
:- pred cd(+atm).

cd(Dir) :- working_directory(_, Dir).

% ---------------------------------------------------------------------------
:- export(directory_files/2).
:- doc(directory_files(Directory, FileList), "@var{FileList} is
   the unordered list of entries (files, directories, etc.) in
   @var{Directory}.").
:- trust pred directory_files(+atm,?list(atm)).
:- if(defined(optim_comp)).
:- '$props'(directory_files/2, [impnat=cbool(prolog_directory_files)]).
:- else.
:- impl_defined(directory_files/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(mktemp/2).
:- doc(mktemp(Template, Filename), "Returns a unique
   @var{Filename} based on @var{Template}: @var{Template} must be a
   valid file name with six trailing X, which are substituted to
   create a new file name.  @var{Filename} is created in read/write mode 
   but closed immediately after creation.").
:- trust pred mktemp(+atm, ?atm).
:- if(defined(optim_comp)).
:- '$props'(mktemp/2, [impnat=cbool(prolog_unix_mktemp)]).
:- else.
:- impl_defined(mktemp/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(mktemp_in_tmp/2).
mktemp_in_tmp(Template, Filename) :-
    get_tmp_dir(TmpDir),
    path_concat(TmpDir, Template, TmpDirTemplate),
    mktemp(TmpDirTemplate, Filename).

% ---------------------------------------------------------------------------
:- export(file_exists/1).
:- doc(file_exists(File), "Succeeds if @var{File} (a file or
    directory) exists (and is accessible).").
:- pred file_exists/1: atm.

file_exists(Path) :- file_exists(Path, 0).

% ---------------------------------------------------------------------------
:- export(file_exists/2).
:- doc(file_exists(File, Mode), "@var{File} (a file or directory)
   exists and it is accessible with @var{Mode}, as in the Unix call
   @tt{access(2)}. Typically, @var{Mode} is 4 for read permission, 2
   for write permission and 1 for execute permission.").
:- trust pred file_exists(+atm, +int) => atm * int.
:- if(defined(optim_comp)).
:- '$props'(file_exists/2, [impnat=cbool(prolog_unix_access)]).
:- else.
:- impl_defined(file_exists/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(file_property/2).
:- doc(file_property(File, Property), "@var{File} has the property
   @var{Property}. The possible properties are:

@begin{description}

@item{type(@var{Type})} @var{Type} is one of @tt{regular}, @tt{directory},
      @tt{fifo}, @tt{socket} or @tt{unknown}.

@item{linkto(@var{Linkto})} If @var{File} is a symbolic link,
      @var{Linkto} is the file pointed to by the link (and the other
      properties come from that file, not from the link itself).

@item{mod_time(@var{ModTime})} @var{ModTime} is the time of last
      modification (seconds since January, 1, 1970).

@item{mode(@var{Protection})} @var{Protection} is the protection mode.

@item{size(@var{Size})} @var{Size} is the size.

@end{description}

   If @var{Property} is uninstantiated, the predicate will enumerate the
   properties on backtracking.").

:- pred file_property(+atm, ?struct).

file_property(Path, Property) :-
    file_property_(Property, Path).

file_property_(Property, Path) :-
    var(Property), !,
    file_properties(Path, Type, Linkto, Time, Protection, Size),
    ( Property = type(Type)
    ; Linkto \== '', Property = linkto(Linkto)
    ; Property = mod_time(Time)
    ; Property = mode(Protection)
    ; Property = size(Size)
    ).
file_property_(type(Type), Path) :- !,
    file_properties(Path, Type0, [], [], [], []),
    Type = Type0.
file_property_(linkto(File), Path) :- !,
    file_properties(Path, [], File0, [], [], []),
    File0 \== '',
    File = File0.
file_property_(mod_time(Time), Path) :- !,
    file_properties(Path, [], [], Time, [], []).
file_property_(mode(Protection), Path) :- !,
    file_properties(Path, [], [], [], Protection, []).
file_property_(size(Size), Path) :- !,
    file_properties(Path, [], [], [], [], Size).
file_property_(Other, _) :-
    throw(error(domain_error(file_property_type,Other),
                file_property/2-2)).

% ---------------------------------------------------------------------------
:- export(file_properties/6).
:- doc(file_properties(Path, Type, Linkto, Time, Protection, Size),
    "The file @var{Path} has the following properties:

@begin{itemize} 

@item File type @var{Type} (one of @tt{regular}, @tt{directory},
      @tt{fifo}, @tt{socket} or @tt{unknown}).

@item If @var{Path} is a symbolic link, @var{Linkto} is the file pointed
      to.  All other properties come from the file pointed, not the
      link.  @var{Linkto} is '' if @var{Path} is not a symbolic link.

@item Time of last modification @var{Time} (seconds since January, 1,
      1970).

@item Protection mode @var{Protection}.

@item Size in bytes @var{Size}.

@end{itemize}
").
:- trust pred file_properties(+atm, ?atm, ?atm, ?int, ?int, ?int).
:- if(defined(optim_comp)).
:- '$props'(file_properties/6, [impnat=cbool(prolog_file_properties)]).
:- else.
:- impl_defined(file_properties/6).
:- endif.

% ---------------------------------------------------------------------------
:- export(modif_time/2).
:- doc(modif_time(File, Time), "The file @var{File} was last
     modified at @var{Time}, which is in seconds since January, 1,
     1970. Fails if @var{File} does not exist.").
:- pred modif_time(+atm, ?int).

modif_time(Path, Time) :-
    prolog_flag(fileerrors, OldFE, off),
    ( file_properties(Path, [], [], Time, [], []) ->
        set_prolog_flag(fileerrors, OldFE)
    ; set_prolog_flag(fileerrors, OldFE),
      fail
    ).

% ---------------------------------------------------------------------------
:- export(modif_time0/2).
:- doc(modif_time0(File, Time), "If @var{File} exists, @var{Time} is
      its latest modification time, as in @pred{modif_time/2}.
      Otherwise, if @var{File} does not exist, @var{Time} is zero.").
:- pred modif_time0(+atm, ?int).

modif_time0(Path, Time) :-
    prolog_flag(fileerrors, OldFE, off),
    ( file_properties(Path, [], [], T, [], []), !
    ; T = 0
    ),
    set_prolog_flag(fileerrors, OldFE),
    Time = T.

% ---------------------------------------------------------------------------
:- export(touch/1).
:- doc(touch(File), "Change the modification time of @var{File} to the
   current time of day. If the file does not exist, it is created with
   default permissions.

   @bf{Note:} This operation cannot be fully implemented with
   @pred{modif_time/2}. In POSIX systems, that can be done as long as
   the user has write permissions on the file, even if the owner is
   different. Change of modification time to arbitrary time values is
   not allowed in this case.").
:- pred touch(+atm).
:- if(defined(optim_comp)).
:- '$props'(touch/1, [impnat=cbool(prolog_touch)]).
:- else.
:- impl_defined(touch/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(fmode/2).
:- doc(fmode(File, Mode), "The file @var{File} has protection mode
    @var{Mode}.").
:- pred fmode(+atm, ?int).

fmode(Path, Mode) :-
    file_properties(Path, [], [], [], Mode, []).

% ---------------------------------------------------------------------------
:- export(chmod/2).
:- doc(chmod(File, NewMode), "Change the protection mode of file
    @var{File} to @var{NewMode}.").
:- trust pred chmod(+atm, +int).
:- if(defined(optim_comp)).
:- '$props'(chmod/2, [impnat=cbool(prolog_unix_chmod)]).
:- else.
:- impl_defined(chmod/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(chmod/3).
:- doc(chmod(File, OldMode, NewMode), "The file @var{File} has
    protection mode @var{OldMode} and it is changed to @var{NewMode}.").
:- pred chmod(+atm, ?int, +int).

:- pred chmod(File, OldMode, NewMode)
      : (atm(File), var(OldMode), var(NewMode))
      => atm * atm * atm
      # "If @var{OldMode} is identical to @var{NewMode} then it is 
          equivalent to fmode(@var{File},@var{OldMode})".

chmod(Path, OldMode, NewMode) :-
    OldMode == NewMode, !,
    fmode(Path, OldMode).
chmod(Path, OldMode, NewMode) :-
    fmode(Path, OldMode),
    chmod(Path, NewMode).

% ---------------------------------------------------------------------------
:- export(set_exec_mode/2).
:- doc(set_exec_mode(SourceName, ExecName), "Copies the
    permissions of @var{SourceName} to @var{ExecName} adding
    permissions to execute.").
:- pred set_exec_mode(+atm, +atm).

set_exec_mode(SourceName, ExecName) :-
    fmode(SourceName, M0),
    M1 is M0 \/ ((M0 >> 2) /\ 0o111), % Copy read permissions to execute
    chmod(ExecName, M1).

% ---------------------------------------------------------------------------
:- export(delete_directory/1).
:- doc(delete_directory(File), "Delete the directory @var{Directory}.").
:- trust pred delete_directory(+atm).
:- if(defined(optim_comp)).
:- '$props'(delete_directory/1, [impnat=cbool(prolog_unix_rmdir)]).
:- else.
:- impl_defined(delete_directory/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(delete_file/1).
:- doc(delete_file(File), "Delete the file @var{File}.").
:- trust pred delete_file(+atm).
:- if(defined(optim_comp)).
:- '$props'(delete_file/1, [impnat=cbool(prolog_unix_delete)]).
:- else.
:- impl_defined(delete_file/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(rename_file/2).
:- doc(rename_file(File1, File2), 
    "Change the name of  @var{File1} to @var{File2}.").
:- trust pred rename_file(+atm,+atm).
:- if(defined(optim_comp)).
:- '$props'(rename_file/2, [impnat=cbool(prolog_unix_rename)]).
:- else.
:- impl_defined(rename_file/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(make_directory/2).
:- doc(make_directory(DirName, Mode), "Creates the directory
   @var{DirName} with a given @var{Mode}.  This is, as usual, operated
   against the current umask value.").
:- trust pred make_directory(+atm, +int).
:- if(defined(optim_comp)).
:- '$props'(make_directory/2, [impnat=cbool(prolog_unix_mkdir)]).
:- else.
:- impl_defined(make_directory/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(make_directory/1).
:- doc(make_directory(DirName),
    "Equivalent to @tt{make_directory(D,0o777)}.").
:- pred make_directory(+atm).

make_directory(D) :-
    make_directory(D,0o777).

% ---------------------------------------------------------------------------
:- export(system_error_report/1).
:- pred system_error_report(Report) : var(Report) => atm(Report)
# "Report is the error message from the last system call, like
  @tt{strerror} in POSIX.".

system_error_report(X) :-
    c_strerror(X).

:- trust pred c_strerror(-atm).
:- if(defined(optim_comp)).
:- '$props'(c_strerror/1, [impnat=cbool(prolog_c_strerror)]).
:- else.
:- impl_defined(c_strerror/1).
:- endif.

% ---------------------------------------------------------------------------
% TODO: deprecate, not used
% :- export(c_errno/1).
:- trust pred c_errno(?int).
:- if(defined(optim_comp)).
:- '$props'(c_errno/1, [impnat=cbool(prolog_c_errno)]).
:- else.
:- impl_defined(c_errno/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_tmp_dir/1). % TODO: see TODO in predicate
% TODO: do not assume trailing '/' -- use path_concat in clients
:- pred get_tmp_dir(TmpDir) : var(TmpDir) => atm(TmpDir) #
  "@var{TmpDir} is the (normalized) temporary directory for scratch
   space. On POSIX systems it is computed from normalizing the path
   specified in the @tt{TMPDIR} environment variable. On Windows it is
   determined by the @tt{TMP} or @tt{TEMP} environment variable. If
   none is defined, this predicate tries to guess it from some usual
   locations (@tt{/tmp}, @tt{/var/tmp}, @tt{/usr/tmp} on POSIX,
   @tt{c:\\\\temp}, @tt{c:\\\\tmp}, @tt{\\\\temp}, @tt{\\\\tmp} on Windows) or
   use @tt{.} as last resort.".

get_tmp_dir(TmpDir) :-
    ( % get from environment
      tmpdir(TmpDir0)
    ; % or, guess some dir
      try_tmpdir(TmpDir0),
      file_exists(TmpDir0, 2) % 2 is for writing
    ),
    !,
    fixed_absolute_file_name(TmpDir0, '.', TmpDir). % Normalize

try_tmpdir(TmpDir) :- using_windows, !,
    try_tmpdir_win32(TmpDir).
try_tmpdir(TmpDir) :-
    try_tmpdir_posix(TmpDir).

try_tmpdir_posix('/tmp').
try_tmpdir_posix('/var/tmp').
try_tmpdir_posix('/usr/tmp').
try_tmpdir_posix('.').

try_tmpdir_win32('c:\\temp').
try_tmpdir_win32('c:\\tmp').
try_tmpdir_win32('\\temp').
try_tmpdir_win32('\\tmp').
try_tmpdir_win32('.').

tmpdir(TmpDir) :- using_windows, !, % Windows
    ( c_get_env('TMP', TmpDir0) -> TmpDir = TmpDir0
    ; c_get_env('TEMP', TmpDir0) -> TmpDir = TmpDir0
    ; fail
    ).
tmpdir(TmpDir) :- % POSIX
    c_get_env('TMPDIR', TmpDir).

% ---------------------------------------------------------------------------
:- export(dev_null/1).
:- pred dev_null(Path) => atm(Path)
   # "File path for the null device (@tt{/dev/null} file in POSIX,
     @tt{nul} in Windows)".

dev_null(Path) :- using_windows, !, Path = 'nul'.
dev_null('/dev/null').

% ---------------------------------------------------------------------------
:- export(pause/1).
:- doc(pause(Seconds), "Make this thread sleep for some @var{Seconds}.").
:- trust pred pause(+int).
:- if(defined(optim_comp)).
:- '$props'(pause/1, [impnat=cbool(prolog_pause)]).
:- else.
:- impl_defined(pause/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(wait/2).
:- trust pred wait(+Pid, -ReturnCode) :
     (int(Pid),var(ReturnCode))
     =>  (int(ReturnCode))
   # "@pred{wait/2} waits for the process numbered @var{Pid}. Fails
      if the process does not terminate normally or in case of error
      (see C @tt{waitpid()} for details). @var{RetCode} is the process
      return code.".
:- if(defined(optim_comp)).
:- '$props'(wait/2, [impnat=cbool(prolog_wait)]).
:- else.
:- impl_defined(wait/2).
:- endif.

% (see internals:$exec/9' for low-level process creation or
% library(process) for a higher-level interface)

% ---------------------------------------------------------------------------
:- export(kill/2).
:- trust pred kill(+Pid, +Signal) : (int(Pid),int(Signal))
   # "@pred{kill/2} sends the signal @var{Signal} to the process or
      process group specified by @var{Pid}. See Unix man page for a
      detailed description of signals.".
:- if(defined(optim_comp)).
:- '$props'(kill/2, [impnat=cbool(prolog_kill)]).
:- else.
:- impl_defined(kill/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_pid/1).
:- doc(get_pid(Pid), "Unifies @var{Pid} with the process
     identificator of the current process or thread.").
:- trust pred get_pid(?int).
:- if(defined(optim_comp)).
:- '$props'(get_pid/1, [impnat=cbool(prolog_getpid)]).
:- else.
:- impl_defined(get_pid/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_uid/1).
:- doc(get_uid(Uid), "Unifies @var{Uid} with the user id of the
     current process.").
:- trust pred get_uid(?int).
:- if(defined(optim_comp)).
:- '$props'(get_uid/1, [impnat=cbool(prolog_getuid)]).
:- else.
:- impl_defined(get_uid/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_gid/1).
:- doc(get_gid(Uid), "Unifies @var{Gid} with the group id of the
     current process.").
:- trust pred get_gid(?int).
:- if(defined(optim_comp)).
:- '$props'(get_gid/1, [impnat=cbool(prolog_getgid)]).
:- else.
:- impl_defined(get_gid/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_pwnam/1).
:- doc(get_pwnam(User), "Unifies @var{User} with the user of the
     current process, as specified in the /etc/passwd file.").
:- trust pred get_pwnam(?atm).
:- if(defined(optim_comp)).
:- '$props'(get_pwnam/1, [impnat=cbool(prolog_getpwnam)]).
:- else.
:- impl_defined(get_pwnam/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_grnam/1).
:- doc(get_grnam(Group), "Unifies @var{Group} with the group of
     the current process, as specified in the /etc/group file.").
:- trust pred get_grnam(?atm).
:- if(defined(optim_comp)).
:- '$props'(get_grnam/1, [impnat=cbool(prolog_getgrnam)]).
:- else.
:- impl_defined(get_grnam/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_numcores/1).
:- doc(get_numcores(N), "Unifies @var{N} with the number of CPU cores.").
:- trust pred get_numcores(?int).
:- if(defined(optim_comp)).
:- '$props'(get_numcores/1, [impnat=cbool(prolog_get_numcores)]).
:- else.
:- impl_defined(get_numcores/1).
:- endif.

% ---------------------------------------------------------------------------
:- export(shell/0).
:- trust pred shell # "Executes the OS-specific system shell. When the
   shell process terminates, control is returned to Prolog. See
   @pred{shell/2} for details.".
:- if(defined(optim_comp)).
:- '$props'(shell/0, [impnat=cbool(prolog_unix_shell0)]).
:- else.
:- impl_defined(shell/0).
:- endif.

% ---------------------------------------------------------------------------
:- export(shell/1).
:- doc(shell(Command), "@var{Command} is executed in the OS-specific
   system shell. It succeeds if the exit code is zero and fails
   otherwise. See @pred{shell/2} for details.").
:- pred shell(+atm).
shell(Path) :- shell(Path, 0).

% ---------------------------------------------------------------------------
:- export(shell/2).
:- doc(shell(Command, RetCode), "Executes @var{Command} using the
   OS-specific system shell and stores the exit code in
   @var{RetCode}.

   On POSIX-like systems the system shell is specified by the
   @tt{SHELL} environment variable (@tt{$SHELL -c \"command\"} for
   passing user commands). On Windows (native builds, MinGW) it is
   specified by the @tt{COMSPEC} environment variable (@tt{%COMSPEC%
   /s /c \"command\"} for passing user commands).

   Note that the use of @pred{shell/2} predicates is discouraged for
   portability and security reasons. Please consider @lib{process} for
   a more robust way to launch external processes.").

:- trust pred shell(+atm, ?int).
:- if(defined(optim_comp)).
:- '$props'(shell/2, [impnat=cbool(prolog_unix_shell2)]).
:- else.
:- impl_defined(shell/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(system/1).
:- doc(system(Command), "Like @pred{shell/1} but ignores exit code.").
:- pred system(+atm).

% TODO: make it a synonym for shell/1? (replace all suspicious uses with system(Cmd,_))
% TODO: In SICStus, fails if return not zero, i.e., should be:
%% system(Path) :- system(Path, 0).?????

system(Path) :- shell(Path, _RetCode).

% ---------------------------------------------------------------------------
:- export(system/2).
:- doc(system(Command, RetCode), "Synonym for @pred{shell/2}.").
:- trust pred system(+atm, ?int).

system(Command, RetCode) :- shell(Command, RetCode).

% ---------------------------------------------------------------------------

:- export(fd_dup/2).
:- doc(fd_dup(FD, NewFD), "Duplicate the file descriptor @var{FD} into @var{NewFD}").
:- pred fd_dup(+int, -int) # "Duplicate a file description into a new
   free file descriptor (C @tt{dup()})".
:- pred fd_dup(+int, +int) # "Duplicate a file description into a specified
   file descriptor (C @tt{dup2()})".

:- if(defined(optim_comp)).
:- '$props'(fd_dup/2, [impnat=cbool(prolog_fd_dup)]).
:- else.
:- impl_defined(fd_dup/2).
:- endif.

% ---------------------------------------------------------------------------

:- export(fd_close/1).
:- pred fd_close(+int) # "Close the file descriptor (using C close())".

:- if(defined(optim_comp)).
:- '$props'(fd_close/1, [impnat=cbool(prolog_fd_close)]).
:- else.
:- impl_defined(fd_close/1).
:- endif.

% ---------------------------------------------------------------------------
:- regtype winpath_option/1.

winpath_option(full).
winpath_option(relative).

% ---------------------------------------------------------------------------
:- export(winpath/3).
:- doc(winpath(Option, Posix, WinPath), "@var{Option} specifies if
   you want to get a relative or a full path.  @var{Posix} represent a
   path as usual in unix, and @var{WinPath} is the Windows-Style
   representation of @var{Posix}.").
:- pred winpath(-winpath_option, -atm, +atm).
:- pred winpath(-winpath_option, +atm, -atm).

% ---------------------------------------------------------------------------
:- export(winpath/2).
:- pred winpath(A,B): (atm(A), var(B)) => (atm(A),atm(B)).
:- pred winpath(A,B): (var(A), atm(B)) => (atm(A),atm(B)).
:- pred winpath(A,B): (atm(A), atm(B)) => (atm(A),atm(B)).

winpath(Path, WinPath) :-
    winpath(full, Path, WinPath).

winpath(Full,_Path,_WinPath) :-
    ( \+ winpath_option(Full) ),
    !,
    throw(error(domain_error(winpath_option,Full),winpath/3-1)).
winpath(_Full, Path,_WinPath) :-
    ( \+ ( var(Path); atom(Path) ) ),
    !,
    throw(error(domain_error(var_or_atom,Path),winpath/3-2)).
winpath(_Full,_Path, WinPath) :-
    ( \+ ( var(WinPath); atom(WinPath) ) ),
    !,
    throw(error(domain_error(var_or_atom,WinPath),winpath/3-3)).
winpath(Full, Path, WinPath) :-
    ( var(Full), var(Path), var(WinPath) ),
    !,
    throw(error(instantiation_error)).
winpath(full, Path, WinPath) :-
    ( atom(Path) ->
        c_winpath(Path, WinPath)
    ; c_posixpath(WinPath, Path)
    ).
winpath(relative, Path, WinPath) :-
    ( atom(Path) ->
        c_winfile(Path, WinPath)
    ; c_posixfile(WinPath, Path)
    ).

:- trust pred c_posixpath(+atm,?atm).
:- if(defined(optim_comp)).
:- '$props'(c_posixpath/2, [impnat=cbool(prolog_c_posixpath)]).
:- else.
:- impl_defined(c_posixpath/2).
:- endif.

:- trust pred c_winpath(+atm,?atm).
:- if(defined(optim_comp)).
:- '$props'(c_winpath/2, [impnat=cbool(prolog_c_winpath)]).
:- else.
:- impl_defined(c_winpath/2).
:- endif.

:- if(defined(optim_comp)).
:- '$props'(c_winfile/2, [impnat=cbool(prolog_c_winfile)]).
:- else.
:- impl_defined(c_winfile/2).
:- endif.
:- if(defined(optim_comp)).
:- '$props'(c_posixfile/2, [impnat=cbool(prolog_c_posixfile)]).
:- else.
:- impl_defined(c_posixfile/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(winpath_c/3).
:- doc(winpath_c/3, "Same as winpath/3, but for strings.").

winpath_c(Option, Dir, Path) :-
    atom_codes(DirA, Dir),
    winpath(Option, DirA, PathA),
    atom_codes(PathA, Path).

% ---------------------------------------------------------------------------
:- export(cyg2win/3).
:- pred cyg2win(CygWinPath, WindowsPath, SwapSlash) : string * var *
   atom => string * string * atom # "Converts a posix path to a
   Windows-style path.  If @var{SwapSlash} is @tt{swap}, slashes are
   converted in to backslash.  If it is @tt{noswap}, they are
   preserved.".

cyg2win(Dir, Path, Swap) :-
    winpath_c(relative, Dir, PathSwap),
    no_swapslash(Swap, PathSwap, Path).

:- export(no_swapslash/3).
% TODO: Check this code w.r.t. what the documentation says.
no_swapslash(swap, Dir, Dir) :-
    !.
no_swapslash(noswap, Dir, Path) :-
    do_no_swapslash(Dir, Path).

do_no_swapslash(Dir, Path) :-
    replace_characters(Dir, 0'\\, 0'/, Path).

% TODO: bad name
:- doc(replace_characters(String, SearchChar, ReplaceChar,
   Output), "Replaces all the occurrences of @var{SearchChar} by
   @var{ReplaceChar} and unifies the result with @var{Output}").

replace_characters([], _, _, []).
replace_characters([S|Ss], C, R, [T|Ts]) :-
    replace_character(S, C, R, T),
    replace_characters(Ss, C, R, Ts).

replace_character(S, S, R, R) :- !.
replace_character(S, _, _, S).

% ---------------------------------------------------------------------------
:- export(cyg2win_a/3).
:- doc(cyg2win_a/3, "Same as cyg2win/3, but for atoms.").

cyg2win_a(Path, WindifiedPath, Swap) :-
    atom_codes(Path, Codes),
    cyg2win(Codes, WindifiedCodes, Swap),
    atom_codes(WindifiedPath, WindifiedCodes).

% ---------------------------------------------------------------------------
% TODO: better name? equivalent to os.name=="nt" in python
:- export(using_windows/0).
:- trust pred using_windows # "Using the Windows native API (not
   POSIX)".

:- if(defined(optim_comp)).
:- '$props'(using_windows/0, [impnat=cbool(prolog_using_windows)]).
:- else.
:- impl_defined(using_windows/0).
:- endif.

% ---------------------------------------------------------------------------
:- export(get_home/1).
:- pred get_home(-H) # "@var{H} is the home directory (@tt{HOME}
environment variable in POSIX systems and APPDATA in Windows)".
% TODO: use C function directly
get_home(H) :-
    fixed_absolute_file_name('~', '.', H).

% ---------------------------------------------------------------------------
:- export(find_executable/2). % TODO: use c_find_exec() from system.c (take Prolog as reference)
:- pred find_executable(+Name, -Path) # "@var{Path} is the absolute
   path of the command @var{Name}, reachable from the @tt{PATH}
   (environment variable) directories if @var{Name} is not an absolute
   path. The suffix for executable files is optionally added to
   @var{Path} depending on the current operating system.".

find_executable(Name, Path) :-
    exec_name(Name, ExecName), % (nondet)
    find_executable_(ExecName, Path0),
    !,
    Path = Path0.

find_executable_(File, Path) :-
    file_exists(File),
    !,
    Path = File. % TODO: strange, it may be relative...
find_executable_(File, Path) :-
    get_paths(Dir), % (nondet)
    path_concat(Dir, File, Path0),
    file_exists(Path0),
    !,
    Path = Path0.

exec_name(Cmd, Exec) :-
    % Try with executable extension first
    get_exec_ext(Ext),
    Ext \== '',
    atom_concat(Cmd, Ext, Exec).
exec_name(Cmd, Cmd).

% Dir is a directory in the PATH environment variable
get_paths(Dir) :-
    getenvstr('PATH', PathStr),
    atom_codes(Path, PathStr),
    extract_paths(Path, PathList),
    member(Dir, PathList).

