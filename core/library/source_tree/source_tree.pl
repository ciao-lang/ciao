:- module(_, [], [assertions, fsyntax, hiord, regtypes, isomodes]).

:- doc(title, "Operations on source trees").

:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao group").

:- doc(module, "This module defines predicates to operate on directory
   trees containing source code (@concept{source trees}). 

   The possible operations include enumerating, copying, and cleaning
   files or selection of files based on filters. See
   @pred{source_filter/1} for a description of the available filters.

   @section{Details of the walk algorithm}

   The walk algorithm (see internal @pred{walk/3}) traverses a
   filtered directory structure and invokes actions (see internal
   @pred{action_hook/3}, @pred{walk_action/1}) parameterized by walk
   events (see internal @pred{walk_event/1}) before entering a
   directory (@tt{enter} event), when a file is processed (@tt{file}
   event), and once the files in a directory are processed (@tt{exit}
   event).

   Filters restrict the walk to selected parts of the file
   tree. Filters are decomposed into formulas of basic filters, where
   a basic filter is a property of a regular file or directory (e.g.,
   based on its name or path).

   A normalized filter (see internal @pred{norm_filter/2}) is given by
   the tuple of formulas (@var{WalkP}, @var{DirP}, @var{FileP}), where
   each component represents the following conditions for the walk
   algorithm:

   @begin{description}
   @item{@var{WalkP}} walk into the directory
   @item{@var{DirP}} treat directory files
   @item{@var{FileP}} treat the file (a regular file or a directory
     not marked for walk)
   @end{description}

   Given a base directory, the walk algorithm will perform the
   following operation on each file @var{F}:

@begin{verbatim}
   if F is a directory and WalkP(F):
      if DirP(F):
         Action 'enter' on F
         call recursively for each file in F
         Action 'exit' on F
   else:
      if FileP(F):
         Action 'file' on F
@end{verbatim}

   @section{Efficiency and memory usage}

   @begin{itemize}
   @item Evaluation of filter checks is optimized to minimize accesses
     to the filesystem.
   @item This code depends on the regular expression library, which is
     currently not optimized for performance.
   @item Since file names are encoded as atoms, this code may suffer
     from exhaustion of the atom table on large file trees.
   @end{itemize}

   @section{Examples}

   Find cleanable files in the current directory (recursively):
   @begin{verbatim}
   ?- current_file_find([cleanable(src)], '.', X).
   @end{verbatim}

   Find distributable packages:
   @begin{verbatim}
   ?- current_file_find([proj(distributable), srctype(package)], '.', X).
   @end{verbatim}
").

:- doc(bug, "(bug) Does not work with CIAOCACHEDIR enabled").

:- doc(bug, "(documentation) Add more examples").
%
%  Some advanced examples:
%   ( current_file_find([untainted, 
%                        file_p-match(name, glob('*.txt')),
%                        nonrec],
%                       '~/Documents/git/ciao-devel', X),
%     display(X), nl, fail
%   ; true
%   ).
%   ( current_file_find([proj(distributable),
%                        file_p-match(name, glob('Manifest*'))],
%                       '~/Documents/git/ciao-devel', X),
%     display(X), nl, fail
%   ; true 
%   ).
%   ( current_file_find([proj(distributable),
%                        file_p-match(name, glob('*clp*')),
%                        file_p-srctype(package),
%                        file_p-not(match(filename, glob('*contrib*')))]
%                       '~/Documents/git/ciao-devel', X),
%     display(X), nl, fail
%   ; true 
%   ).
%   ( current_file_find([compilable_module,
%                        file_p-match(filename, glob('*/contrib/*'))],
%                       '~/Documents/git/ciao-devel', X),
%     display(X), nl, fail
%   ; true
%   ).

:- doc(bug, "(code) This code and documentation could be greatly
   improved by language extensions (like hooks definitions)").

:- doc(bug, "(code) Use (;)/2 and (,)/2 for filters").

:- doc(bug, "(code) Use dynamic facts instead of peep_p (the
   implementation may be simpler and more efficient)").

:- doc(bug, "(code) Merge with bundle_scan.pl? (at least some
   predicates)").

:- doc(bug, "(performance) Optimize regexp/glob matching").

:- doc(bug, "(performance) We need atom GC").

:- doc(bug, "(feature) Use a single PROPS or META file to store
   metadata?").

:- doc(bug, "(feature) Extend and document all filter expressions").

:- doc(bug, "(feature) Maybe WalkP can be inserted into DirP and
   FileP (see appendix)").

:- doc(bug, "(feature) Add filters for system:file_property").

:- doc(bug, "(feature) OR-composition of filters is not supported (we
   need a more involved algorithm that avoid duplicates)").

:- doc(bug, "(feature) Add a predoc filter? (similar to precomp)").

% NOTE: This module can be tested with core/tests/test_source_tree.
%   Recompile and use it as follows:
% 
%     $ ciao build core/shell && \
%       ciaoc test_source_tree && ./test_source_tree
%
%   The packaged bundle generation, which also depends on this module,
%   can also be used as a test. E.g.,
%
%     $ ciao gen-pbundle--bin
%
%   and compare the generated .list file that is left in
%   build/pbundle.

% ===========================================================================

:- doc(section, "Generic support for source tree walking").

% ---------------------------------------------------------------------------

:- doc(subsection, "Interface for walk actions").

:- regtype walk_action(Action) # "@var{Action} is a walk action".
:- impl_defined(walk_action/1).
% (values of first argument of action_hook/3)

:- regtype walk_event(Event) # "@var{Event} is a walk event".
:- doc(walk_event(Event), "
   Events of walk that parameterize walk actions:
   @begin{description}
   @item{@tt{enter}} before visiting directory files (includes the
     base directory)
   @item{@tt{file}} visiting a file
   @item{@tt{exit}} after visiting directory files (includes the base
     directory)
   @end{description}
").

walk_event(enter).
walk_event(exit).
walk_event(file).

:- discontiguous action_hook/3.
:- pred action_hook(+Action, +Event, +Env) ::
     walk_action * walk_event * list(term)
   # "Perform a walk action".
% (defined later)

% ---------------------------------------------------------------------------

:- doc(subsection, "Source tree walk algorithm").

:- use_module(library(system),
	[file_exists/1, file_exists/2,
	 file_property/2,
	 directory_files/2]).
:- use_module(library(pathnames),
	[path_concat/3, path_split/3, path_relocate/4]).
:- use_module(library(lists), [append/3]).

:- pred walk(+Filter, +BaseDir, +Action)
   # "Perform @var{Action} recursively on files matching @var{Filter}
     under @var{BaseDir}.".

walk(Filter, BaseDir, Action) :-
	norm_filter(Filter, NFilter),
	walk_dir(NFilter, BaseDir, [basedir(BaseDir)], Action).

walk_dir(NFilter, FileName, Env, Action) :-
	DirEnv = [filename(FileName)|Env],
        ( action_hook(Action, enter, DirEnv)         % Enter directory
	; walk_dir_(FileName, Env, NFilter, Action) % Walk inside directory
	; action_hook(Action, exit, DirEnv)          % Exit directory
	).

walk_dir_(CurrDir, Env, NFilter, Action) :-
	NFilter = nfilter(WalkP, DirP, FileP),
	% (peep literals of FileP that depend on the current directory)
        DirCtxEnv = ~peep_p(step_currdir(CurrDir), FileP, Env),
	% Get CurrDir children
	children(CurrDir, Env, ChildEnv, FileName),
	%
	( eval_p(WalkP, ChildEnv), % Walk on dir?
	  is_dir_nolink(FileName) ->
	    % Filter dir
	    eval_p(DirP, ChildEnv),
	    walk_dir(NFilter, FileName, Env, Action)
	; % (peep literals of FileP that depend on filename)
	  FileEnv = ~append(~peep_p(step_file, FileP, ChildEnv),
	                    ~append(DirCtxEnv, ChildEnv)),
	  % Filter file
	  eval_p(FileP, FileEnv),
	  action_hook(Action, file, FileEnv)
	).

children(CurrDir, Env, ChildEnv, FileName) :-
	directory_files(CurrDir, Files),
	member(Name, Files),
	\+ dot_dir(Name),
	path_concat(CurrDir, Name, FileName),
	ChildEnv = [name(Name), filename(FileName)|Env].

dot_dir('.').
dot_dir('..').

% FileName is a directory that is not a symbolic link
is_dir_nolink(FileName) :-
	\+ file_property(FileName, linkto(_)),
	file_exists(FileName),
	file_property(FileName, type(directory)).

:- pred foreach_file_find(+Filter, +BaseDir, +Action) ::
   source_filter * atm * walk_action
   # "Perform Action recursively on files matching @var{Filter} under
     @var{BaseDir}.".

foreach_file_find(Filter, BaseDir, Action) :-
	( % (failure-driven loop)
	  walk(Filter, BaseDir, Action),
	    fail
	; true
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Filter normalization").

:- use_module(library(sort), [sort/2]).
:- use_module(library(aggregates), [findall/3]).

% Definition of filters
:- discontiguous filter_def/2.
% Definition of match patterns
:- discontiguous match_def/3.

% Normalize a tree filter
norm_filter(Filter, NFilter) :-
	ground(Filter), % (very coarse check)
	( is_list(Filter) -> Filter2 = Filter ; Filter2 = [Filter] ),
	Filter3 = ~reduce_filter(Filter2),
	NFilter = ~expand_and_collect(Filter3).

is_list([]).
is_list([_|_]).

% Reduce a filter (apply filter_def/2)
reduce_filter([]) := [].
reduce_filter([X|Xs]) := Ys :-
	( X = true ->
	    Xs1 = Xs, Ys1 = Ys
	; filter_def(X, Def) ->
	    Xs1 = ~append(Def, Xs), Ys1 = Ys
	; Xs1 = Xs, Ys = [X|Ys1]
	),
	Ys1 = ~reduce_filter(Xs1).

% Classify and expand filters
expand_and_collect(Fs, NFilter) :-
	NFilter = nfilter(~expand_and_collect_p(walk_p, Fs),
			  ~expand_and_collect_p(dir_p, Fs),
	                  ~expand_and_collect_p(file_p, Fs)).

% Collect literals of Cond from Fs and expand them
expand_and_collect_p(Cond, Fs) := Expr :-
	% Group all Cond
	Exprs = ~findall(Expr0, member(Cond-Expr0, Fs)),
	%
	( Exprs = [] -> Expr = true % no filter
	; Expr = ~expand_p(and(Exprs)) % conjunction of filters
	).

% Expand match literals (apply match_def/3)
expand_p(not(Expr)) := P :- !,
	P = not(~expand_p(Expr)).
expand_p(and(Exprs)) := P :- !,
	P = and(~expand_p_list(Exprs)).
expand_p(or(Exprs)) := P :- !,
	P = or(~expand_p_list(Exprs)).
expand_p(match(Arg, Pattern)) := P :- !,
	P = ~expand_match(Arg, Pattern).
expand_p(P) := P.

expand_p_list([], []) :- !.
expand_p_list([K|Ks], [K2|Ks2]) :-
	K2 = ~expand_p(K),
	expand_p_list(Ks, Ks2).

expand_match(Arg, glob(E), P) :- !, P = match_glob(Arg, E).
expand_match(Arg, Pattern, P) :-
	Pattern =.. [N, Kind],
	E2 = ~sort(~findall(Def, match_def(Kind, N, Def))),
	( E2 = [E] -> P = match_glob(Arg, E)
	; P = or(~expand_match_list(E2, Arg))
	).

expand_match_list([], _Arg, []).
expand_match_list([X|Xs], Arg, [X2|Xs2]) :-
	X2 = match_glob(Arg, X),
	expand_match_list(Xs, Arg, Xs2).

% ---------------------------------------------------------------------------

:- doc(subsection, "Evaluation of normalized filter formulas").

% Environment dependencies for the literal (at the given step)
:- discontiguous depend_p/3.
% Evaluation of a literal
:- discontiguous builtin_p/2.
% Evaluation of the literal dependencies on the environment
:- discontiguous eval_env/3.

% Get a value from the environment
env_get(Prop, Env) :- ( member(Prop, Env) -> true ; fail ).

peep_p(Step, Expr, PrevEnv, Env) :-
	Env = ~sort(~findall(E, (depend_p(Lit, Step, E), in_p(Expr, Lit)))),
	eval_env_p(Env, PrevEnv, Step).

eval_env_p([], _, _).
eval_env_p([E|Env], PrevEnv, Step) :-
	eval_env(E, PrevEnv, Step),
	eval_env_p(Env, PrevEnv, Step).

% in_p(Expr, Lit): Lit appears in the Expr formula
in_p(or(Xs), Lit) :- !, in_list(Xs, Lit).
in_p(and(Xs), Lit) :- !, in_list(Xs, Lit).
in_p(not(X), Lit) :- !, in_p(X, Lit).
in_p(Lit, Lit).

in_list([], _) :- fail.
in_list([X|Xs], Lit) :-
	( in_p(X, Lit)
	; in_list(Xs, Lit)
	).

% Evaluate formula on the given environment
eval_p(true, _) :- !.
eval_p(false, _) :- !, fail.
eval_p(not(Expr), Env) :- !,
	\+ eval_p(Expr, Env).
eval_p(or(Exprs), Env) :- !,
	( member(Expr, Exprs),
	  eval_p(Expr, Env) -> true
	; fail
	).
eval_p(and(Exprs), Env) :- !,
	eval_conj(Exprs, Env).
eval_p(Expr, Env) :-
	builtin_p(Expr, Env).

eval_conj([], _).
eval_conj([X|Xs], Env) :-
	eval_p(X, Env),
	eval_conj(Xs, Env).

% ---------------------------------------------------------------------------

depend_p(glob_from_fileset(Files), step_currdir(_), or_from_files(Files, _)).
builtin_p(glob_from_fileset(Files), Env) :- !,
	% Get glob patterns from files in the base directory
	% (must be precomputed in Env)
	env_get(or_from_files(Files, Expr), Env),
	eval_p(Expr, Env).

eval_env(or_from_files(Files, Expr), _, step_currdir(CurrDir)) :-
	atoms_from_files(CurrDir, Files, Xs),
	Expr = or(~findall(match_glob(name, X), member(X, Xs))).

% Read all atoms from a set of files
atoms_from_files(BaseDir, Files, Exprs) :-
	findall(Expr, atoms_from_files_(BaseDir, Files, Expr), Exprs).

atoms_from_files_(BaseDir, Files, Expr) :-
	member(Name, Files),
	path_concat(BaseDir, Name, File),
	file_exists(File),
	file_to_atoms(File, Exprs),
	member(Expr, Exprs).

% ---------------------------------------------------------------------------

:- use_module(library(regexp/regexp_code), [match_term/2]).

% TODO: rename 'name' by 'basename'?
% TODO: add 'ext' (extension)?
% TODO: non-glob (exact) matches? (so that we can use prolog indexing)

% Elem matches Pattern
builtin_p(match_glob(Elem, Pattern), Env) :- atom(Pattern), !,
	( Elem = name -> env_get(name(X), Env)
	; Elem = filename -> env_get(filename(X), Env)
	; fail
	),
	match_term(Pattern, X).

% ---------------------------------------------------------------------------

builtin_p(dirmark(DirMark), Env) :- atom(DirMark), !,
	env_get(filename(Dir), Env),
	% The directory Dir is marked as specified in DirMark expression
	path_concat(Dir, DirMark, DirMarkName),
	file_exists(DirMarkName).

% ---------------------------------------------------------------------------

depend_p(srctype(_), step_file, srctype(_)).
builtin_p(srctype(SrcType), Env) :- atom(SrcType), !,
	% file type is SrcType
	env_get(srctype(SrcType0), Env),
	SrcType = SrcType0.

eval_env(srctype(SrcType), PrevEnv, step_file) :-
	env_get(filename(FileName), PrevEnv),
	get_file_srctype(FileName, SrcType).

% ===========================================================================

:- doc(section, "Common operations on source trees").

% ---------------------------------------------------------------------------

:- doc(subsection, "Enumerate source files").

:- export(current_file_find/3).
:- pred current_file_find(+Filter, +BaseDir, -FileName)
	:: source_filter * atm * atm
   # "Enumerates recursively all files @var{FileName} (absolute file
      name) in @var{BaseDir} directory files that match the
      corresponding @var{Filter}.".

current_file_find(Filter, BaseDir, FileName) :-
	walk(Filter, BaseDir, enum(FileName)).

% Enumerate files
action_hook(enum(FileName0), Event, Env) :-
	( Event = file ->
	    env_get(filename(FileName), Env),
	    FileName0 = FileName
	; fail
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Copy source trees (for installation and distribution)").

:- doc(bug, "Implement a copy that preserves original modes, perms,
   and owner? (like @tt{cp -p})").

:- export(copy_file_tree/4).
:- pred copy_file_tree(Filter, SrcDir, DestDir, Perms) 
   # "Copy the file tree from @var{SrcDir} to @var{DestDir} with
     permissions @var{Perms}".

copy_file_tree(Filter, SrcDir, DestDir, Perms) :-
	foreach_file_find(Filter, SrcDir, copy(DestDir, Perms, _)).

:- export(copy_file_tree/5).
:- pred copy_file_tree(Filter, SrcDir, DestDir, Perms, Owner) 
   # "Copy the file tree from @var{SrcDir} to @var{DestDir} with
     permissions @var{Perms} and owner @var{Owner}".

copy_file_tree(Filter, SrcDir, DestDir, Perms, Owner) :-
	foreach_file_find(Filter, SrcDir, copy(DestDir, Perms, Owner)).

:- use_module(library(system),
	[copy_file/2, copy_file/3]).
:- use_module(library(system_extra),
	[mkpath/1, mkpath/2, mkpath/3,
	 set_file_perms/2, set_file_owner/2]).

% Install files and directories
action_hook(copy(DestDir, Perms, Owner), Event, Env) :-
	env_get(basedir(BaseDir), Env),
	env_get(filename(FileName), Env),
	( Event = file ->
	    path_relocate(BaseDir, DestDir, FileName, TargetFile),
	    copy_file_perms(FileName, TargetFile,
	                    [overwrite, timestamp], Perms, Owner)
	; Event = enter ->
	    path_relocate(BaseDir, DestDir, FileName, TargetDir),
	    copy_mkpath(TargetDir, Perms, Owner)
	; fail
	).

% copy_mkpath(+TargetDir, +Perms, +Owner):
%   mkpath with given file (optional) attributes
copy_mkpath(TargetDir, Perms, Owner) :-
	( var(Perms), var(Owner) ->
	    mkpath(TargetDir)
	; var(Owner) ->
	    mkpath(TargetDir, Perms)
	; mkpath(TargetDir, Perms, Owner)
	).

% copy_file with given file (optional) attributes
copy_file_perms(FileName, TargetFile, CopyOptions, Perms, Owner) :-
	copy_file(FileName, TargetFile, CopyOptions),
	( var(Perms) -> true
	; set_file_perms(FileName, Perms)
	),
	( var(Owner) -> true
	; set_file_owner(TargetFile, Owner)
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Clean source trees").

:- use_module(library(system), [delete_file/1]).

:- export(clean_file_tree/2).
:- pred clean_file_tree(PrecompLevel, Dir)
   # "Unwind the precompilation level at directory @var{Dir}, cleaning
      the contents recursively (see @tt{untainted} and
      @tt{cleanable(PrecompLevel)} @pred{source_filter/1}).".

clean_file_tree(PrecompLevel, Dir) :-
	foreach_file_find([untainted, cleanable(PrecompLevel)], Dir, delete).

% Delete files
action_hook(delete, Event, Env) :-
	env_get(filename(FileName), Env),
	( Event = file ->
	    delete_file(FileName)
	; fail
	).

% ===========================================================================

:- doc(section, "Other copying and cleaning predicates").

:- use_module(library(system), [delete_file/1, delete_directory/1]).

:- export(copy_file_or_dir/2).
:- pred copy_file_or_dir(FileName, DestDir) # "Copy @var{FileName}
   file into @var{DestDir} directory. The path for @var{DestDir} is
   created if it does not exists. If @var{FileName} is a directory,
   all its contents are copied recursively.".

copy_file_or_dir(FileName, DestDir) :-
	( is_dir_nolink(FileName) ->
	    path_split(FileName, _Dir, Name), % TODO: use path_split
	    path_concat(DestDir, Name, T1),
	    mkpath(T1),
	    %
	    foreach_file_find(true, FileName, copy(T1, _Perms, _Owner))
	; copy_file(FileName, DestDir, [overwrite, timestamp])
	).

:- export(remove_dir/1).
:- pred remove_dir(Dir) # "Delete the directory @var{Dir} and all its
   contents recursively.".

remove_dir(Dir) :-
	foreach_file_find(true, Dir, remove).

% Delete files and directories
action_hook(remove, Event, Env) :-
	env_get(filename(FileName), Env),
	( Event = file ->
	    delete_file(FileName)
	; Event = exit ->
	    delete_directory(FileName)
	; fail
	).

:- export(remove_file_or_dir/1).
:- pred remove_file_or_dir(FileName) # "Delete @var{FileName}. If
   @var{FileName} is a directory, all its contents are deleted
   recursively.".

remove_file_or_dir(FileName) :-
	( is_dir_nolink(FileName) ->
	    remove_dir(FileName)
	; file_exists(FileName) ->
	    delete_file(FileName)
	; true
	).

:- export(delete_glob/2).
:- pred delete_glob(Dir, Pattern) # "Delete each file in directory
   @var{Dir} (non-recursively) that matches the glob pattern
   @var{Pattern}".

delete_glob(Dir, Pattern) :-
	foreach_glob_find(Dir, Pattern, delete_file).

:- export(remove_glob/2).
:- pred remove_glob(Dir, Pattern) # "Like @pred{delete_glob/2}, but
   uses @pred{remove_dir/1} for each matching directory in
   @var{Dir}.".

remove_glob(Dir, Pattern) :-
	foreach_glob_find(Dir, Pattern, remove_file_or_dir).

foreach_glob_find(Dir, Pattern, Action) :-
	( % (failure driven loop)
	  file_exists(Dir),
          current_file_find([nonrec,
	                     file_p-match(name, glob(Pattern)) % match pattern
                            ], Dir, FileName),
	    process_file(Action, FileName),
	    fail
	; true
	).

% TODO: Use action_hook instead
process_file(delete_file, FileName) :-
	delete_file(FileName).
process_file(remove_file_or_dir, FileName) :-
	remove_file_or_dir(FileName).

% ===========================================================================

:- doc(section, "Definition of source tree filters").

:- export(source_filter/1).
:- regtype source_filter(Filter) # "@var{Filter} is a source file filter".

:- doc(source_filter/1, "A filter for source files. A filter is a
   basic filter, an extended filter (defined on terms on other
   filters), and a list of basic or extended filters.

   Basic filters:
   @begin{description}

   @item{@tt{true}} Enumerate all files (recursively).

   @item{@tt{nonrec}} Do not recurse into directories (directories
     will be treated as files).

   @item{@tt{untainted}} Enumerate all files, except those that are
     backups or hold repository metadata.

   @item{@tt{srctype(SrcTypes)}} Enumerate source files of any of the
     specified @var{SrcTypes}, a source type or list of source types.
     A source type (@pred{file_srctype/1}) is any of types@tt{module},
     @tt{package}, or @tt{include} files (for user or included
     files).

   @item{@tt{proj(TreeProj)}} Obtain a tree projection based on marks.
     Projections allow discarding whole subtrees or individual files.
     The valid projection names and their marks are:

     @begin{description} 

     @item{@tt{compilable}} for each directory, discard the subtree if
       it contains the @tt{NOCOMPILE} file, or ignore the files whose
       name match with any of the patterns listed in the file
       @tt{NOCOMPILEFILES}.

     @item{@tt{testable}} same with @tt{NOTEST} and @tt{NOTESTFILES}
       (implies @tt{compilable}).

     @item{@tt{distributable}} same with @tt{NODISTRIBUTE} and
       @tt{NODISTRIBUTEFILES}.

     @item{@tt{installable}} same with @tt{NOINSTALL} and
       @tt{NOINSTALLFILES}.

     @end{description}

   @item{@tt{precomp(PrecompLevel)}} Enumerate files for the
     specified precompilation level @var{PrecompLevel} (see
     @regtype{precomp_level/1}).

   @item{@tt{cleanable(PrecompLevel)}}
     Enumerate the files resulting from compilation of source files
     that must be cleaned in order reduce the precompilation to
     @var{PrecompLevel}. E.g., @tt{PrecompLevel=src} cleans all
     compiler output, @tt{PrecompLevel=noa} cleans only platform
     dependant files.

   @end{description}

   Extended filters:
   @begin{description}
   @item{@tt{compilable_module}} Enumerate Ciao modules that are
     suitable for automatic compilation during bundle build (in the
     untainted @tt{proj(compilable)} projection).

   @item{@tt{testable_module}} Enumerate Ciao modules that can be
     compiled and tested (in the untainted @tt{proj(compilable)} and
     @tt{proj(testable)} projection).

   @item{@tt{distributable_precomp(PrecompLevel)}}
     Enumerate files that can be distributed (in the untainted
     @tt{proj(distributable)} projection and for the given
     precompilation level @var{PrecompLevel}).

     Additionally, excludes the build directory and some temporary
     files.

   @item{@tt{installable_precomp(PrecompLevel)}} Enumerate (source)
     files that can be installed (in the untainted
     @tt{proj(installable)} projection and for the given
     precompilation level @var{PrecompLevel}).

     Additionally, excludes the build directory and some temporary
     files.
   @end{description}").

% TODO: Given by the available definitions, find a better way to document it
:- impl_defined(source_filter/1).

% ---------------------------------------------------------------------------

:- doc(subsection, "Some simple filters").

filter_def(nonrec, [walk_p-false]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Untainted filter").

% Filter for untainted source tree
filter_def(untainted, [
    file_p-not(match(name, file(backup_or_repo))),
    dir_p-not(match(name, dir(backup_or_repo)))
]).

% Backup files
match_def(backup_or_repo, PT) := ~match_def(backup, PT).
match_def(backup_or_repo, PT) := ~match_def(repo, PT).
% Temporary and backup files and directories
match_def(backup, file) := '*.bak'.
match_def(backup, file) := '*.old'.
match_def(backup, file) := '*~'.
match_def(backup, file) := '#*'.
match_def(backup, file) := '.#*'. % TODO: not really backup
match_def(backup, dir) := 'bak'.
match_def(backup, dir) := '*.bak'.
match_def(backup, dir) := 'tmp'.
% Directories for repository metadata
match_def(repo, file) := '.gitmodules'.
match_def(repo, file) := '.gitignore'.
match_def(repo, file) := '.DS_Store'. % TODO: not repo, metadata
match_def(repo, dir) := 'CVS'.
match_def(repo, dir) := '.svn'.
match_def(repo, dir) := '.git'.

% ---------------------------------------------------------------------------

:- doc(subsection, "Mark-based tree projection filter").

filter_def(proj(TreeProj), [
    file_p-not(match(name, file(~neg_proj(TreeProj)))),
    file_p-not(or(~list_fileset_pattern(~neg_proj(TreeProj)))),
    dir_p-not(or(~list_dirmark(~neg_proj(TreeProj)))),
    file_p-not(glob_from_fileset(~list_fileset(~neg_proj(TreeProj))))
]).

neg_proj(any) := none.
neg_proj(compilable) := nocompilable.
neg_proj(installable) := noinstallable.
neg_proj(distributable) := nodistributable.
neg_proj(testable) := notestable.

list_dirmark(Kind) :=
	~findall(dirmark(Mark), dirmark(Kind, Mark)).

list_fileset(Kind) :=
	~findall(F, fileset(Kind, F)).

list_fileset_pattern(Kind) :=
	~findall(match(name, glob(F)), fileset(Kind, F)).

% Marks for not compilable files

% TODO: Avoid this complex dirmark by adding a Dirctx that introduces
%   '*' for those dirs that have Makefile and SETTINGS.pl?
dirmark(nocompilable) := 'Makefile.pl'. % Contain Makefile.pl (for lpmake)
dirmark(nocompilable) := 'SETTINGS.pl'. % Contain SETTINGS.pl (for lpdoc)
dirmark(nocompilable) := 'NOCOMPILE'.
% Marks for not testable files
dirmark(notestable) := 'NOTEST'.
% Marks for not installable files
dirmark(noinstallable) := 'NOINSTALL'.
% Marks for not distributed files
dirmark(nodistributable) := 'NODISTRIBUTE'.

fileset(nocompilable) := 'NOCOMPILEFILES'.
fileset(notestable) := 'NOTESTFILES'.
fileset(noinstallable) := 'NOINSTALLFILES'.
fileset(nodistributable) := 'NODISTRIBUTEFILES'.

match_def(nocompilable, file) := '*_co.pl'. % (result of transformation)
match_def(nocompilable, file) := 'SETTINGS.pl'.
match_def(nocompilable, file) := 'Makefile.pl'.

% ---------------------------------------------------------------------------

:- doc(subsection, "Source type filter").

filter_def(srctype(SrcTypes), [
    file_p-match(name, file(pl_source)),
    file_p-or(~findall(srctype(T),
                       ( atom(SrcTypes), T = SrcTypes
		       ; member(T, SrcTypes))))
]).

match_def(pl_source, file) := '*.pl'.

% ---------------------------------------------------------------------------

:- doc(subsection, "Precompilation-level filters").

% TODO: add filter for .c and .h files, etc.

% All files with precompilation lower or equal than PrecompLevel
% (excludes all in precomp_yield)

:- export(precomp_level/1).
:- regtype precomp_level(PrecompLevel) # "@var{PrecompLevel} is a
   pre-compilation level".

:- doc(precomp_level/1, "The valid precompilation levels are:
   @begin{description}
   @item{@tt{src}} source files only
   @item{@tt{noa}} sources and portable objects
   @item{@tt{bin}} sources, portable, and architecture dependant objects
     (except special third-party files like elisp objects and Java classes)
   @item{@tt{full}} like @tt{bin}, including special third-party
     files.
   @end{description}").

precomp_level(src).
precomp_level(noa).
precomp_level(bin).
precomp_level(full).

% (order of precomp levels)
precomp_next(src) := noa.
precomp_next(noa) := bin.
precomp_next(bin) := full.

filter_def(precomp(PrecompLevel), [
    file_p-not(match(name, file(precomp_yield(PrecompLevel))))
]).

% All files with precompilation level greater than PrecondLevel
filter_def(cleanable(PrecompLevel), [
    file_p-match(name, file(precomp_yield(PrecompLevel)))
]).

% Like precomp, but applies to builddir
filter_def(precomp_builddir(PrecompLevel), [
    dir_p-not(match(filename, filename(precomp_builddir_yield(PrecompLevel))))
]).

% Filters for temporary precompilation files (logs, errors,
% and some files needed for incremental compilation)
filter_def(no_precomp_tmp, [
    file_p-not(match(name, file(precomp_tmp))),
    walk_p-not(match(name, dir(precomp_tmp)))
]).

:- export(match_def/3).
% No architecture-dependant compilation output
match_def(noarch, file) := '*.po'.
match_def(noarch, file) := '*.itf'.
match_def(noarch, file) := '*.wam'.
match_def(noarch, file) := '*.asr'.
match_def(noarch, file) := '*.ast'.
match_def(noarch, file) := '*.testout'.
match_def(noarch, file) := '*_auto.pl'.
% Architecture-dependant compilation output
match_def(arch, file) := '*.a'.
match_def(arch, file) := '*.so'.
match_def(arch, file) := '*.dll'.
match_def(arch, file) := '*.dylib'.
% Temporary noarch_file (can be removed in binary distributions)
% TODO: hardwired files
match_def(noarch_tmp, file) := '*.out'.
match_def(noarch_tmp, file) := '*.aux'.
match_def(noarch_tmp, file) := '*.log'.
match_def(noarch_tmp, file) := '*.err'.
match_def(noarch_tmp, file) := '*.tmp'.
match_def(noarch_tmp, file) := 'tmpciao*'.
match_def(noarch_tmp, file) := '*.tmp-*'.
match_def(noarch_tmp, dir) := '*.tmp-*'.
% NOTE: don't exclude *.iss to facilitate debugging of Windows installer
% match_def(noarch_tmp, file) := '*.iss'.
match_def(noarch_tmp, file) := '*_co.pl'. % (result of transformation)
match_def(noarch_tmp, file) := '*_co.java'. % (result of transformation)
match_def(noarch_tmp, file) := '*_glue.c'.
match_def(noarch_tmp, file) := '*_inline.c'.
match_def(noarch_tmp, file) := '*.texic'.
match_def(noarch_tmp, file) := '*.refs'.
match_def(noarch_tmp, file) := 'auto*.eps'.
match_def(noarch_tmp, file) := 'auto*.txt'.
match_def(noarch_tmp, file) := 'auto*.ppm'.
match_def(noarch_tmp, file) := '*.src'.
match_def(noarch_tmp, file) := '*refs.el'.
match_def(noarch_tmp, file) := '*refs.aux'.
match_def(noarch_tmp, file) := '*refs.blg'.
match_def(noarch_tmp, file) := '*_wrapper_auto.*'.
% Temporary arch_file (can be removed in binary distributions)
match_def(arch_tmp, file) := '*.o'.
% TODO: really exclude in binary form?
match_def(javac_output, file) := '*.class'.
% (emacs-mode)
% Note:  *.elc excluded (depend on a specific emacs version)
match_def(elisp_output, file) := 'ciao-mode-init.el'. % TODO: really exclude?
match_def(elisp_output, file) := 'ciao-config.el'. % TODO: really exclude?
match_def(elisp_output, file) := '*.elc'.
%
% precomp_yield(Level): expected in a greater precompilation level
match_def(precomp_yield(Level), PT) :=
	~match_def(precomp_yield(~precomp_next(Level)), PT).
match_def(precomp_yield(src), PT) := ~match_def(noarch, PT).
match_def(precomp_yield(src), PT) := ~match_def(noarch_tmp, PT).%%
match_def(precomp_yield(noa), PT) := ~match_def(arch, PT).
match_def(precomp_yield(noa), PT) := ~match_def(arch_tmp, PT).%%
match_def(precomp_yield(bin), PT) := ~match_def(elisp_output, PT). % TODO: strange
match_def(precomp_yield(bin), PT) := ~match_def(javac_output, PT). % TODO: strange
% match_def(precomp_yield(full), _PT) := _ :- fail.
% temporary for compilation/build (at any level)
match_def(precomp_tmp, PT) := ~match_def(noarch_tmp, PT).
match_def(precomp_tmp, PT) := ~match_def(arch_tmp, PT).
%
match_def(precomp_builddir_yield(Level), PT) :=
	~match_def(precomp_builddir_yield(~precomp_next(Level)), PT).
match_def(precomp_builddir_yield(Level), filename) :=
	~precomp_builddir_yield_(Level).

:- use_module(library(bundle/paths_extra), [fsR/2]).

precomp_builddir_yield_(src) := ~fsR(builddir(~builddir_id)/doc). % src level produces doc/
precomp_builddir_yield_(noa) := ~fsR(builddir(~builddir_id)/eng). % noa level produces eng/
precomp_builddir_yield_(noa) := ~fsR(builddir(~builddir_id)/bin). % noa level produces bin/
% TODO: add a level to include third-party source?
% (see third_party_install.pl)
precomp_builddir_yield_(bin) := ~fsR(bundle_src(ciao)/'third-party'). % bin level produces third-party/
% precomp_builddir_yield_(full) := _ :- fail. % full level produces nothing (more)

:- use_module(ciaobld(config_common), [local_bldid/1]).

builddir_id := ~local_bldid.
builddir_id := bootbuild.

% ---------------------------------------------------------------------------

:- doc(subsection, "Extended filter definitions").

filter_def(compilable_module,
	[untainted, proj(compilable), srctype(module)]).

filter_def(testable_module,
	[untainted, proj(compilable), proj(testable), srctype(module)]).

filter_def(distributable_precomp(Level),
	[untainted, proj(distributable),
	 precomp(Level),
	 precomp_builddir(Level),
	 no_precomp_tmp]).

filter_def(installable_precomp(Level),
	[untainted, proj(installable),
	 precomp(Level),
	 precomp_builddir(Level),
	 no_precomp_tmp]).

% ===========================================================================

:- doc(section, "Auxiliary predicates").
% TODO: move somewhere else?

:- use_module(library(file_utils), [file_to_string/2]).

file_to_atoms(File, Atoms) :-
	file_to_string(File, String),
	strings_atoms(String, Atoms).

strings_atoms("", []) :-
	!.
strings_atoms(String, [Atom|Atoms]) :-
	append(Line, [0'\n|Tail], String),
	atom_codes(Atom, Line),
	!,
	strings_atoms(Tail, Atoms).
strings_atoms(Line, [Atom]) :-
	atom_codes(Atom, Line).

% ----------------------------------------------------------------------------

:- use_module(library(read), [read/2]).

:- export(file_srctype/1).
:- regtype file_srctype(T) # "@var{T} is a source file type".
:- doc(file_srctype/1, "Source file types, defined as:
   @includedef{file_srctype/1}").

file_srctype(module).
file_srctype(package).
file_srctype(include).

:- export(get_file_srctype/2).
:- pred get_file_srctype(+FileName, ?Type) :: atm * file_srctype
   # "@var{FileName} is a file of type @var{Type} (@tt{module},
     @tt{package}, or @tt{include} for included or user
     sources). Fails if the file does not seem Ciao code".

:- doc(get_file_srctype/2, "This operation should be relatively fast,
   since we only need to read the first term in the file. Modules
   start with a @tt{:- module/3} (or @tt{:- module/2}) or @tt{:-
   package/1} directive.").

get_file_srctype(FileName, Type) :-
	% TODO: safety check (read/2 runs out of memory on wrong files)
	atom_concat(_, '.pl', FileName),
	%
	read_first_term(FileName, Term),
	( var(Term) -> fail
	; ( Term = (:- module(_, _, _))
	  ; Term = (:- module(_, _))
	  ) ->
	    Type = module
	; Term = (:- package(_)) ->
	    Type = package
	; Type = include
	).

% Read the first term (leave unbound on failure or exception)
read_first_term(File, Term) :-
	catch(open(File, read, Stream), _, fail), % (it may be a broken symlink)
	( catch(read(Stream, Term), _, true) ->
	    true
	; true
	),
	close(Stream).

% ===========================================================================
%
% APPENDIX. General AND/OR composition of filters
%
% We can see 'walk' as a recursion operator (similar to * in regular
% expressions). In regexp, clearly the following equivalence for AND
% composition follows:
%
%   (A*) & (B*) is (A&B)*
%
% but not the OR composition:
%
%   (A*) | (B*) is not (A|B)* 
%
% The walk algorithm can be simplified as follows:
% 
%   walk(W,D,F,X,Z) :-    W(X), D(X), child(X,Y), walk(W,D,F,Y,Z).
%   walk(W,D,F,X,Z) :- \+ W(X), F(X), Z=X.
% 
% which can be further simplified by introducing W and child into 
% the conditions:
%
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% Our task is to simplify the following predicates:
%
%   walkO(D1,F1,D2,F2,X,Z) :- walk(D1,F1,X,Z).
%   walkO(D1,F1,D2,F2,X,Z) :- walk(D2,F2,X,Z).
%
%   walkA(D1,F1,D2,F2,X,Z) :- walk(D1,F1,X,Z), walk(D2,F2,X,Z).
%
% Expansion for "walkO":
% {step1}
%
%   walkO(D1,F1,D2,F2,X,Z) :- walk(D1,F1,X,Z).
%   walkO(D1,F1,D2,F2,X,Z) :- walk(D2,F2,X,Z).
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% {step2: unfold}
%
%   walkO(D1,F1,D2,F2,X,Z) :- D1(X,Y), walk(D1,F1,Y,Z).
%   walkO(D1,F1,D2,F2,X,X) :- F1(X).
%   walkO(D1,F1,D2,F2,X,Z) :- D2(X,Y), walk(D2,F2,Y,Z).
%   walkO(D1,F1,D2,F2,X,X) :- F2(X).
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% {step2: reorder}
%
%   walkO(D1,F1,D2,F2,X,Z) :-
%       (D1(X,Y),Dy=D1,Fy=F1
%       ;D2(X,Y),Dy=D2,Fy=F2),
%       walk(Dy,Fy,Y,Z).
%   walkO(D1,F1,D2,F2,X,X) :- (F1(X);F2(X)).
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% {SOLUTION: We can compose D and F, but D needs an extra argument
%   that selects the next filter}
% 
% Expansion for "walkA":
% {step1}
%
%   walkA(D1,F1,D2,F2,X,Z) :- walk(D1,F1,X,Z), walk(D2,F2,X,Z).
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% {step2: unfold}
%
%   walkA(D1,F1,D2,F2,X,Z) :- 
%       D1(X,Y), D2(X,Y), walk(D1,F1,Y,Z), walk(D2,F2,Y,Z).
%   walkA(D1,F1,D2,F2,X,X) :- F1(X), D2(X,X), walk(D2,F2,X,X).
%   walkA(D1,F1,D2,F2,X,X) :- D1(X,X), walk(D1,F1,X,X), F2(X,X).
%   walkA(D1,F1,D2,F2,X,X) :- F1(X), F2(X).
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% {step3: simplify, since D and F are mutually exclusive on same X,
%   since X cannot be its own child}
%
%   walkA(D1,F1,D2,F2,X,Z) :- 
%       D1(X,Y), D2(X,Y), walk(D1,F1,Y,Z), walk(D2,F2,Y,Z).
%   walkA(D1,F1,D2,F2,X,X) :- F1(X), F2(X).
%   walk(D,F,X,Z) :- D(X,Y), walk(D,F,Y,Z).
%   walk(D,F,X,X) :- F(X).
%
% {step4: fold walkA}
%
%   walkA(D1,F1,D2,F2,X,Z) :- 
%       (D1(X,Y), D2(X,Y)), walkA(D1,D2,F1,F2,Y,Z).
%   walkA(D1,F1,D2,F2,X,X) :- (F1(X), F2(X)).
%
% {SOLUTION: We can compose D and F for AND composition.  The
%   composition is more complex when W is not introduced in D and
%   F. We need a positive W (pW) and negative W (nW). Both are AND
%   composed.}
%
% ===========================================================================

