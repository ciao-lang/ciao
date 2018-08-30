:- module(datadir, [], [assertions, basicmodes, fsyntax]).

:- doc(title, "Manager for persistent data directories").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module abstracts the location of directories for
   storing persistent data.

   The directory for persistent data will be placed under
   @pred{deploy_data_root_dir/1} (multifile) or inside the temporary
   build directory (see @pred{config_common:data_root_dir/1}.

   See @pred{ensure_datadir/2} for usage. This module is handy to
   select the directory for @lib{persdb} databases, e.g.:
@begin{verbatim}
persistent_dir(db, Dir) :- ensure_datadir('yourapp', Dir).
@end{verbatim}
").

% ---------------------------------------------------------------------------

:- use_module(engine(data_facts)).
:- use_module(library(pathnames), [path_concat/3, path_is_relative/1]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system), [get_home/1]).
:- use_module(library(system_extra), [mkpath/1]).

:- use_module(ciaobld(config_common), [data_root_dir/1]).

:- doc(deploy_data_root_dir/1, "Directory for persistent data on
   deployment. If this is a relative path, it will be placed under the
   @tt{HOME} directory automatically.").

:- multifile deploy_data_root_dir/1.

:- data curr_datadir/2. % (memoize ensure_datadir/2)

:- export(ensure_datadir/2).
:- pred ensure_datadir(+RelPath, -Path) :: atm * atm # "Obtain the
   absolute path @var{Path} for storing persistent data under relative
   @var{RelPath} directory. The directory is created if needed.".

ensure_datadir(RelPath, Path) :-
	( curr_datadir(RelPath, Path0) -> true
	; get_datadir(RelPath, Path0),
	  mkpath(Path0),
	  assertz_fact(curr_datadir(RelPath, Path0))
	),
	Path = Path0.

% Directory for storing persistent data (with relative path RelPath)
get_datadir(RelPath, D) :-
	% Use the global download datadir
	deploy_data_root_dir(D0),
	( path_is_relative(D0) ->
	    % (relative to home if it is a relative path) % TODO: I am not
	    % sure if this is a good idea
	    Home = ~get_home,
	    path_concat(Home, D0, D1)
	; D0 = D1
	),
	file_exists(D1),
	!,
        D = ~path_concat(D1, RelPath).
get_datadir(RelPath, D) :-
	% Otherwise, use a local data path
	data_root_dir(Dir0),
        D = ~path_concat(Dir0, RelPath).
