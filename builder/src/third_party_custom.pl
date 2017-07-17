:- module(third_party_custom, [], [assertions, basicmodes, fsyntax, hiord]).

:- doc(title, "Installation of custom third-party components").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module implements installation of third-party
   components using custom installers (e.g., manual, @tt{npm},
   etc.)").

:- multifile m_bundle_foreign_dep/4.

% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(library(format), [format/3]).
%
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(system), [find_executable/2, file_exists/1, working_directory/2, cd/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(port_reify)).
%
:- use_module(ciaobld(third_party_install), [third_party_path/2]).

:- export(third_party_custom_install/1).
% Other installation methods for third party code
third_party_custom_install(Bundle) :-
	check_foreign_cmd(Bundle),
	findall(X, m_bundle_foreign_dep(Bundle, npm, X, _), As),
	findall(X, m_bundle_foreign_dep(Bundle, bower, X, _), Bs),
	install_npm_deps(As),
	install_bower_deps(Bs).

% Check that foreign commands are available (where no automatic
% installation is available)
check_foreign_cmd(Bundle) :-
	( % (failure-driven loop)
	  m_bundle_foreign_dep(Bundle, cmd, Cmd, Desc),
	    ( find_executable(Cmd, _) ->
	        true
	    ; format(user_error, "ERROR: Missing '~w'. Required: ~w~n", [Cmd, Desc]),
	      throw(missing_dep)
	    ),
	    fail
	; true
	).

install_npm_deps(Deps) :-
	normal_message("installing third-party dependencies via npm", []),
	third_party_path(prefix, ThirdParty), % TODO: add bundle to third_party_path/2
	ExtDir = ~path_concat(ThirdParty, '3rd-npm'),
	mkpath(ExtDir),
	working_directory(Old, ExtDir),
	once_port_reify(install_npm_deps_(Deps), Port),
	cd(Old),
	port_call(Port).

install_npm_deps_(Deps) :-
	( file_exists('package.json') -> true
	; process_call(path(npm), [init, '-f'], [stdin(string(""))]) % create if missing
	),
	process_call(path(npm), [install, '--save'|Deps], []).

install_bower_deps(Deps) :-
	normal_message("installing third-party dependencies via bower", []),
	third_party_path(prefix, ThirdParty), % TODO: add bundle to third_party_path/2
	ExtDir = ~path_concat(ThirdParty, '3rd-bower'),
	mkpath(ExtDir),
	process_call(path(bower),
	             ['--allow-root',
		      '--config.interactive=false',
		      ~atom_concat('--config.directory=', ExtDir),
		      install|Deps], []).

:- export(third_party_custom_path/2).
third_party_custom_path(node_modules, Path) :-
	third_party_path(prefix, ThirdParty), % TODO: add bundle to third_party_path/2?
	Path = ~path_concat(ThirdParty, '3rd-npm/node_modules').
third_party_custom_path(bower_components, Path) :-
	third_party_path(prefix, ThirdParty), % TODO: add bundle to third_party_path/2?
	Path = ~path_concat(ThirdParty, '3rd-bower').
