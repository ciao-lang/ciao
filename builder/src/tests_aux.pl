:- module(tests_aux, [], [assertions, fsyntax, dcg]).

:- doc(title, "Auxiliary for tests_aux.bash").
:- doc(author, "Jose F. Morales").

% TODO: Do not use bash

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_split/3]).

:- export(bundle_test/2).
% Perform the bundle tests
bundle_test(BundleDir, Action) :- !,
    ( test_action(Action) ->
        true
    ; throw(error(['Unknown test action ', Action]))
    ),
    exec_aux(BundleDir, [Action]).

test_action(check).
test_action(compare).
test_action(briefcompare).
test_action(save).
test_action(bench).

exec_aux(BundleDir, Args) :-
    path_split(BundleDir, _, BundleName),
    exec_aux_(BundleDir, BundleName, Args).
    
exec_aux_(BundleDir, BundleName, Args) :-
    Env = ['CURR_BUNDLEDIR' = BundleDir,
           'CURR_BUNDLENAME' = BundleName],
    bundle_path(builder, 'src/tests_aux.bash', Exec),
    process_call(Exec, Args, 
                 [env(Env), cwd(BundleDir)]).






