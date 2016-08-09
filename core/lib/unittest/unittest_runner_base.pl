% Included from each instance of the unittest runner (see unittest.pl)

:- use_module(library(unittest/unittest_runner_aux)).
:- use_module(library(unittest/unittest_utils)).
:- use_module(library(unittest/unittest_base)).

% stop_on_first_error(false).
main_tests(A) :-
        process_runner_args(A),
        file_test_input_path(FileTestInput),
        assert_from_file(FileTestInput,assert_test_id),
        runtests.

runtests :-
        ( % (failure-driven loop)
          get_active_test(TestId, Module),
          % TODO: multiple test results bug
          % TODO: use data predicate to store the testing
          %       status of the predicate, whether some
          %       input failed (thus no testing to be
          %       continued), or no
          % TODO: requires splitting runtests/0 into 2
          %       preds with 2 failure-driven loops,
          %       one for TestIds and another for all
          %       results for a chosen TestId
          internal_runtest_module(Module, TestId),
          fail
        ; true
        ).
