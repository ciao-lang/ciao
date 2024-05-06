:- module(unittest_db, [], [hiord, datafacts]).

% This module deals with everything related to the test database
% shared between the driver, runner and other parts of unittest. It
% defines the relevant data as well as the predicates to read/write it
% from/to files for sharing with the runner or for regression.
% 
% Important pieces of data:
% 
%  - List of modules under test
%  - List of tests and tests attributes
%  - List of tests results
% 
% This data needs to be shared and stored as files for the following:
% 
%  - Sharing between driver and test runner.
% 
%    The test driver reads and asserts the test attributes, but the
%    test runner, which runs in another process, also needs them.
% 
%    The test runner produces the test results, but the test driver
%    is the one that shows and stores them.
% 
%  - Saving it for regression, so we can compare saved results against
%    new results.
% 
% As a result, we have the following data:
% 
%  - module_base_path_db/3: The modules being tested, with their base
%    and absolute paths.
% 
%  - test_db/n: The tests and their attributes for the
%    current unittest run. It might be a subset of all the tests
%    available in the modules under test, when filter options are
%    used.
% 
%  - test_output_db/2: The result for each solution generated for the
%    goals under test.
% 
% Which are shared in the following files:
% 
%  - module.testout: File that stores test results.
% 
%  - module.testout-saved: File that stores saved test results for
%    regression.
% 
%  - module.testin: File that saves the test attributes. It always
%    contains all the tests in a module, even if filter options are
%    used.
% 
%  - module.testin-saved: Saved version of .testin file for
%    regression.
% 
%  - <tmp_dir>/test_input_auto: shares test attributes between driver
%    and runner.
% 
%  - <tmp_dir>/test_output_oauto: shares test results between runner
%    and driver.

:- use_module(library(streams), [open/3, close/1, absolute_file_name/7]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(engine(internals), [opt_suff/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).

% ---------------------------------------------------------------------------
%! # Database

% Modules under test
:- export(module_base_path_db/3).
:- data module_base_path_db/3.
% Tests
:- export(test_db/8).
:- data test_db/8.
% Test results
:- export(test_output_db/2).
:- data test_output_db/2.
% Tests for runner
:- export(runtest_db/4).
:- data runtest_db/4.

:- export(cleanup_modules_under_test/0).
cleanup_modules_under_test :-
    retractall_fact(module_base_path_db(_,_,_)).

:- export(cleanup_test_db/0).
cleanup_test_db :-
    retractall_fact(test_db(_, _, _, _, _, _, _, _)).

%:- export(assert_test_db/1).
assert_test_db(test_db(TestId, Module, C, D, E, F, G, H)) :-
    assertz_fact(test_db(TestId, Module, C, D, E, F, G, H)).

:- export(filtered_test_db/9).
filtered_test_db(Filter, TestId, Module, C, D, E, F, G, H) :-
    test_db(TestId, Module, C, D, E, F, G, H),
    run_filter(Filter, test_db(TestId, Module, C, D, E, F, G, H)).

:- multifile test_filter/2.
:- use_module(library(unittest/unittest_filters), []). % to import multifile test_filter/2

run_filter([],_).
run_filter([Filter|Filters], Test) :-
    test_filter(Filter, Test), !,
    run_filter(Filters, Test).

:- export(cleanup_test_results/0).
cleanup_test_results :-
    retractall_fact(test_output_db(_, _)).

% ---------------------------------------------------------------------------
%! # Persistent state

:- use_module(library(system), [mktemp_in_tmp/2, delete_file/1]).

:- data tmp_dir/1.

:- export(get_test_tmp_dir/1).
get_test_tmp_dir(TmpDir) :-
    tmp_dir(TmpDir0), !,
    TmpDir = TmpDir0.
get_test_tmp_dir(TmpDir) :-
    mktemp_in_tmp('ciaotestXXXXXX', TmpDir),
    delete_file(TmpDir),
    retractall_fact(tmp_dir(_)),
    assertz_fact(tmp_dir(TmpDir)).

% ---------------------------------------------------------------------------
% file suffixes

file_test_input_suffix(new, '.testin').
file_test_input_suffix(saved, '.testin-saved').
file_test_output_suffix(new, '.testout').
file_test_output_suffix(saved, '.testout-saved').

% test input

:- export(file_test_input/3).
file_test_input(Module, Vers, File) :-
    file_test_input_suffix(Vers, Suffix),
    module_base_path_db(Module,Base,_),
    atom_concat(Base,Suffix,File).

:- export(load_test_input/2).
load_test_input(Module, Vers) :-
    file_test_input(Module, Vers, File),
    assert_from_file(File, assert_test_db).

:- export(remove_test_input/2).
remove_test_input(Module, Vers) :-
    file_test_input(Module, Vers, File),
    del_file_nofail(File).

% .testout

:- export(file_test_output/3).
file_test_output(Module, Vers, File) :-
    file_test_output_suffix(Vers,Suffix),
    module_base_path_db(Module,Base,_),
    atom_concat(Base,Suffix,File).

:- export(load_test_output/2).
load_test_output(Module,Vers) :-
    file_test_output(Module,Vers,File),
    assert_from_file(File, assert_test_output).

assert_test_output(test_output_db(A, B)) :-
    assertz_fact(test_output_db(A, B)).

% runtest input (file from passing test inputs from driver to runner)
file_runtest_input_name('test_input_auto.pl').

:- export(file_runtest_input/2).
file_runtest_input(TestRunDir, File) :-
    file_runtest_input_name(File0),
    path_concat(TestRunDir, File0, File).

:- export(load_runtest_input/1).
load_runtest_input(TestRunDir) :-
    retractall_fact(runtest_db(_, _, _, _)),
    file_runtest_input(TestRunDir, File),
    assert_from_file(File, assert_runtest_db).

% (assert into runtest_db/4)
assert_runtest_db(test_db(TestId, Module, _, _, _, F, G, _)) :-
    assertz_fact(runtest_db(TestId, Module, F, G)).

:- export(store_runtest_input/3).
% save only filtered entries
store_runtest_input(TestRunDir, Module, Filter) :-
    file_runtest_input(TestRunDir, File),
    open(File, write, Stream),
    ( % (failure-driven loop)
      Term = test_db(TestId, Module, C, D, E, F, G, H),
      filtered_test_db(Filter, TestId, Module, C, D, E, F, G, H),
        write_data(Stream, Term),
        fail
    ; true
    ),
    close(Stream).

:- meta_predicate data_to_file(addterm(goal),?,?).
% TODO: unify serialization in Ciao
data_to_file(Data, Term, File, Mode) :-
    open(File, Mode, Stream),
    ( % (failure-driven loop)
      current_fact(Data), % TODO: use a get_data/1 predicate, with one clause for each data
        write_data(Stream, Term),
        fail
    ; true
    ),
    close(Stream).

% ---------------------------------------------------------------------------

:- export(file_runtest_redirect/3).
file_runtest_redirect(Std, TestRunDir, AbsFile) :-
    file_runtest_redirect_(Std, File),
    path_concat(TestRunDir, File, AbsFile).

file_runtest_redirect_(stdout, 'stdout_redirected').
file_runtest_redirect_(stderr, 'stderr_redirected').

% ---------------------------------------------------------------------------

:- export(wrapper_file_name/3).
wrapper_file_name(TestRunDir, Module, WrapperFile) :-
    atom_concat(Module,'_wrp_auto.pl',WrpModule),
    path_concat(TestRunDir,WrpModule,WrapperFile).

% ---------------------------------------------------------------------------

:- meta_predicate assert_from_file(?, pred(1)).
assert_from_file(File, AssertMethod) :-
    open(File, read, SI),
    repeat,
    ( read_data(SI, Term) ->
        AssertMethod(Term),
        fail
    ; !,
      close(SI)
    ).

% ---------------------------------------------------------------------------
%! # Test data serialization

%% The commented out line can be used to save data in text mode and
%% facilitate debugging --EMM
:- compilation_fact(pretty_testout).

:- if(defined(pretty_testout)).

:- use_module(library(read), [read/2]).
:- use_module(engine(io_basic), [displayq/2, display/2]).

:- export(read_data/2).
read_data(SI, Term) :-
    read(SI, Term),
    Term \== end_of_file.

:- export(write_data/2).
write_data(SI, Term) :-
    displayq(SI, Term), display(SI, ' .\n').

:- else.

:- use_module(library(fastrw), [fast_read/2, fast_write/2]).

:- export(read_data/2).
read_data(SI, Term) :- fast_read(SI, Term).

:- export(write_data/2).
write_data(SI, Term) :- fast_write(SI, Term).

:- endif.

