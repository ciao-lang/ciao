:- module(unittest_runner_aux,
        [ assert_test_id/1,
          get_active_test/2,
          process_test_args/1,
          testing/4
        ],
        [assertions]).

:- doc(title,"Testing support lib (runner)").

:- doc(author, "Edison Mera").

% TODO: postcondition failure treating?
% TODO: how rtchecks deals with exceptions in pre/postconditins
% TODO: move (parts of) this lib into rtchecks?

% ----------------------------------------------------------------------
:- use_module(library(compiler),   [use_module/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(pathnames),  [path_concat/3]).
:- use_module(library(rtchecks/rtchecks_utils),
        [
            load_rtchecks/1,
            save_rtchecks/1
        ]).
:- use_module(library(unittest/unittest_base),
        [
            write_data/2,
            file_test_output/1
        ]).
% ----------------------------------------------------------------------

:- data test_id_db/2.
:- data skip_tests_before/1.
:- data active_test/0.

% ----------------------------------------------------------------------

assert_test_id(test_input_db(ARef, Mod)) :-
	assertz_fact(test_id_db(ARef, Mod)).

get_active_test(ARef, Mod) :-
        (\+ skip_tests_before(_)), !,
         test_id_db(ARef, Mod).
get_active_test(ARef, Mod) :-
        retractall_fact(active_test),
        test_id_db(ARef0, Mod0),
        (  skip_tests_before(ARef0)
        -> assertz_fact(active_test),
           fail
        ; true ),
        active_test,
        ARef = ARef0,
        Mod  = Mod0.

process_test_args([]).
process_test_args([resume_after, ARef|Args]) :- !,
	retractall_fact(skip_tests_before(_)),
        assertz_fact(skip_tests_before(ARef)),
	process_test_args(Args).
process_test_args([load, Module|Args]) :-
	use_module(Module),
	process_test_args(Args).

% ----------------------------------------------------------------------

:- meta_predicate testing(?, ?, goal, goal).
testing(ARef, TmpDir, Precond, Pred) :-
	file_test_output(BOut),
        path_concat(TmpDir,BOut,Out),
	testing_internal(Precond, Pred, Status),
	open(Out, append, IO),
	write_data(IO, test_output_db(ARef, Status)),
	close(IO).

:- data signals_db/1.

:- meta_predicate testing_internal(goal, goal, ?).
testing_internal(Precond, Pred, st(RTCErrors, Signals, Result)) :-
	retractall_fact(signals_db(_)),
	intercept(exec_test(Precond, Pred, Result),
	    E, assertz_fact(signals_db(E))),
	findall(E, retract_fact(signals_db(E)), Signals),
	load_rtchecks(RTCErrors).

:- meta_predicate exec_test(goal, goal, ?).
exec_test(Precond, Pred, Result) :-
	test_precondition_exception(
	    test_precondition(Precond,
		test_pred_exception(
		    test_postcondition(
			save_rtchecks(
			    test_result(Pred,
				Result)), Result), Result), Result), Result).

:- meta_predicate test_result(goal, ?).
test_result(Pred, Result) :-
	if(Pred, Result = true, Result = fail(predicate)).

:- meta_predicate test_postcondition(goal, ?).
test_postcondition(Pred, Result) :-
	catch(Pred, postcondition(PostEx),
	    (Result = exception(postcondition, PostEx))).

:- meta_predicate test_pred_exception(goal, ?).
test_pred_exception(Pred, Result) :-
	catch(Pred, PredEx, (Result = exception(predicate, PredEx))).

:- meta_predicate test_precondition(goal, goal, ?).
test_precondition(Precond, Pred, Result) :-
	Precond -> Pred ; Result = fail(precondition).

:- meta_predicate test_precondition_exception(goal, ?).
test_precondition_exception(Pred, Result) :-
	catch(Pred, PrecEx, (Result = exception(precondition, PrecEx))).

