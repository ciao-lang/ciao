:- module(test_examples,
    [
        p/1,
        display1/1,
        call_test10/1,
        cut_test5/0,
        display_fail/0
    ],
    [assertions, nativeprops, basicmodes, rtchecks, unittestdecls, hiord]).

:- doc(author, "Edison Mera").
:- doc(author, "Nataliia Stulova").

:- doc(module, "Some examples of unit tests.").

:- use_module(engine(io_basic)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------------------------------------
% tests only with preconditions

:- test p(A) : (A = c) %+ (possible_exceptions([_]))
    # "test should pass".
:- test p(A) : (A = d, A = a)
    # "test should fail (precondition)".
:- test p(A) : (A = exception, throw(A))
    # "test should fail (throws exception in precondition)".
:- test p(A) : (A = h)
    # "test should abort".

% ----------------------------------------------------------------------
% tests with preconditions and computational properties

:- test p(A) : (A = a) + not_fails
    # "test should pass".
:- test p(A) : (A = b) + fails
    # "test should pass".
:- test p(A) : (A = c) + exception(error(c, _))
    # "test should pass".
:- test p(A) : (A = c) + (fails, no_exception)
    # "test should fail".
:- test p(A) : (A = c) + not_fails
    # "test should fail".

% ----------------------------------------------------------------------
% tests with preconditions and postonditions

:- test p(Z) => (Z = d).

% ----------------------------------------------------------------------
% assertions that can be added to all unit tests for p/1 with the
% 'rtc_entry' unittest option
:- check comp p(X) : (X = a) + not_fails.
% :- check comp p(X) : (X = b) + not_fails.

p(a).                               % rule succeeds
p(b) :- fail.                       % rule fails
p(c) :- throw(error(c, 'error c')). % rule throws an exception
p(h) :- halt(1).                    % rule aborts the execution (note: cannot be used with 'sameproc' option)

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------------------------------------
% this set of tests demonstrates the correct and incorrect uses of texec
% assertions, it only gives examples of passing tests
%
:- texec display1(A) : (A = hello)
    # "correct texecassertion, test should pass".
:- texec display1(A) : (A = hello) + user_output("hello")
    # "incorrect texec assertion, test should pass".
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% examples of the alternatives for the texec assertions above
%
:- test display1(A) : (A = hello)
    # "test should pass".
:- test display1(A) : (A = hello) + user_output("bye")
    # "test should fail".
% ----------------------------------------------------------------------

display1(A) :- display(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- test display_fail + (user_output("hello"), fails) # "Test OK".

display_fail :- display(hello), fail.

:- pred display_fail(A) + (user_output("hello2"), fails).
:- export(display_fail/1).

display_fail(_A) :- display(hello), fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(write), [write/1]).

:- test cut_test5 + (user_output("Cut disjunction"), fails) #
    "Test OK".

cut_test5 :- (! ; write('No')), write('Cut disjunction'), fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- test call_test10(X) : (X=(write(3), call(1)))
    + ( user_output("output"),
        exception(error(type_error(callable, 1), 'in metacall')) )
# "Wrong test".

:- meta_predicate call_test10(goal).

call_test10(X) :- call(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export(qs/2).
:- pred qs(+list(int), -list(int)) + is_det.

qs(A, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tests below cover most of scenarios of the unit test expansion
%% (missing: tests that take into account different values of prolog flags
%%  rtchecks_predloc and rtchecks_asrloc (see rtchecks_tr:combine_locators/6)
%%  for the reason behind).

%% tests for different uses of GP properties in texec assertions

:- use_module(library(unittest/unittest_props), [try_sols/2]).

:- export(case_tx/2).

% processing texec as texec

:- texec case_tx(A, AB) : (A = a)
     # "test should pass".
:- texec case_tx(A, AB) : (A = a) + try_sols(2)
     # "test should pass".

% processing texec as test

:- texec case_tx(A, AB) : (A = a) + is_det
     # "test should fail, bug: true&false test".
:- texec case_tx(A, AB) : (A = a) + (try_sols(2), non_det)
     # "test should pass".

case_tx(a, a).
case_tx(a, b).

% ----------------------------------------------------------------------

%% tests for different uses of AP and GP properties in test assertions

:- export(case_ts/2).

:- test case_ts(X, Y) : (X = a)
    # "test should pass".
:- test case_ts(X, Y) : (X = a) + try_sols(1)
    # "test should pass".
:- test case_ts(X, Y) : (X = b) + is_det
    # "test should pass".
:- test case_ts(X, Y) : (X = a) => (Y = b) + try_sols(1)
    # "test should fail".
:- test case_ts(X, Y) : (X = b) => (Y = a ; Y = b) + is_det
    # "test should pass".

% ----------------------------------------------------------------------

%% checks for the properties in the assertions are added to the unit
%% tests for case_ts/2 iff this module does not include the rtchecks
%% package. To see the effects of this assertions remove the rtchecks
%% package from the package list and run the tests for this module
%% wuth the [rtc_entry] test option. If you do not remove the
%% package, the assertions below will be simply turned into rtchecks.

% :- pred case_ts(X,Y) : atm(X) => int(Y) # "makes unit tests fail".
% :- pred case_ts(X,Y) : atm(X) => gnd(Y) # "does not affect unit tests".

case_ts(a, a).
case_ts(a, b).
case_ts(b, b).
