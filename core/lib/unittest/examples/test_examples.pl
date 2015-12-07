:- module(_, [call_test10/1, cut_test5/0, display_fail/0, p/1, display1/1],
	     [assertions, nativeprops, basicmodes, rtchecks, unittestdecls]).

:- use_module(library(unittest/unittest_props)).

:- reexport(library(write)). % ?

:- doc(author, "Edison Mera").

:- doc(module, "Some examples of unit tests.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- test p(A) : (A = a) + not_fails # "Test OK".
:- test p(A) : (A = b) + fails # "Test OK".
:- test p(A) : (A = c) + exception(error(c, _)) # "Test OK".
:- test p(A) : (A = c) + fails # "Test OK".
:- test p(A) : (A = c) + not_fails # "Test OK".
:- test p(A) : (A = c) # "Test OK (?)".
:- test p(A) : (A = d, A = a) # "Wrong test (precondition fails)".
:- test p(A) : possible_exceptions([any_exception(A)]) # "Wrong test (throws exception in precondition)".
:- test p(A) : (A = h) # "Abort".
:- test p(Z) => (Z = d).

:- check comp p(X) : (X = a) + not_fails.
% :- check comp p(X) : (X = b) + not_fails.

p(a).
p(b) :- fail.
p(c) :- throw(error(c, 'error c')).
p(h) :- halt(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- texec display1(A) : (A = hello2) # "Test OK".
:- texec display1(A) : (A = hello2) + user_output("bye") # "Wrong test".

:- test display1(A) : (A = hello) + user_output("hello") # "Test OK".
:- test display1(A) : (A = hello2) + user_output("bye") # "Wrong test".
:- test display1(A) : (A = hello3) + user_output("hello!") # "Wrong test".

display1(A) :- display(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- test display_fail + (user_output("hello"), fails) # "Test OK".

display_fail :- display(hello), fail.

:- pred display_fail(A) + (user_output("hello2"), fails).
:- export(display_fail/1).
display_fail(_A) :- display(hello), fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- test cut_test5 + (times(3), user_output("Cut disjunction"), fails) #
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
