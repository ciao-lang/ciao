:- module(_, _, [assertions, unittestdecls]).

:- use_module(library(gsl_imports/matrix_gsl), [matrix_QR_Rsolve/3]).
:- use_module(library(math/vector)).

:- doc(module, "Statistics functions implemented using matrixes.").

:- test confidence_factor(Upper, Value, CF) :
	(
	    Upper = [[1, 2, 3], [1, 1, 2], [3, 1, 4], [2, 1, 1]],
	    Value = [2, 5, 7]
	) =>
	near(CF, 44.375, 0.000000001)
# "Test of confidence factor with a 3x4 QR matrix.".

confidence_factor(Upper, Value, ConfidenceFactor) :-
	matrix_QR_Rsolve(Upper, Value, A),
	vector_multiply(A, A, ConfidenceFactor).
