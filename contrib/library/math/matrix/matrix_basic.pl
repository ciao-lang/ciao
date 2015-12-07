:- module(matrix_basic, _, [assertions, unittestdecls, nativeprops,
		library(math/math_clp)]).

:- use_module(library(lists)).
:- use_module(library(hiordlib)).
:- use_module(library(llists)).
:- use_module(library(math/vector)).

:- load_test_package(clpr).

:- doc(author, "Edison Mera").

:- doc(module, "This module contain the matrix predicates that do not
   depend of C implemented functions.").

matrix_cols([Row|_X], Cols) :-
	length(Row, Cols).

matrix_rows(X, Rows) :-
	length(X, Rows).

matrix_add_column([],     [], []).
matrix_add_column([V|Vs], M,  [R|Rs]) :-
	matrix_add_element(M, V, R, Ms),
	matrix_add_column(Vs, Ms, Rs).

matrix_add_element([M|Ms], V, [V|M], Ms).
matrix_add_element([],     V, [V],   []).

matrix_add_column_zero([],     [],         []).
matrix_add_column_zero([A|As], [[0|A]|Bs], [0|Cs]) :-
	matrix_add_column_zero(As, Bs, Cs).

:- test matrix_norm(M, N) : (M = []) => succeeds(N .=. 0)
# "Matrix 0x0 have norm 0".

:- test matrix_norm(M, N) : (M = [[1, -2.0], [1, 1]]) => (N .=. 3).

matrix_norm(M, N) :-
	matrix_norm_(M, 0, N).
matrix_norm_([],    N,  N).
matrix_norm_([C|M], N0, N) :-
	vector_norm_inf(C, X),
	N1 is X + N0,
	matrix_norm_(M, N1, N).

matrix_substraction([],     Y,      Y).
matrix_substraction([X|Xs], [Y|Ys], [Z|Zs]) :-
	vector_substraction(X, Y, Z),
	matrix_substraction(Xs, Ys, Zs).

:- true pred matrix_addition(A, B, C) : list(list(number)) *
	list(list(number)) * term => list(list(number)) *
	list(list(number)) * list(list(number)) # "Unifies @var{Z}
	with the sum of the matrices @var{X} and @var{Y}.".

matrix_addition([],     Y,      Y).
matrix_addition([X|Xs], [Y|Ys], [Z|Zs]) :-
	vector_addition(X, Y, Z),
	matrix_addition(Xs, Ys, Zs).

:- test matrix_constant_multiply(X, Y, Z) : (X = [[1.5]], Y = 2)
	=> (Z = [[Z0]], Z0 .=. 3.0) # "Multiply a 1x1 matrix by a number".

matrix_constant_multiply([],    _, []).
matrix_constant_multiply([A|M], K, [B|R]) :-
	vector_constant_multiply(A, K, B),
	matrix_constant_multiply(M, K, R).


:- test matrix_identity(N, I) : (N = 3)
	=> (I == [[1, 0, 0], [0, 1, 0], [0, 0, 1]])
# "Matrix identity 3x3".

matrix_identity(0, []) :- !.
matrix_identity(N, [[1|I]|Is]) :-
	N > 0,
	N1 is N-1,
	matrix_identity(N1, Is1),
	matrix_add_column_zero(Is1, Is, I).

matrix_eq(A, B) :- map(A, vector_eq, B).

:- test matrix_multiply(A, B, C) : (
	    A = [[-0.60, -0.64, -0.48],
		[0.80, -0.48, -0.36],
		[0.00, -0.60, 0.80]],
	    B = [[1, 2],
		[0, 3],
		[0, 0]]) =>
	succeeds(matrix_eq(C, [[-0.6, -3.12], [0.8, 0.16], [0.0, -1.8]]))
# "Product M3x3 and M1x3".

:- true pred matrix_multiply_transpose(Matrix, Transpose, Result) :
	list(list(term)) * list(list(term)) * term => list(list(term))
	* list(list(term)) * list(list(term)) # "Unifies @var{Result}
	with the matricial product between the matrices @var{Matrix}
	and the transposed matrix of @var{Transpose}.".

matrix_multiply_transpose([],           _,         []).
matrix_multiply_transpose([Row|Matrix], Transpose, [Element|Result]) :-
	matrix_vector_multiply(Transpose, Row, Element),
	matrix_multiply_transpose(Matrix, Transpose, Result).

matrix_vector_multiply([],           _,      []).
matrix_vector_multiply([Row|Matrix], Vector, [Element|Result]) :-
	vector_multiply(Row, Vector, Element),
	matrix_vector_multiply(Matrix, Vector, Result).

:- true pred matrix_multiply(Matrix1, Matrix2, Result) :
	list(list(number)) * list(list(number)) * term =>
	list(list(number)) * list(list(number)) * list(list(number)) #
"Unifies @var{Result} with the matricial product between the
	matrices @var{Matrix1} and @var{Matrix2}.".

matrix_multiply(A, B, C) :-
	transpose(B, B2),
	matrix_multiply_transpose(A, B2, C).

:- test matrix_diagonal(M, D) : (M = [[a, 2, 3], [4, 5, 6], [7, 8, 9]])
	=> (D = [a, 5, 9]).

matrix_diagonal([],                    []).
matrix_diagonal([[Element|_]|Matrix0], [Element|Vector]) :-
	matrix_add_column(_, Matrix, Matrix0),
	matrix_diagonal(Matrix, Vector).

:- test triangular_product_combination(A, B) :
	(A = [3, 2, 1, 5]) =>
	succeeds(matrix_eq(B, [[9.0, 6.0, 3.0, 15.0], [4.0, 2.0, 10.0], [1.0, 5.0], [25.0]])).

triangular_product_combination([],               []).
triangular_product_combination([Element|Vector], [Row|Matrix]) :-
	vector_constant_multiply([Element|Vector], Element, Row),
	triangular_product_combination(Vector, Matrix).

:- test triangular_matrix(A, B) : (A = [[1, 2, 4], [3, 4], [5]])
	=> (B = [[1, 2, 4], [2, 3, 4], [4, 4, 5]]) # "Simple call".
:- test triangular_matrix(A, B) : (B = [[1, 2, 4], [2, 3, 4], [4, 4, 5]])
	=> (A = [[1, 2, 4], [3, 4], [5]]) # "Reverse call".
:- test triangular_matrix(_A, B) : (B = [[1, 2, 4], [2, 3, 4], [4, 8, 5]])
	+ fails
# "Reverse call over non simetric matrix".

:- true pred triangular_matrix(A, B) : list(list(number)) *
	list(list(number)) => list(list(number)) * list(list(number)) #
"Converts the triangular matrix @var{A} to the rectangular matrix
@var{B}.  A triangular matrix is defined as:

   A = [[A11,A12,    ...,A1N],
            [A22,A23,...,A2N],
                         ....
                        [ANN]]

   And the respective rectangular matrix is:

   B = [[A11,A12,    ...,A1N],
        [A12,A22,A23,...,A2N],
        [A13,A23,A33,...,A3N],
	                  ...
        [A1N,A2N,A3N,...,ANN]]
".

triangular_matrix([],          []).
triangular_matrix([[A|Ar]|As], [[A|Ar]|Bs]) :-
	matrix_add_column(Ar, Bs1, Bs),
	triangular_matrix(As, Bs1).

:- test matrix_lssolve(A, B, AA, X, R) : (
	    A=[[1, 58, 111],
		[1, 84, 131],
		[1, 78, 158],
		[1, 81, 147],
		[1, 82, 121],
		[1, 102, 165],
		[1, 85, 174],
		[1, 102, 169]],
	    B=[[64, 78, 83, 88, 89, 99, 101, 102]]) => (
	    succeeds(matrix_eq(X, [[9.053849885836959,
			    0.520278972183837, 0.2397463704130662]])),
	    succeeds(matrix_eq(R, [[-1.841877388349833, -6.164058073390942,
			    -4.515536241440712, 1.560836916551507,
			    8.273963575107393, -2.680456166744258,
			    6.00656902666337, -0.6394416483965236]]))) #
	"8x3 over determined system.".

matrix_lssolve(A, B, AA, X, R) :-
	transpose(A, AT),
	matrix_multiply_transpose(B,  AT, BA),
	matrix_multiply_transpose(AT, AT, AA),
	matrix_multiply_transpose(X,  AA, BA),
	% matrix_multiply_transpose(XA, AT, BA),
	% Now get R:
	matrix_multiply_transpose(X, A, XA),
	matrix_substraction(B, XA, R).
