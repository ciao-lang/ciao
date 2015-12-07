:- module(matrix_gsl, [
		double_list_to_matrix/2,
		matrix_to_double_list/2,
		matrix_QR/3,
		matrix_QR_decomp/3,
		matrix_QR_Rsolve/3,
		matrix_QR_lssolve/5,
		matrix_lssolve/4,
		matrix_lssolveu/7,
		matrix_R_solve_list/3,
		min/3
	    ],
	    [assertions, unittestdecls, fsyntax]).

:- use_module(library(lists)).

:- include(library(gsl_imports/gsl_imports_auto)).
:- use_module(library(math/matrix/matrix_basic)).

:- doc(module, "This module provides the predicates that convert
   from C implemented matrix to prolog matrix.  A prolog matrix is a
   list of list of numbers.").

double_list_to_matrix(DList, Matrix) :-
	double_list_matrix_properties(DList, Elements, Rows, Cols),
	Length is Rows * Cols,
	properties_to_matrix(Elements, Length, Rows, Cols, Matrix).

matrix_to_double_list(Matrix, DList) :-
	matrix_to_properties(Matrix, Elements, _Length, Rows, Cols),
	double_list_matrix_properties(DList, Elements, Rows, Cols).

double_list_matrix_properties(DList, Elements, Rows, Cols) :-
	matrix_rows(DList, Rows),
	double_list_matrix_properties_(DList, Elements, Cols).

double_list_matrix_properties_([],           [],       _).
double_list_matrix_properties_([Row|Matrix], Elements, Cols) :-
	length(Row, Cols),
	append(Row, Elements0, Elements),
	double_list_matrix_properties_(Matrix, Elements0, Cols).

% matrix_destroy_list([]    ).
% matrix_destroy_list([M|Ms]) :-
% 	matrix_destroy(M),
% 	matrix_destroy_list(Ms).

% vector_destroy_list([]    ).
% vector_destroy_list([M|Ms]) :-
% 	vector_destroy(M),
% 	vector_destroy_list(Ms).

:- test matrix_QR(X, Y, Z) : (X = [[2]]) => ( Y
	    == [[1.0]], Z == [[2.0]] ) # "Matrix 1x1".

:- test matrix_QR(X, Y, Z) : (X = [[2.0, 3.0], [0.0, 5.0]]) => ( Y
	    == [[1.0, 0.0], [0.0, 1.0]], Z == [[2.0, 3.0], [0.0, 5.0]] ) #
	"Matrix 2x2".

:- test matrix_QR(C, A, B) :
	(
	    C = [[-0.6, -3.12], [0.8, 0.16], [0.0, -1.8]]
	) =>
	(
	    near(A, [[-0.6, -0.64, -0.48], [0.8, -0.48, -0.36], [0.0, -0.6, 0.8
		    ]
		], 0.00000001),
	    near(B, [[1.0, 2.0], [0.0, 3.0], [0.0, 0.0
		    ]
		], 0.00000001)
	) # "Matrix 2x3".

matrix_QR_decomp0(Matrix, Rows, Cols, QR0, Tau0) :-
	double_list_to_matrix(Matrix, QR0),
	matrix_rows(Matrix, Rows),
	matrix_cols(Matrix, Cols),
	min(Cols, Rows, D),
	gsl_vector_alloc(D, Tau0),
	gsl_linalg_QR_decomp(QR0, Tau0).

matrix_QR_decomp(Matrix, QR, Tau) :-
	matrix_QR_decomp0(Matrix, _Rows, _Cols, QR0, Tau0),
	matrix_to_double_list(QR0, QR),
	matrix_destroy(QR0),
	vector_to_list(Tau0, Tau, _),
	vector_destroy(Tau0).

matrix_QR(Matrix, Q, R) :-
	matrix_QR_decomp0(Matrix, Rows, Cols, QR0, Tau0),
	gsl_matrix_alloc(Rows, Rows, Q0),
	gsl_matrix_alloc(Rows, Cols, R0),
	gsl_linalg_QR_unpack(QR0, Tau0, Q0, R0),
	matrix_destroy(QR0),
	matrix_to_double_list(Q0, Q),
	matrix_destroy(Q0),
	matrix_to_double_list(R0, R),
	matrix_destroy(R0).

% :- test matrix_r_solve(R, B, A) : (
% 	Y=[64,78,83,88,89,99,101,102],
% 	X=[[1,58,111],
% 	   [1,84,131],
% 	   [1,78,158],
% 	   [1,81,147],
% 	   [1,82,121],
% 	   [1,102,165],
% 	   [1,85,174],
% 	   [1,102,169]],
% 	matrix_QR(X, Q, RC),
% 	matrix_vector_multiply(Q, Y, BC),
% 	matrix_cols(RC, N),
% 	length(R, N),
% 	append(R,_, RC),
% 	length(B, N),
% 	append(B,BT,BC)) =>
% 	( A0 =
% 	[[9.053849885836879],[0.5202789721838376],[0.2397463704130665]],
% 	  BT0 =
% 	[[5.539862922155672],[8.433689046736811],[-1.748806496201705],
% 	 [8.416071640337355],[-0.79981317926797]],
% 	    structure_substraction([A, BT], [A0, BT0], Diff),
% 	    structure_max_abs_number(Diff, Max),
% 	    Max < 0.00000001).

:- test min(A, B, C) : (A = -5, B = 2.0) => near(C, -5, 0).
:- test min(A, B, C) : (A = 5, B = -2.0) => near(C, -2, 0).
:- test min(A, B, C) : (A = 5, B = 5.0) => near(C, 5, 0).

min(A, B, A) :-
	A =< B,
	!.
min(_A, B, B).

matrix_lssolve(A, B, X, R) :-
	matrix_lssolveu(A, B, _, _, _, X, R).

:- test matrix_lssolveu(A, B, Rows, Cols, U, X, R) : (
	    A=[[1, 58, 111],
		[1, 84, 131],
		[1, 78, 158],
		[1, 81, 147],
		[1, 82, 121],
		[1, 102, 165],
		[1, 85, 174],
		[1, 102, 169]],
	    B=[64, 78, 83, 88, 89, 99, 101, 102]) => (
	    Rows=8,
	    Cols=3,
	    near(U, [[-2.82842712474619, -237.58787847868, -415.7787873376899],
		    [0.0, -37.0675059857013, -45.02595887200545],
		    [0.0, 0.0, -43.1122143673516]], 0.00000001),
	    near(X, [9.053849885836959, 0.520278972183837, 0.2397463704130662],
		0.00000001),
	    near(R, [-1.841877388349833, -6.164058073390942,
		    -4.515536241440712, 1.560836916551507,
		    8.273963575107393, -2.680456166744258,
		    6.00656902666337, -0.6394416483965236], 0.00000001)) #
	"8x3 over determined system.".

matrix_lssolveu(Matrix, Result, Rows, Cols, Upper, Solution, Residual) :-
	matrix_QR_decomp0(Matrix, Rows, Cols, QR0, Tau0),
	gsl_matrix_alloc(Cols, Cols, Upper0),
	gsl_linalg_QR_U(QR0, Upper0),
	matrix_to_double_list(Upper0, Upper),
	matrix_QR_lssolve0(QR0, Rows, Cols, Tau0, Result, Solution, Residual).

matrix_QR_lssolve(QR, Tau, B, X, R) :-
	double_list_to_matrix(QR, QR0),
	matrix_rows(QR, Rows),
	matrix_cols(QR, Cols),
	min(Rows, Cols, D),
	list_to_vector(Tau, D, Tau0),
	matrix_QR_lssolve0(QR0, Rows, Cols, Tau0, B, X, R).

matrix_QR_lssolve0(QR0, Rows, Cols, Tau0, B, X, R) :-
	list_to_vector(B, Rows, B0),
	gsl_vector_alloc(Cols, X0),
	gsl_vector_alloc(Rows, R0),
	gsl_linalg_QR_lssolve(QR0, Tau0, B0, X0, R0),
	matrix_destroy(QR0),
	vector_destroy(Tau0),
	vector_to_list(X0, X, _),
	vector_destroy(X0),
	vector_to_list(R0, R, _),
	vector_destroy(R0).

:- test matrix_R_solve_list(A, B, C) :
	(A = [[40, 80], [400, 800]], B = [[8, 2], [0, 5]])
	=> near(C, [[1.0, 16.0], [10.0, 160.0]], 0).

matrix_R_solve_list(Matrix, R, Solution) :-
	matrix_cols(Matrix, Cols),
	length(RU, Cols),
	append(RU, _, R),
	double_list_to_matrix(RU, R0),
	matrix_R_solve_list_(Matrix, R0, Cols, Solution),
	matrix_destroy(R0).

matrix_R_solve_list_([],         _R0, _Cols, []).
matrix_R_solve_list_([B|Matrix], R0,  Cols,  [X|Solutions]) :-
	matrix_QR_Rsolve0(R0, Cols, B, X),
	matrix_R_solve_list_(Matrix, R0, Cols, Solutions).

:- test matrix_QR_Rsolve(A, B, C) : (A = [[3]], B = [12]) => near(C, [4], 0).
:- test matrix_QR_Rsolve(A, B, C) : (A = [[8, 2], [0, 5]], B = [40, 80])
	=> (C = [1.0, 16.0]).
:- test matrix_QR_Rsolve(A, B, C) : ( A = [[8, 2], [0, 5], [10, 100]],
	    B = [40, 80] ) => (C = [1.0, 16.0]).
:- test matrix_QR_Rsolve(A, B, C) : ( A = [[8, 2], [-100000, 5], [0, 0]],
	    B = [40, 80] ) => (C = [1.0, 16.0])
# "Matrix back substitution of 3x2.".

matrix_QR_Rsolve(R, B, X) :-
	length(B,  Cols),
	length(RU, Cols),
	append(RU, _, R),
	double_list_to_matrix(RU, R0),
	matrix_QR_Rsolve0(R0, Cols, B, X),
	matrix_destroy(R0).

matrix_QR_Rsolve0(R0, Cols, B, X) :-
	list_to_vector(B, Cols, B0),
	gsl_linalg_QR_Rsvx(R0, B0),
	vector_to_list(B0, X, _),
	vector_destroy(B0).

% matrix_QR_Rsolve(R, B, X) :-
% 	double_list_to_matrix(R, R0),
% 	length(B, N),
% 	list_to_vector(B, N, B0),
% 	gsl_linalg_QR_Rsolve_over_determined(R0, B0),
% 	matrix_destroy(R0),
% 	vector_to_list(B0, X, _),
% 	vector_destroy(B0).
