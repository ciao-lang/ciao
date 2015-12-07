:- module(_, _, [assertions, library(math/math_clp), unittestdecls]).

:- use_module(library(math/matrix/matrix_basic)).
:- use_module(library(math/vector)).

:- load_test_package(clpr).
:- load_test_module(library(math/vector)).
:- load_test_module(library(math/matrix/matrix_basic)).

:- doc(author, "Edison Mera").

:- doc(rss/2, "The residual square sum.").

:- test rss(Residual, RSS) :
	(
	    Residual = [-1.841877388349833,
		-6.164058073390942, -4.515536241440712, 1.560836916551507,
		8.273963575107393, -2.680456166744258,
		6.00656902666337, -0.6394416483965236]
	) => succeeds(RSS .=. 176.345479271502).

rss(Residual, RSS) :-
	vector_multiply(Residual, Residual, RSS).

:- test mrss(RSS, Rows, Cols, MRSS) :
	(
	    RSS = 176.345479271502,
	    Rows = 8,
	    Cols = 3
	) => succeeds(MRSS .=. 35.2690958543004).

:- doc(mrss/4, "The mean of residual square sum.").

mrss(RSS, Rows, Cols, MRSS) :-
	MRSS .=. RSS / (Rows - Cols).

:- doc(stderror/2, "The standard error.").

stderror(Variance, Error) :-
	vector_sqrt(Variance, Error).

:- doc(tvalue/2, "The relative error.").

tvalue(Error, Vector, TValue) :-
	vector_division(Error, Vector, TValue).

bound_solution(StdError, Z, Solution, BoundSolution) :-
	vector_constant_multiply_addition(StdError, Z, Solution,
	    BoundSolution).

:- test covariance(XXT, Cols, MRSS, CoVariance) : (
	    XXT = [[8, 672, 1176],
		[672, 57822, 100453],
		[1176, 100453, 176758]],
	    Cols = 3,
	    MRSS = 35.2690958543004) =>
	succeeds((C0 =
	    [[223.8943237364822, -1.119762785525169, -0.8532354610243564],
		[-1.119762785525169, 0.05366729794342455, -0.0230495934811054],
		[-0.8532354610243564, -0.0230495934811054, 0.01897551913902865]
	    ], matrix_eq(CoVariance, C0)))
# "Calculating known covariance matrix of 3x3.".

:- doc(covariance/3, "The covariance matrix").

covariance(XXT, Cols, MRSS, Covariance) :-
	matrix_identity(Cols, I),
	matrix_multiply_transpose(XXT, B, I),
	matrix_constant_multiply(B, MRSS, Covariance).

:- doc(variance/2, "The variance which is the diagonal of the
   covariance.").

variance(Covariance, Variance) :-
	matrix_diagonal(Covariance, Variance).

:- true pred general_regression(Data, Results, Pattern, Variables,
	    Coefficients, Residuals) : list * list * list * list * term * term
	=>
	list * list * list * list * list * list # "Generic linear regression.
@var{Data} contains the data, @var{Results} contains the observed
results, @var{Pattern} contains a list of arithmetic expressions that
represent the components of the linear model. @var{Variables} contains
the list of variables used in the expression Pattern.".

:- test general_regression(Data, Result, Pattern, Variables, Coefficients,
	    Residuals) :
	(
	    Data = [[2, 2], [1, 2], [1, 1], [2, 1]],
	    Result = [3, 5, 7, 9],
	    Pattern = [1, X1, X2, X1*X2],
	    Variables = [X1, X2]
	) => (
	    succeeds(vector_eq(Coefficients, [3, 6, 2, -4.0])),
	    succeeds(vector_eq(Residuals, [0.0, 0.0, 0.0, 0.0]))).

general_regression(Datas, Results, Pattern, Variables, Coefficients,
	    Residuals) :-
	apply_data_variables(Datas, Pattern, Variables, AppliedDatas),
	matrix_lssolve(AppliedDatas, [Results], _, [Coefficients], [Residuals]).

:- data general_regression_elem/1.

apply_data_variables([],     _,       _,         []).
apply_data_variables([D|Ds], Pattern, Variables, [L|Ls]) :-
	( Variables = D,
	    evaluate_patterns(Pattern, L),
	    assertz_fact(general_regression_elem(L)),
	    fail
	)
    ;
	current_fact(general_regression_elem(L)),
	retract_fact(general_regression_elem(L)),
	apply_data_variables(Ds, Pattern, Variables, Ls).

evaluate_patterns([],     []).
evaluate_patterns([P|Ps], [L|Ls]) :-
	L .=. P,
	evaluate_patterns(Ps, Ls).
