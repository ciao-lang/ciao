:- module(functionstr, [defunc/3, defunc_goal/2], []).

:- include(library(fsyntax/ops)).

:- use_module(library(terms), [copy_args/3]).

% Database to store declaration for the currently compiled module
:- data fun_eval/3.
:- data eval_arith/1.
:- data eval_hiord/1.
:- data defined_functions/1.
:- data fun_return/4.

%%%% Sentence translation %%%%

% defunc(FuncItem, PredItem, Module) :- PredItem is a clause, query or
% command which is equivalent to FuncItem but without functions.

% defunc(0, _, Mod) :- !, % no need for initialization
defunc(end_of_file, end_of_file, Mod) :- !,
        retractall_fact(fun_eval(_,Mod,_)),
        retractall_fact(eval_arith(Mod)),
        retractall_fact(eval_hiord(Mod)),
        retractall_fact(defined_functions(Mod)),
        retractall_fact(fun_return(_,_,Mod,_)).
defunc((?- _), _, _) :- !, fail.
defunc((:- Decl), NewDecl, Mod) :- !,
        defunc_decl(Decl, NewDecl, Mod).
% todo: the following rule seems to be redundant (jfmc)
defunc((FuncHead := FuncValOpts), Clauses, Mod) :-
        nonvar(FuncValOpts),
        FuncValOpts = (FuncVal1 | FuncValR),
        !,
        Clauses = [Clause1 | ClauseR],
        defunc((FuncHead := FuncVal1), Clause1, Mod),
        defunc((FuncHead := FuncValR), ClauseR, Mod).
defunc(Def, Clauses, Mod) :-
	defunc_pred(Def, Clauses, Mod).

defunc_pred((FuncHead := CondFuncVal), (Head :- Body), Mod) :-
        nonvar(CondFuncVal),
        CondFuncVal = (Cond ? FuncVal),
        !,
        arith_flag(Mod, ArithF),
        make_tmp_fun_eval(FuncHead, Mod, Ref),
        defunc_funhead(FuncHead, Mod, ArithF, Head, Ret_Arg, Body, RestBody),
        normalize(Cond, Mod, ArithF, NCond),
        normalize(FuncVal, Mod, ArithF, NFuncVal),
        delt_tmp_fun_eval(Ref),
        concat_bodies(NCond, (!, '\6\Unif_ret'(Ret_Arg, NFuncVal)), RestBody).
defunc_pred((FuncHead := FuncVal), (Head :- Body), Mod) :- !,
        arith_flag(Mod, ArithF),
        make_tmp_fun_eval(FuncHead, Mod, Ref),
        normalize(FuncVal, Mod, ArithF, NFuncVal),
        defunc_funhead(FuncHead, Mod, ArithF, Head, NFuncVal, AddBody, true),
        delt_tmp_fun_eval(Ref),
        del_last_true(AddBody, Body).
defunc_pred((FuncHead := FuncVal :- FuncBody), (Head :- Body), Mod) :- !,
        arith_flag(Mod, ArithF),
        make_tmp_fun_eval(FuncHead, Mod, Ref),
        defunc_funhead(FuncHead, Mod, ArithF, Head, Ret_Arg, Body, RestBody),
        normalize(FuncBody, Mod, ArithF, NFuncBody),
        normalize(FuncVal, Mod, ArithF, NFuncVal),
        delt_tmp_fun_eval(Ref),
        concat_bodies(NFuncBody, '\6\Unif_ret'(Ret_Arg, NFuncVal), RestBody).
defunc_pred((Head :- Body), (NewHead :- NewBody), Mod) :- !,
        arith_flag(Mod, ArithF),
        normalize(Body, Mod, ArithF, NBody),
        defunc_head(Head, Mod, ArithF, NewHead, NewBody, NBody).
defunc_pred(Head, (NewHead :- NewBody), Mod) :-
        arith_flag(Mod, ArithF),
        defunc_head(Head, Mod, ArithF, NewHead, Body, true),
        del_last_true(Body, NewBody).

defunc_decl(function(Spec), R, Mod) :- !,
        warning_function_decl,
        defunc_decl(fun_eval(Spec), R, Mod).
defunc_decl(fun_eval(Spec), _, Mod) :- !,
        ( Spec = QM:F/A, functor(P, F, A) ->
            make_fun_eval(P, Mod, QM)
        ; Spec = F/A, functor(P, F, A) ->
            make_fun_eval(P, Mod, (-))
        ; Spec = arith(true) ->
            asserta_fact(eval_arith(Mod))
        ; Spec = arith(false) ->
            retractall_fact(eval_arith(Mod))
        ; Spec = hiord(true) ->
            asserta_fact(eval_hiord(Mod))
        ; Spec = hiord(false) ->
            retractall_fact(eval_hiord(Mod))
        ; Spec = defined(true) ->
            asserta_fact(defined_functions(Mod))
        ; Spec = defined(false) ->
            retractall_fact(defined_functions(Mod))
        ; function_output_arg(Spec, Fun, A, QM) ->
            asserta_fact(fun_return(Fun, A, Mod, QM)),
            make_fun_eval(Fun, Mod, QM)
        ; error(['Invalid fun_eval specification: ',Spec])
        ).
defunc_decl(fun_return(FSpec), _, Mod) :- !,
        ( function_output_arg(FSpec, Fun, A, QM) ->
            asserta_fact(fun_return(Fun, A, Mod, QM))
        ;
            error(['Invalid fun_return specification: ',FSpec])
        ).
defunc_decl(lazy(Decl), (:- lazy(LazySpec)), Mod) :- !,
        defunc_lazy_decl(Decl, LazySpec, Mod).
defunc_decl(initialization(Goal), (:- initialization(NGoal)), Mod) :- !,
        arith_flag(Mod, ArithF),
        normalize(Goal, Mod, ArithF, NGoal).
defunc_decl(on_abort(Goal), (:- on_abort(NGoal)), Mod) :- !,
        arith_flag(Mod, ArithF),
        normalize(Goal, Mod, ArithF, NGoal).

defunc_lazy_decl(function(Spec), LazySpec, Mod) :- !,
        warning_function_decl,
        defunc_lazy_decl(fun_eval(Spec), LazySpec, Mod).
defunc_lazy_decl(fun_eval(Spec), LazySpec, Mod) :- !,
        ( Spec = F/A, functor(P, F, A)  ->
            make_fun_eval(P, Mod, (-)),
            A1 is A + 1,
            LazySpec = F/A1
        ; functor(Spec, F, A), has_tilde(A, Spec, Arg) ->
            Am1 is A - 1,
            functor(Fun, F, Am1),
            asserta_fact(fun_return(Fun, Arg, Mod, (-))),
            make_fun_eval(Fun, Mod, (-)),
            LazySpec = F/A-Arg
        ; error(['Invalid fun_eval specification in lazy declaration: ',Spec])
        ).
defunc_lazy_decl(fun_return(Spec), LazySpec, Mod) :- !,
        ( functor(Spec, F, A), has_tilde(A, Spec, Arg) ->
            Am1 is A - 1,
            functor(Fun, F, Am1),
            asserta_fact(fun_return(Fun, Arg, Mod, (-))),
            LazySpec = F/A-Arg
        ; error(['Invalid fun_return specification in lazy declaration: ',Spec])
        ).

warning_function_decl :-
        warning(
	   'Declaration "function" deprecated, please use "fun_eval" instead').

% Are arithmetic operation interpreted as functions?
arith_flag(Mod, ArithF) :-
        eval_arith(Mod) -> ArithF = true ; ArithF = false.
/*
% Does the module uses hiord?
hiord_flag(Mod, HiordF) :-
        eval_hiord(Mod) -> HiordF = true ; HiordF = false.
*/

defunc_funhead(Head, Mod, Arith, NPred, Ret_Arg, AddBody, RestBody) :-
        fun_to_pred_ret(Head, (-), Mod, Arith, Pred, Ret_Arg),
        defunc_nrf_args_of(Pred, NPred, AddBody, RestBody).

defunc_head(Head, Mod, Arith, NewHead, AddBody, RestBody) :-
        normalize_args_of(Head, Mod, Arith, NHead),
        defunc_nrf_args_of(NHead, NewHead, AddBody, RestBody).

concat_bodies(V, B, NB) :- var(V), !,
        NB = (V, B).
concat_bodies((G, Gs), B, (G, NB)) :- !,
        concat_bodies(Gs, B, NB).
concat_bodies(G, B, (G, B)).

%%%% Translating terms to a normal form %%%%

normalize(Var,_Mod,_Arith, Var) :- var(Var), !.
normalize(~V,_Mod,_Arith, NrF) :-
        var(V), !,
        NrF = '\6\Eval'(call(V, Ret_Arg), Ret_Arg). % Apply?
normalize(^(T), Mod, Arith, NT) :- !,
        normalize_args_of(T, Mod, Arith, NT).
normalize(F, Mod, _Arith, NrF) :-
	is_funabs_or_predabs(F),
	eval_hiord(Mod),
	!,
	defunc_funabs_or_predabs(F, NF, Mod),
        NrF = '\6\Predabs'(NF).
normalize(F, Mod, Arith, NrF) :-
        is_arith_exp(F, Arith, F0), !,
        NrF = '\6\Arit'(NF),
        arith_false(Arith, NArith),
        normalize_args_of(F0, Mod, NArith, NF).
normalize(~T, Mod, Arith, NrF) :- !,
        take_qualification(T, QM, Fun),
        fun_to_pred_ret_tilde(Fun, QM, Mod, Arith, Pred0, Ret_Arg),
        add_qualification(QM, Pred0, Pred),
        NrF = '\6\Eval'(Pred, Ret_Arg).
normalize(F, Mod, Arith, NrF) :-
        take_qualification(F, QM, Fun),
        nonvar(Fun),
        fun_eval(Fun, Mod, QM), !,
        fun_to_pred_ret(Fun, QM, Mod, Arith, Pred0, Ret_Arg),
        add_qualification(QM, Pred0, Pred),
        NrF = '\6\Eval'(Pred, Ret_Arg).
normalize((A|B), Mod, Arith, '\6\Opts'(NA, NB)) :- !,
        restore_arith(Arith, NArith),
        normalize(A, Mod, NArith, NA),
        normalize(B, Mod, NArith, NB).
normalize((A?B), Mod, Arith, '\6\Cond'(NA,NB)) :- !,
        restore_arith(Arith, NArith),
        normalize(A, Mod, NArith, NA),
        normalize(B, Mod, NArith, NB).
normalize(T, Mod, Arith, NT) :-
        normalize_args_of(T, Mod, Arith, NT).

normalize_args_of(T, Mod, Arith, NT) :-
        functor(T, F, A),
        functor(NT, F, A),
        normalize_args(A, T, Mod, Arith, NT).

normalize_args(0, _, _Mod,_Arith, _ ) :- !.
normalize_args(N, T0, Mod, Arith, T1) :-
        arg(N, T0, A0),
        arg(N, T1, A1),
        N1 is N-1,
        normalize(A0, Mod, Arith, A1),
        normalize_args(N1, T0, Mod, Arith, T1).

% Is a predicate abstraction?
is_predabs((A :- _)) :-
	nonvar(A),
	( A = (_ShVs -> A0) -> A1 = A0 ; A1 = A	),
	nonvar(A1), functor(A1, '', _).

% Is a function or predicate abstraction?
is_funabs(B) :-
	( nonvar(B), B = (A :- _) -> true ; A = B ),
	nonvar(A),
	( A = (_ShVs -> A0) -> A1 = A0 ; A1 = A	),
	nonvar(A1), A1 = (A2 := _),
	nonvar(A2), functor(A2, '', _).

is_funabs_or_predabs(A) :- is_predabs(A) ; is_funabs(A).

% Defunc for predicate or function abstractions
defunc_funabs_or_predabs(F, NF, Mod) :-
	( F = (ShVs -> Head := R :- Body) ->
	    F0 = (Head := R :- Body)
	; F = (ShVs -> Head := R) ->
	    F0 = (Head := R)
	; F = (ShVs -> Head :- Body) ->
	    F0 = (Head :- Body)
	),
	!,
	defunc_pred(F0, (Head2 :- Body2), Mod),
	NF = (ShVs -> Head2 :- Body2).
defunc_funabs_or_predabs(F, NF, Mod) :-
	defunc_pred(F, NF, Mod).

is_arith_exp(~(F),_Arith, F) :-
        arith_exp(F), !.
is_arith_exp(F, true, F) :-
        arith_exp(F).

arith_false(true,  tempfalse).
arith_false(false, false).

restore_arith(false, false).
restore_arith(tempfalse, true).
restore_arith(true, true).

% PRE: 1st is not var
take_qualification(QM:T, QM, T) :- !.
take_qualification(T,   (-), T).

add_qualification(Q, T, T) :- Q == (-), !.
add_qualification(QM, T, QM:T).

fun_to_pred_ret_tilde(Fun,_QM, Mod, Arith, Pred, Ret_Arg) :-
        functor(Fun, F, A),
        has_tilde(A, Fun, Arg), !, % Quick check
        functor(Pred, F, A),
        arg(Arg, Pred, Ret_Arg),
        restore_arith(Arith, NArith),
        normalize_args_but(A, Arg, Fun, Mod, NArith, Pred).
fun_to_pred_ret_tilde(Fun, QM, Mod, Arith, Pred, Ret_Arg) :-
        fun_to_pred_ret(Fun, QM, Mod, Arith, Pred, Ret_Arg).

normalize_args_but(N, N, T0, Mod, Arith, T1) :- !,
        N1 is N-1,
        normalize_args(N1, T0, Mod, Arith, T1).
normalize_args_but(N, Exc, T0, Mod, Arith, T1) :-
        arg(N, T0, A0),
        arg(N, T1, A1),
        N1 is N-1,
        normalize(A0, Mod, Arith, A1),
        normalize_args_but(N1, Exc, T0, Mod, Arith, T1).

fun_to_pred_ret(Fun, QM, Mod, Arith, Pred, Ret_Arg) :-
        functor(Fun, F, A),
        A1 is A+1,
        functor(Pred, F, A1),
        ( fun_return(Fun, Arg, Mod, QM) -> true
        ; Arg = A1
        ),
        arg(Arg, Pred, Ret_Arg),
        restore_arith(Arith, NArith),
        normalize_args_fun_but(A1, Arg, Fun, Mod, NArith, Pred).

normalize_args_fun_but(N, N, T0, Mod, Arith, T1) :- !,
        N1 is N-1,
        normalize_args(N1, T0, Mod, Arith, T1).
normalize_args_fun_but(N, Exc, T0, Mod, Arith, T1) :-
        N1 is N-1,
        arg(N1, T0, A0),
        arg(N, T1, A1),
        normalize(A0, Mod, Arith, A1),
        normalize_args_fun_but(N1, Exc, T0, Mod, Arith, T1).

%%%% Translating normal forms to terms + goals %%%%

% defunc_nrf(Exp, NewExp, AddGoal, RestGoal) :- NewExp is an expression
% without functions which is equivalent to normal form Exp when adding goals
% AddGoal minus RestGoal.

% Assumes is/2 is imported
% PRE: If is_evaluable(Exp) do not bind to non-variable NewExp
defunc_nrf(V, V, G, G) :- var(V), !.
defunc_nrf(^^(T), ^^(T), G, G) :- !.
defunc_nrf(V, V, G, G) :- is_predabs(V), !. % todo: checking for eval_hiord here does not work
defunc_nrf('\6\Arit'(Fun), V, Add, Rest) :-
        defunc_nrf_args_of(Fun, NFun, Add, Rest0),
        Rest0 = (V is NFun, Rest).
defunc_nrf('\6\Eval'(Pred, Ret_Arg), Ret_Arg, Add, Rest) :-
        defunc_nrf_args_of(Pred, NPred, Add, Rest0),
        Rest0 = (NPred, Rest).
defunc_nrf('\6\Predabs'(Predabs), V, Add, Rest) :-
        V = Predabs,
        Add = Rest.
defunc_nrf(Opts, V, Add, Rest) :-
        Opts = '\6\Opts'(_,_), !,
        Add = (Assigns, Rest),
        defunc_nrf_opts(Opts, V, Assigns).
defunc_nrf('\6\Cond'(Cond, Val), V, Add, Rest) :- !,
        Add = ((Cond -> Assign), Rest),
        defunc_nrf_assign(Val, V, Assign).
defunc_nrf(T0, T1, Add, Rest) :-
        defunc_nrf_args_of(T0, T1, Add, Rest).

defunc_nrf_args_of(T, NT, Add, Rest) :-
        functor(T, F, A),
        functor(NT, F, A),
        defunc_nrf_args(A, T, NT, Add, Rest).

defunc_nrf_args(0, _, _, X, X) :- !. 
defunc_nrf_args(N, T0, T1, Add, Rest) :-
        arg(N, T0, A0),
        arg(N, T1, A1),
        N1 is N-1,
        defunc_nrf(A0, A1, NRest, Rest),
        defunc_nrf_args(N1, T0, T1, Add, NRest).

defunc_nrf_opts(A, V, (V = A)) :- var(A), !.
defunc_nrf_opts('\6\Opts'(A,B), V, (A_As ; Assigns)) :- !,
        defunc_nrf_assign(A, V, A_As),
        defunc_nrf_opts(B, V, Assigns).
defunc_nrf_opts(A, V, A_As) :-
        defunc_nrf_assign(A, V, A_As).

% todo: rename 'assign' by 'unify'? (jfmc) (to distinguish from imperative assignment)
defunc_nrf_assign(Val, V, (V = Val)) :- var(Val), !.
defunc_nrf_assign(Val, V, Assign) :-
        is_evaluable(Val), !,
        defunc_nrf(Val, V, Assign1, true),
        del_last_true(Assign1, Assign).
defunc_nrf_assign(Val, V, Assign) :-
        defunc_nrf(Val, NVal, Assign, (V = NVal)).

%%%% Goal translation, translates normal forms %%%%

% defunc_goal(Goal, NewGoal) :- NewGoal is a goal which is equivalent to Goal
% (which is normalized) but without functions.
defunc_goal(^^(G), G) :- !.
defunc_goal('\6\Unif_ret'(R, Val), Goal) :-
        ( nonvar(Val),
          is_evaluable(Val) ->
            defunc_nrf(Val, R, AddGoal, true),
            del_last_true(AddGoal, Goal)
        ;
          defunc_nrf(Val, NVal, AddGoal, true),
          del_last_true((R = NVal, AddGoal), Goal)
        ).
defunc_goal('\6\Arit'(G), G) :- !.       % For integer/1, float/1
defunc_goal('\6\Eval'(G,X), NG) :- !,    % A predicate is like a function
        functor(G, F, A),
        A1 is A-1,
        functor(NG, F, A1),
        take_out_arg(A, G, X, NG).
defunc_goal('\6\Opts'(A,B), (A|B)) :- !. % To give a warning
defunc_goal('\6\Cond'(A,B), (A?B)) :- !. % To give a warning
defunc_goal((U1 = U2), NewGoal) :-
        (V = U1, Fun = U2 ; V = U2, Fun = U1),
        ( nonvar(Fun),
          Fun = '\6\Eval'(_,_)
        ; var(V), nonvar(Fun),
          is_evaluable(Fun)
        ), !,
        defunc_nrf(Fun, V, AddGoal, true),
        del_last_true(AddGoal, NewGoal).
defunc_goal(QM:Goal, NewGoal) :- !,
        defunc_nrf_args_of(Goal, Goal1, NewGoal, QM:Goal1),
        NewGoal \== QM:Goal.
defunc_goal(Goal, NewGoal) :-
        defunc_nrf_args_of(Goal, Goal1, NewGoal, Goal1),
        NewGoal \== Goal.

take_out_arg(A, G, X, NG) :-
        arg(A, G, Ag),
        A1 is A-1,
        ( X == Ag ->
            copy_args(A1, G, NG)
        ;
            arg(A1, NG, Ag),
            take_out_arg(A1, G, X, NG)
        ).

is_evaluable('\6\Arit'(_)).
is_evaluable('\6\Eval'(_,_)).
is_evaluable('\6\Opts'(_,_)).
is_evaluable('\6\Cond'(_,_)).
is_evaluable('\6\Predabs'(_)).

% todo: Ask the compiler which terms are arithmetic expressions, do not place a table here
arith_exp(-(_)).
arith_exp(+(_)).
arith_exp(--(_)).
arith_exp(++(_)).
arith_exp(+(_,_)).
arith_exp(-(_,_)).
arith_exp(*(_,_)).
arith_exp(/(_,_)).
arith_exp(//(_,_)).
arith_exp(rem(_,_)).
arith_exp(mod(_,_)).
arith_exp(#(_,_)).
arith_exp(/\(_,_)).
arith_exp(\/(_,_)).
arith_exp(\(_)).
arith_exp(<<(_,_)).
arith_exp(>>(_,_)).
arith_exp(integer(_)).
arith_exp(truncate(_)).
arith_exp(float(_)).
arith_exp(gcd(_,_)).
arith_exp(abs(_)).
arith_exp(sign(_)).
arith_exp(float_integer_part(_)).
arith_exp(float_fractional_part(_)).
arith_exp(floor(_)).
arith_exp(round(_)).
arith_exp(ceiling(_)).
arith_exp(**(_,_)).
arith_exp(exp(_)).
arith_exp(log(_)).
arith_exp(sqrt(_)).
arith_exp(sin(_)).
arith_exp(cos(_)).
arith_exp(atan(_)).

make_fun_eval(P, Mod, QM) :-
        current_fact(fun_eval(P, Mod, QM)), !.
make_fun_eval(P, Mod, QM) :-
        asserta_fact(fun_eval(P, Mod, QM)).

make_tmp_fun_eval(Fun, Mod, Ref) :-
        defined_functions(Mod), !,
        functor(Fun, F, A),
        functor(P, F, A),
        ( current_fact(fun_eval(P, Mod, (-))) ->
            Ref = []
        ;
            asserta_fact(fun_eval(P, Mod, (-)), Ref)
        ).
make_tmp_fun_eval(_F, _Mod, []).

delt_tmp_fun_eval([]) :- !.
delt_tmp_fun_eval(Ref) :- erase(Ref).

function_output_arg(QM:FSpec, Fun, Arg, QM) :-
        nonvar(QM), !,
        function_output_arg(FSpec, Fun, Arg, (-)).
function_output_arg(FSpec, Fun, Arg, (-)) :-
        functor(FSpec, F, A),
        has_tilde(A, FSpec, Arg),
        A1 is A-1,
        functor(Fun, F, A1).

has_tilde(N, F, Arg) :-
        arg(N, F, F_N),
        N1 is N-1,
        ( F_N == (~) ->
            Arg = N,
            not_tilde(N1, F)
        ; has_tilde(N1, F, Arg)
        ).

not_tilde(0, _) :- !.
not_tilde(A, F) :-
        arg(A, F, F_A),
        F_A \== (~), !,
        A1 is A-1,
        not_tilde(A1, F).
not_tilde(_, F) :-
        error(['More than one "~" marking function return argument in ',F]).

del_last_true(true, true).
del_last_true((G, Gs), NG) :-
        del_last_true_(Gs, G, NG).

del_last_true_(true, G, G).
del_last_true_((G,Gs), G0, (G0,NG)) :-
        del_last_true_(Gs, G, NG).

