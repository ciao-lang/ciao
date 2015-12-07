:- use_module(library(terms_vars), [varsbag/3]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(lists),      [length/2, append/3, nocontainsx/2]).

:- set_prolog_flag(multi_arity_warnings, off).

compile_constr(Type, Constr) -->
	{
	    copy_term(Constr, Diff),
	    normalize(Diff, K, I, H), % exec
	    solve_constr(Type, K, I, H), % exec
	    varsbag(Constr, Vs, []),
	    copy_term([Constr|Vs], [Constr_|Vs_]),
	    dump_internal(Diff, Constr_, Cs)
	},
	ctidy(Cs),
	explicit_unifs(Vs_, Vs, []),
	!.
compile_constr(_, _) -->
	[fail].

solve_constr(.<>., K, I, H) :- var_with_def(_, nz, K, I, H).
solve_constr(.=.,  _, I, H) :- solve_lin(H, I).
solve_constr(.<.,  _, I, H) :- solve_ineq_lt(H, I).
solve_constr(.=<., _, I, H) :- solve_ineq_le(H, I).

explicit_unifs([],      _,       _) --> [].
explicit_unifs([V1|L1], [V2|L2], Sofar) -->
	( {var(V1), nocontainsx(Sofar, V1)} -> % Free independent variable
	    {V2 = V1}
	; [V2 = V1]
	),
	explicit_unifs(L1, L2, [V1|Sofar]).


% ----------------------------------------------------------------------

compile_curried(Args, H) -->
	{
	    %
	    % the limit must correspond with solve_generic_N/K
	    %
	    build_call(5, solve_generic, Args, H, Call)
	},
	[Call].

% reduce the amount of structure passing
%
build_call(Limit, Name, Args, H, Call) :-
	length(H, Len),
	( Len =< Limit ->
	    flatten_h(H, Hf, []),
	    number_codes(Len, Lencs),
	    atom_codes(LenAtm, Lencs),
	    atom_concat([Name, '_', LenAtm], Fullname),
	    append(Args, Hf, A),
	    Call =.. [Fullname|A]
	;
	    atom_concat(Name, '_n', Fullname),
	    append(Args, [H], A),
	    Call =.. [Fullname, Len|A]
	).

flatten_h([]) --> [].
flatten_h([V*K|Vs]) --> [V, K], flatten_h(Vs).

% ------------------------------- compiler dumper -----------------------------
/*
% DCG output of nonlinear stuff
%
compile_normalize( Exp, Ic, Hc) -->
  {
    copy_term( Exp, Copy), 		% isolate
    normalize( Copy, I, H),
    dump_internal( Copy-I-H, Exp-Ic-Hc, Cs)
  },
  ctidy( Cs).
*/
% Compiler backend for the dumper
% Here we would only have to dump frozen_system
%
ctidy([]) --> [].
ctidy([C|Cs]) -->
	( {C = eqs(_, Dep, Indep)},
	    ctidy_deps(Dep),
	    ctidy_indeps(Indep)
	; {C = cva(Var, Constr)},
	    ctidy(Constr, Var)
	),
	ctidy(Cs).

ctidy(float(F), V) -->
	% {V = F}. % exec
	[attach_attribute(X, float(F)), X = V, true]. % exec
%
ctidy(clpr_frozen(Goals), V) -->
	ctidy_nonlin(Goals, V).
%
ctidy(cva(Attrib), V) --> % general case
	[attach_attribute(V, Attrib)].

ctidy_indeps([]) --> [].
ctidy_indeps([indep(Var, Type, Nl)|Vs]) -->
	ctidy_indep(Type, Var),
	ctidy_nonlin(Nl, Var),
	ctidy_indeps(Vs).

ctidy_deps([]) --> [].
ctidy_deps([dep(Var, Type, I, H, Nl)|Vs]) -->
	ctidy_dep(Type, I, H, Var),
	ctidy_nonlin(Nl, Var),
	ctidy_deps(Vs).

ctidy_nonlin(none,     _) --> [].
ctidy_nonlin(nl(S, U), Var) -->
	ctidy_frozen_system(S),
	ctidy_frozen_user(U, Var).

ctidy_indep(v,    _) --> [].
ctidy_indep(l(t), V) --> compile_curried([lt, 0], [V*1]).
ctidy_indep(l(e), V) --> compile_curried([le, 0], [V*1]).
ctidy_indep(nz,   V) --> [nonzero(V)]. % Check this!

ctidy_dep(v, I, H, V) -->
	ctidy_eq(H, I, V).
ctidy_dep(nz, I, H, V) -->
	ctidy_neq(H, I, V).
ctidy_dep(l(Type), I, H, _) -->
	ctidy_inequality(Type, H, I).

ctidy_inequality(e, H, I) -->
	compile_curried([le, I], H).
ctidy_inequality(t, H, I) -->
	compile_curried([lt, I], H).

ctidy_frozen_system(true) --> !, [].
ctidy_frozen_system((A, B)) --> !,
	ctidy_frozen_system(A),
	ctidy_frozen_system(B).
ctidy_frozen_system(Goal) -->
	( {
		arg(1, Goal, Mutex),
		var(Mutex),
		Mutex = dumped
	    } ->
	    ctidy_nonlin(Goal)
	;
	    []
	).

ctidy_frozen_user(true, _) --> !, [].
ctidy_frozen_user(Goal, V) --> [clpr_freeze(V, Goal)].

% For the code below, please see the comment for the predicate 
% tidy_frozen_system_op/4 in file clp_dump.pl

%% Code for CLPQ
ctidy_nonlin('solver_q:solve_abs'(_, A, B, _)) --> [solve_abs(A, B)].
ctidy_nonlin('solver_q:solve_sign'(_, A, B, _)) --> [solve_sign(A, B)].
ctidy_nonlin('solver_q:solve_mix'(_, Mix, A, B, C, _)) -->
	[solve_mix(Mix, A, B, C)].
ctidy_nonlin('solver_q:solve_mult'(_, A,    B, C, _)) --> [solve_mult(A, B, C)].
ctidy_nonlin('solver_q:solve_pow'(_,  A,    B, C, _)) --> [solve_pow(A, B, C)].
ctidy_nonlin('solver_q:solve_trig'(_, Trig, A, B, _)) -->
	[solve_trig(Trig, A, B)].

%% Code for CLPR
ctidy_nonlin('solver_r:solve_abs'(_, A, B, _)) --> [solve_abs(A, B)].
ctidy_nonlin('solver_r:solve_sign'(_, A, B, _)) --> [solve_sign(A, B)].
ctidy_nonlin('solver_r:solve_mix'(_, Mix, A, B, C, _)) -->
	[solve_mix(Mix, A, B, C)].
ctidy_nonlin('solver_r:solve_mult'(_, A,    B, C, _)) --> [solve_mult(A, B, C)].
ctidy_nonlin('solver_r:solve_pow'(_,  A,    B, C, _)) --> [solve_pow(A, B, C)].
ctidy_nonlin('solver_r:solve_trig'(_, Trig, A, B, _)) -->
	[solve_trig(Trig, A, B)].

ctidy_eq([X*K], I, V) -->
	{
	    arith_zero(I),
	    arith_eval(1=:=K),
	    !,
	    X = V
	}.
ctidy_eq(H, I, V) -->
	compile_curried([eq, I], [V* -1|H]).

%
% this pattern is produced by A=B/C
%
ctidy_neq([X*K], I, V) -->
	{
	    arith_zero(I),
	    arith_eval(1=:=K),
	    !,
	    X = V
	},
	[nonzero(V)].
ctidy_neq(H, I, V) -->
	compile_curried([nz(V), I], H).
