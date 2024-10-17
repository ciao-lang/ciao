:- module(hiord_rt, [], [assertions, nortchecks, isomodes]).

:- doc(title,"Higher-order support").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Jose F. Morales (improvements)").

:- doc(module,"This module is a wrapper for the
   implementation-defined predicate @pred{call/1}, and it implements
   the @pred{call/2} predicate.").

% ---------------------------------------------------------------------------
:- export(call/1).
:- doc(call(G), "Executes goal @var{G}, restricting the scope of
   the cuts to the execution of @var{G}.  Equivalent to writing a
   variable @var{G} in a goal position.").

:- trust pred call(+cgoal) + (iso, native). 
:- meta_predicate call(primitive(goal)).
:- impl_defined(call/1).

% ---------------------------------------------------------------------------
:- export(call/2). % call/N
:- doc(call(Pred,Arg1), "The family of builtin predicates of the form
   @pred{call/N} with @tt{N > 1} execute the predicate @var{Pred}
   applying arguments @var{Arg1} ... @var{ArgN}. If @var{Pred} has
   already @tt{M} arguments, @var{Arg1} ... @var{ArgN} are added to
   the end resulting in a call to a predicate of arity @tt{M+N}. When
   using the @lib{hiord} package, the @tt{Pred(Arg1,...,ArgN)} syntax
   (with @var{Pred} a variable) is accepted as equivalent to
   @tt{call(Pred, Arg1, ..., ArgN)}.").

% Won't get checked due to throw/1 in the first clause of calln/2.
:- pred call(+cgoal,?term) + native. 

% :- meta_predicate call(primitive(pred(1)),?).

call(V, Args) :- calln(V, Args).

calln(V, _) :- var(V), !, throw(error(instantiation_error, call/n-1)).
calln(Pred, Args) :-
    % ShEnv contains actual values while Sh,H,B contain fresh
    % variables. This is needed to avoid copying the whole
    % environment for every call.
    % TODO: merge with fastcall.pl
    Pred = 'PAEnv'(ShEnv,PA),
    copy_term_nat(PA, 'PA'(ShEnv,Args,Goal)), !,
    '$meta_call'(Goal).
calln(Pred, Args) :-
    Pred = 'PAEnv'(_ShEnv,'PA'(_Sh,H,_B)),
    functor(H,'',N),
    functor(Args,_,N), !, % Predicate abstraction OK, argument unif. failed
    fail.
calln(Pred, Args) :-
    Pred = 'PA'(Sh,_H,_B), % TODO: Deprecate this case (without PAEnv)
    copy_term_nat(Pred, 'PA'(Sh,Args,Goal)), !,
    '$meta_call'(Goal).
calln(Pred, Args) :-
    Pred = 'PA'(_Sh,H,_B), % TODO: Deprecate this case (without PAEnv)
    functor(H,'',N),
    functor(Args,_,N), !, % Predicate abstraction OK, argument unif. failed
    fail.
calln(Pred, Args) :-
    functor(Args,_,N),
    throw(error(type_error(pred(N),Pred), call/n-1)).

% ---------------------------------------------------------------------------
:- export('SYSCALL'/1).
:- trust pred 'SYSCALL'(+cgoal).
:- impl_defined('SYSCALL'/1).

% ---------------------------------------------------------------------------
:- export('$nodebug_call'/1).
:- trust pred '$nodebug_call'(+cgoal).
:- meta_predicate '$nodebug_call'(primitive(goal)).
:- impl_defined('$nodebug_call'/1).

% ---------------------------------------------------------------------------
:- export('$meta_call'/1).
:- trust pred '$meta_call'(+A) : cgoal(A) + native(call(A)).
:- impl_defined('$meta_call'/1).

% ---------------------------------------------------------------------------
:- export('$meta_exp'/3).
:- trust pred '$meta_exp'(+Metatype, +P, -E) # "Unify @var{E} with the
   metaexpansion of @var{P} with metatype @var{Metatype} (using static
   metaexpansion when possible and dynamic metaexpansion when
   necessary)".
:- impl_defined('$meta_exp'/3).

% ---------------------------------------------------------------------------
:- export('$is_pa'/1).
% PA is a predicate abstraction
% TODO: define next to callable/1?
'$is_pa'(PA) :-
    nonvar(PA), PA = '$:'(PA1),
    nonvar(PA1), PA1 = 'PAEnv'(_, _).

:- export('$pa_env'/2).
% Extract ShEnv shared environment from a PA:
%  - captured variables for predicate abstractions
%    (e.g., '$meta_exp'(goal, ([X,Y] -> ''(Z) :- append(X,Y,Z), _))) ;
%  - or partial application (unqualified term)
%    (e.g., '$meta_exp'(goal, append(X,Y,Z), _), '$meta_exp'(pred(2), append(X), _), etc.)
% TODO: define next to callable/1?
'$pa_env'(PA, ShEnv) :-
    nonvar(PA), PA = '$:'(PA1),
    nonvar(PA1), PA1 = 'PAEnv'(ShEnv, _).

% ---------------------------------------------------------------------------
:- export(this_module/1).
:- meta_predicate this_module(addmodule).
:- impl_defined(this_module/1). % TODO: avoid problems with addmodule and export

this_module(M, M).

:- trust pred this_module(Module) => internal_module_id #
    "@var{Module} is the internal module identifier for current module.".

