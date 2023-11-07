:- module(hiord_rt, 
    [
        call/1,
        call/2 /* call/N, */, 
        'SYSCALL'/1, 
        '$nodebug_call'/1,
        '$meta_call'/1,
        '$meta_exp'/3,
        this_module/1
    ],[assertions, nortchecks, isomodes]).

:- doc(title,"Higher-order support").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Jose F. Morales (improvements)").

:- doc(module,"This module is a wrapper for the
   implementation-defined predicate @pred{call/1}, and it implements
   the @pred{call/2} predicate.").

:- doc(call(G), "Executes goal @var{G}, restricting the scope of
   the cuts to the execution of @var{G}.  Equivalent to writing a
   variable @var{G} in a goal position.").

:- trust pred call(+cgoal) + (iso, native). 
:- primitive_meta_predicate(call(goal)).
:- impl_defined(call/1).

:- doc(call(Pred,Arg1), "There exists a set of builtin predicates
   of the form @pred{call/N} with @tt{N > 1} which execute predicate
   @var{Pred} given arguments @var{Arg1} ... @var{ArgX}. If @var{Pred}
   has already arguments @var{Arg1} is added to the start, the rest to
   the end. This predicate, when @var{Pred} is a variable, can be
   written using the special Ciao syntax @tt{Pred(Arg1,...,ArgX)}.").


% Won't get checked due to throw/1 in the first clause of calln/2.
:- pred call(+cgoal,?term) + native. 

% :- primitive_meta_predicate(call(pred(1),?)).

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


:- trust pred 'SYSCALL'(+cgoal).
:- impl_defined('SYSCALL'/1).


:- trust pred '$nodebug_call'(+cgoal).
:- primitive_meta_predicate('$nodebug_call'(goal)).
:- impl_defined('$nodebug_call'/1).

:- trust pred '$meta_call'(+A) : cgoal(A) + native(call(A)).
:- impl_defined('$meta_call'/1).

:- trust pred '$meta_exp'(+Metatype, +P, -E) # "Unify @var{E} with the
   metaexpansion of @var{P} with metatype @var{Metatype} (using static
   metaexpansion when possible and dynamic metaexpansion when
   necessary)".
:- impl_defined('$meta_exp'/3).

:- meta_predicate this_module(addmodule).
:- impl_defined(this_module/1). % TODO: avoid problems with addmodule and export

this_module(M, M).

:- trust pred this_module(Module) => internal_module_id #
    "@var{Module} is the internal module identifier for current module.".

