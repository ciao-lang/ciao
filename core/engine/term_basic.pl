:- module(term_basic,
        [(=)/2, (\=)/2, arg/3, functor/3, (=..)/2, non_empty_list/1,
         copy_term/2, copy_term_nat/2, cyclic_term/1, 'C'/3],
        [assertions, nativeprops, isomodes, nortchecks]).

:- doc(title,"Basic term manipulation").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This module provides basic term manipulation.").


:- pred copy_term(Term, Copy) + ( sideff(free), native, iso )
        # "@var{Copy} is a renaming of @var{Term}, such that brand new
           variables have been substituted for all variables in
           @var{Term}.  If any of the variables of @var{Term} have
           @concept{attributes}, the copied variables will have copies
           of the attributes as well. It behaves as if defined by:

@begin{verbatim}
:- data 'copy of'/1.

copy_term(X, Y) :-
        asserta_fact('copy of'(X)),
        retract_fact('copy of'(Y)).
@end{verbatim}".

:- true comp copy_term(Term, Copy) : ground(Term) + eval.

:- impl_defined(copy_term/2).

:- trust pred copy_term_nat(Term, Copy) + ( sideff(free) )
        # "Same as @pred{copy_term/2}, except that attributes of
          variables are not copied.".

:- impl_defined(copy_term_nat/2).

% Compiled inline -- these provide hooks for the interpreter and comments.

:- trust comp '='(X,Y) + (sideff(free), native, iso, eval, is_det,
	relations(inf), test_type(unification))
	# "@var{X} and @var{Y} unify.". 
:- true prop '='/2.

X=Y :- X=Y.

:- trust pred \=(X,Y) + (sideff(free), bind_ins, iso, is_det)
	# "@var{X} and @var{Y} are not unifiable.". 

:- trust comp \=(X,Y) : (ground(X) , ground(Y)) + eval.
:- trust comp \=(X,Y) + sideff(free).

X \= X :- !, fail.
_ \= _.

:- trust pred arg(ArgNo, Term, Arg) : num(ArgNo) + (is_det, relations(inf)).
:- trust pred arg(ArgNo, Term, Arg) : (num(ArgNo), gnd(Term)) => gnd(Arg).

:- trust pred arg(ArgNo,Term,Arg) : (nnegint(ArgNo), struct(Term))
                + ( sideff(free), native, iso, bind_ins)
	# "Argument @var{ArgNo} of the term @var{Term} is @var{Arg}.".

:- trust pred arg(ArgNo, Term, Arg) : (nnegint(ArgNo), gnd(Term)) =>
	gnd(Arg).
% :- trust pred arg(+ArgNo,+Term,?Arg)
% 	: (nnegint(ArgNo), struct(Term)) + eval.

arg(X, Y, Z) :- arg(X, Y, Z).


:- trust pred functor(@Term,Name,Arity)
	: nonvar(Term) => ( atm(Name), nnegint(Arity) )
	+ ( sideff(free), native, iso, bind_ins, eval ).

:- trust pred functor(Term,+Name,+Arity) =>
	( nonvar(Term), atm(Name), nnegint(Arity) )
	+ ( sideff(free), native, iso, bind_ins, eval, not_fails )
        # "The principal functor of the  term @var{Term} has name @var{Name}
           and arity @var{Arity}.".

% This trust contains an undefined metric (arity), this will test that
% the resource analysis is able to handle this situation. --EMM

:- trust pred functor(+Term, -Name, -Arity) : nonvar(Term) =>
	( size(Name, 1), size(Arity, arity(Term)) ) + size_metric(Term, arity).

:- trust comp functor/3 + ( sideff(free), native, iso, is_det ).

functor(X, Y, Z) :- functor(X, Y, Z).


:- trust pred (?Term =.. ?List) => list(List) + ( sideff(free), native, iso )
	# "The functor and arguments of the term @var{Term} comprise the
           list @var{List}.".
:- true comp (+ =.. ?)  + eval.
:- true comp (_ =.. List) : (list(List),const_head(List)) + eval.

X=..Y :- X=..Y.

:- export(const_head/1).
:- prop const_head/1.

const_head([Head|_]):-
	constant(Head).

:- trust pred 'C'(S1,Terminal,S2) => list_functor(S1).
:- trust success 'C'(S1,Terminal,S2) : non_empty_list(S1) =>  list(S2).
:- trust pred 'C'(?S1,?Terminal,?S2) + ( sideff(free), native )
	# "@var{S1} is connected by the terminal @var{Terminal} to @var{S2}.
           Internally used in @em{DCG grammar rules}. Defined as if by the 
           single clause: @tt{'C'([X|S], X, S).}".

:- true comp 'C'(+,?,?) + eval.

'C'(X, Y, Z) :- 'C'(X, Y, Z).

:- export(list_functor/1).
:- prop list_functor(A) + regtype.

list_functor([A|B]):-
	term(A),
	term(B).

:- prop non_empty_list(A) + regtype # "A list that is not the empty
	list [].".

non_empty_list([_|B]):-
	list(B).

:- pred cyclic_term(T) # "True if @var{T} is cyclic (infinite).".

:- impl_defined(cyclic_term/1). 

