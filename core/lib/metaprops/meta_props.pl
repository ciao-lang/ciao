:- module(meta_props,[call/2, prop/2, regtype/2],[assertions, hiord]).

%:- use_module(engine(basic_props),[callable/1]).

:- doc(title,"Meta-properties").
:- doc(author,"Francisco Bueno").
:- doc(module,"This library allows the use of some meta-constructs
	which provide for specifying properties of terms which are
	unknown at the time of the specification, or expressed with a
	shorthand for the
	property definition, i.e., without really defining it.

        An example of such use is an assertion which specifies that
        any property holding upon call will also hold upon exit:
        @begin{verbatim}
         :- pred p(X) : Prop(X) => Prop(X).
        @end{verbatim}

        Another example is using shorthands for properties when
        documenting:
        @begin{verbatim}
         :- pred p(X) : regtype(X,(^(list;list);list)).
        @end{verbatim}

        (See below for an explanation of such a regular type.)
").

:- doc(usage,"@tt{:- use_module(library(assertions/meta_props))}

   or also as a package @tt{:- use_package(metaprops)}.

   Note the different names of the library and the package.").

:- doc(bug,"Using a hook predicate is not very elegant. Need something
        else.").
:- doc(bug,"The cut in the hook prevents backtracking (enough for most
        uses of properties but not quite ok).").

:- doc(call(P,A),
    "@var{A} has property @var{P} (provided that @var{P} is a property).
     Equivalent to @tt{P(A)}.").
:- true prop call(P,A) : callable(P)
   # "@var{A} has property @var{P}.".

%:- impl_defined(call/2).
% Belongs to basiccontrol, defined as an expansion in the compiler.

call(P,A):- hiord_rt:call(P,A).

:- true prop prop(A,P) : prop(P,^((callable ; prop_abs)))
   # "@var{A} has property @var{P}.".

prop(X,^(T)):- !,
	functor(X,F,A),
	functor(T,F,A),
	prop_(A,X,T).
prop(X,T):-
	callme(T,X).

prop_(0,_X,_T).
prop_(A,X,T):-
	A > 0,
	A1 is A-1,
	arg(A,X,Arg),
	arg(A,T,Type),
	prop(Arg,Type),
	prop_(A1,X,T).

:- true prop regtype(A,T) : prop(T,^((regtype;prop_abs)))
   # "@var{A} is of type @var{T}.".

regtype(A,P):- prop(A,P).

:- doc(doinclude,prop_abs/1).

:- doc(prop_abs(Prop),
	"@var{Prop} is a @index{property abstraction},
	i.e., a @index{parametric property}, or a term formed of
        property abstractions, where the functors used in the term are 
        escaped by @tt{^}.

        One particular case of property abstractions are @index{parametric
        regular type abstractions}, i.e., a parametric type functor or
        a @tt{^}-escaped term formed of regular type abstractions.

        Such abstractions are a short-hand for a corresponding regular type
        (correspondingly, property). For example, the following
        abstraction:
        @begin{verbatim}
         ^(list;list);list
        @end{verbatim}
        denotes terms of the form @tt{(X;Y)} where @tt{list(X)} and
        @tt{list(Y)} hold and also terms @tt{T} such that @tt{list(T)}
        holds. It is equivalent to the regular type:
        @begin{verbatim}
         abstract_type((X;Y)):- list(X), list(Y).
         abstract_type(T):- list(T).
        @end{verbatim}
").
:- push_prolog_flag(unused_pred_warnings, no).
:- prop prop_abs(Prop)
   # "@var{Prop} is a @concept{property abstraction}.".

:- impl_defined(prop_abs/1).
:- pop_prolog_flag(unused_pred_warnings).

:- trust pred callme(A,B) : callable(A).
:- multifile callme/2.

:- doc(callme/2,"(User defined.) A hook predicate you have to define
	as @tt{callme(P,X):- P(X), !.} in the program that uses this
        library. This is done automatically if the package is used
        instead of the library module (but then you @em{should not}
	define @tt{callme/2} in your program).").
