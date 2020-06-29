:- module(basic_props,
    [term/1, int/1, nnegint/1, flt/1, num/1, atm/1, struct/1,
     gnd/1, gndstr/1, constant/1,
     callable/1, internal_module_id/1,
     operator_specifier/1, list/1, list/2, nlist/2, member/2,
     sequence/2, sequence_or_list/2,
     character_code/1, string/1,
     bytelist/1,
     % byte/1, in_byte/1,
     predname/1, atm_or_atm_list/1, compat/2, inst/2,
     iso/1, deprecated/1, not_further_inst/2, sideff/2, regtype/1,
     native/1, native/2, rtcheck/1, rtcheck/2, no_rtcheck/1, eval/1,
     equiv/2, bind_ins/1, error_free/1, memo/1, filter/2, flag_values/1,
     pe_type/1 ],
    [assertions, nortchecks, nativeprops]).

:- doc(title,"Basic data types and properties").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "This module is automatically imported by the
   @lib{assertions} package. It can be imported explicitly with the
   @tt{:- use_module(engine(basic_props))} directive.").

:- doc(module,"@cindex{properties, basic} This library contains
   the set of basic properties used by the builtin predicates, and
   which constitute the basic data types and properties of the
   language. They are intented to be used as properties in assertions (for both
   runtime and compile time checking).

   Note that most of those properties can be used directly from goals
   as type testing builtins, and they should be correct when terms are
   sufficiently instantiated. Calling with uninstantiated terms have
   undefined behaviour (failure or non termination, depending on the
   type). Please use the run-time checking facilities (@pred{inst/2}
   and @pred{compat/2}) to ensure well-defined behaviour. For
   low-level instantiation checks we encourage the use
   @lib{term_typing} builtins.").

%% Commented out to avoid including hiord_rt in all executables,
%% put declarations instead:
%% :- use_package(hiord).
:- set_prolog_flag(read_hiord, on).
:- use_module(engine(hiord_rt), [call/1]).
:- import(hiord_rt, [call/2]). % TODO: import from hiord_rt instead?
:- use_module(library(terms_check), [instance/2]). % for inst/2

:- doc(term/1, "The most general type (includes all possible terms).").

:- trust prop term(X) + (regtype, native) # "@var{X} is any term.".
:- trust comp term(X) + sideff(free).
:- trust comp term(X) + eval.
:- trust comp term(X) + equiv(true).
:- trust success term(_) => true.

term(_).

:- doc(int/1, "The type of integers. The range of integers is
    @tt{[-2^2147483616, 2^2147483616)}.  Thus for all practical
    purposes, the range of integers can be considered infinite.").

:- trust prop int(T) + (regtype, native) # "@var{T} is an integer.".
:- trust comp int(T) + sideff(free).
:- trust comp int(T) : nonvar(T) + (eval, is_det).
:- trust success int(T) => int(T).
:- trust comp int/1 + test_type(arithmetic).

%:- impl_defined(int/1).  % TODO:T279
int(X) :- integer(X).

:- doc(nnegint/1, "The type of non-negative integers, i.e.,
    natural numbers.").

:- trust prop nnegint(T) + ( regtype, native )
    # "@var{T} is a non-negative integer.".
:- trust comp nnegint(T) + sideff(free).
:- trust comp nnegint(T) : nonvar(T) + eval.
:- trust success nnegint(T) => nnegint(T).
:- trust comp nnegint/1 + test_type(arithmetic).

%:- impl_defined(nnegint/1).  % TODO:T279
nnegint(X) :- integer(X), X >= 0.

:- doc(flt/1, "The type of floating-point numbers. The range of
    floats is the one provided by the C @tt{double} type, typically
    @tt{[4.9e-324, 1.8e+308]} (plus or minus).  There are also three
    special values: Infinity, either positive or negative,
    represented as @tt{1.0e1000} and @tt{-1.0e1000}; and
    Not-a-number, which arises as the result of indeterminate
    operations, represented as @tt{0.Nan}").

:- trust prop flt(T) + (regtype, native) # "@var{T} is a float.".
:- trust comp flt(T) + sideff(free).
:- trust comp flt(T) : nonvar(T) + (eval, is_det).
:- trust success flt(T) => flt(T).
:- trust comp flt/1 + test_type(meta).

%:- impl_defined(flt/1).  % TODO:T279
flt(X) :- float(X).

:- doc(num/1, "The type of numbers, that is, integer or floating-point.").

:- trust prop num(T) + (regtype, native) # "@var{T} is a number.".
:- trust comp num(T) + (sideff(free),bind_ins).
:- trust comp num(T) : nonvar(T) + (eval, is_det).
:- trust success num(T) => num(T).
:- trust comp num/1 + test_type(arithmetic).

% :- impl_defined(num/1).  % TODO:T279
num(X) :- number(X).

:- doc(atm/1, "The type of atoms, or non-numeric constants.  The
    size of atoms is unbound.").

:- trust prop atm(T) + (regtype, native) # "@var{T} is an atom.".
:- trust comp atm(T) + sideff(free).
:- trust comp atm(T) : nonvar(T) + (eval, is_det).
:- trust success atm(T) => atm(T).
:- trust comp atm/1 + test_type(arithmetic).

%:- impl_defined(atm/1).  % TODO:T279
atm(X) :- atom(X).

:- doc(struct/1, "The type of compound terms, or terms with
non-zeroary functors. By now there is a limit of 255 arguments.").

% TODO: struct/1 does not seem to be a regtype
%       (see basic_props_rtc:rtc_struct/1 for details).

:- trust prop struct(T) + (regtype, native) # "@var{T} is a compound term.".
:- trust comp struct(T) + sideff(free).
:- trust comp struct(T) : nonvar(T) + eval.
:- trust success struct(T) => struct(T).

% NOTE: instantiation check version, kept for compatibility
%:- impl_defined(struct/1).  % TODO:T279
struct(T) :- nonvar(T), functor(T, _, A), A>0. % compound(T).

% TODO:[new-resources] needed for etermsvar?
% :- export(vr/1).
% :- doc(vr/1, "The type of all variables.").
% 
% :- trust prop vr(T) + (regtype, native) # "@var{T} is a variable.".
% :- trust comp vr(T) + sideff(free).
% :- trust comp vr(T) : var(T) + eval.
% :- trust success vr(T) => vr(T).
% :- trust comp vr/1 + test_type(meta).
% 
% % Should be current var/1
% vr(T) :- var(T).

:- doc(gnd/1, "The type of all terms without variables.").

:- trust prop gnd(T) + (regtype, native) # "@var{T} is ground.".
:- trust comp gnd(T) + sideff(free).
:- trust comp gnd(T) : ground(T) + (eval, is_det).
:- trust success gnd(T) => gnd(T).
:- trust comp gnd/1 + test_type(meta).

%:- impl_defined(gnd/1).  % TODO:T279
% NOTE: instantiation check version, kept for compatibility
gnd(X) :- ground(X).

:- trust prop gndstr(T) + (regtype, native) # "@var{T} is a ground compound term.".
:- trust comp gndstr(T) + sideff(free).
:- trust comp gndstr(T) : ground(T) + (eval, is_det).
:- trust success gndstr(T) => gndstr(T).

gndstr(A) :- gnd(A), struct(A).

:- prop constant(T) + regtype
   # "@var{T} is an atomic term (an atom or a number).".
:- trust comp constant(T) + sideff(free).
:- trust comp constant(T) : nonvar(T) + (eval, is_det).
:- trust success constant(T) => constant(T).

% TODO: atomic?
constant(T) :- atm(T).
constant(T) :- num(T).

:- prop callable(T) + regtype
   # "@var{T} is a term which represents a goal, i.e.,
    an atom or a structure.".
:- trust comp callable(T) + sideff(free).
:- trust comp callable(T) : nonvar(T) + (eval, is_det).
:- trust success callable(T) => nonvar(T).

% TODO: move to other type checks add "+ iso"
callable(T) :- atom(T). % instantiation check version, kept for compatibility
callable(T) :- nonvar(T), functor(T, _, A), A>0. % compound(T).
%callable(T) :- atm(T).
%callable(T) :- struct(T).

% TODO: rename?
:- doc(internal_module_id/1, "For a user file it is a term user/1
    with an argument different for each user file, for
    other modules is just the name of the module (as an atom).").

:- prop internal_module_id(M) + regtype #
    "@var{M} is an internal module identifier".

internal_module_id(user(M)) :- atm(M). % TODO: do in other way?
internal_module_id(M) :- atm(M).

:- doc(operator_specifier/1, "The type and associativity of an
operator is described by the following mnemonic atoms:

@begin{description}

@item{@tt{xfx}} Infix, non-associative: it is a requirement that both of
the two subexpressions which are the arguments of the operator must be
of @em{lower} precedence than the operator itself.

@item{@tt{xfy}} Infix, right-associative: only the first (left-hand)
subexpression must be of lower precedence; the right-hand subexpression
can be of the @em{same} precedence as the main operator.

@item{@tt{yfx}} Infix, left-associative: same as above, but the other
way around.

@item{@tt{fx}} Prefix, non-associative: the subexpression must be of
@em{lower} precedence than the operator.

@item{@tt{fy}} Prefix, associative: the subexpression can be of the
@em{same} precedence as the operator.

@item{@tt{xf}} Postfix, non-associative: the subexpression must be of
@em{lower} precedence than the operator.

@item{@tt{yf}} Postfix, associative: the subexpression can be of the
@em{same} precedence as the operator.

@end{description}
").

:- prop operator_specifier(X) + regtype # "@var{X} specifies the type and
    associativity of an operator.".
:- trust comp operator_specifier(X) + sideff(free).
:- trust comp operator_specifier(X) : nonvar(X) + (eval, is_det, relations(7)).
:- trust success operator_specifier(T) => operator_specifier(T).

operator_specifier(fy).
operator_specifier(fx).
operator_specifier(yfx).
operator_specifier(xfy).
operator_specifier(xfx).
operator_specifier(yf).
operator_specifier(xf).

:- doc(list/1, "A list is formed with successive applications of the
   functor @tt{'.'/2}, and its end is the atom @tt{[]}.  Defined as
   @includedef{list/1}").

:- prop list(L) + regtype # "@var{L} is a list.".
:- trust comp list(L) + sideff(free).
:- trust comp list(L) : ground(L) + (eval, is_det).
:- trust success list(T) => list(T).

list([]).
list([_|L]) :- list(L).

:- doc(list(T,L), "@var{L} is a list, and for all its elements,
   @var{T} holds.").

:- prop list(T,L) + regtype # "@var{L} is a list of @var{T}s.".
:- trust comp list(T,L) + sideff(free).
:- meta_predicate list(pred(1), ?).
:- trust comp list(T,L) : (ground(T),ground(L)) + eval.
:- trust success list(T,X) => list(X). % TODO: should be list(T,X), but does not work

list(_, []).
list(T, [X|Xs]) :-
    T(X),
    list(T, Xs).

:- prop nlist(T,L) + regtype #
    "@var{L} is @var{T} or a nested list of @var{T}s.  Note that
    if @var{T} is term, this type is equivalent to term, this
    fact explains why we do not have an @pred{nlist/1} type.".
:- trust comp nlist(T,L) + sideff(free).
:- meta_predicate nlist(pred(1), ?).
:- trust comp nlist(T,L) : (ground(T),ground(L)) + eval.
:- trust success nlist(T,X) => term(X).

nlist(_, []).
nlist(T, [X|Xs]) :-
    nlist(T, X),
    nlist(T, Xs).
nlist(T, X) :-
    T(X).

:- prop member(X,L) # "@var{X} is an element of @var{L}.".
:- trust comp member(X,L) + (sideff(free), bind_ins).
:- trust comp member(X,L) : list(L) + eval.
:- trust success member(_X,L) => list(L).
:- trust success member(X,L) : ground(L) => ground(X).

member(X, [X|_]).
member(X, [_Y|Xs]):- member(X, Xs).

:- doc(sequence/2, "A sequence is formed with zero, one, or more
   occurrences of the operator @op{','/2}.  For example, @tt{a, b, c} is
   a sequence of three atoms, @tt{a} is a sequence of one atom.").

:- prop sequence(T,S) + regtype # "@var{S} is a sequence of @var{T}s.".
:- trust comp sequence(T,S) + sideff(free).

:- meta_predicate sequence(pred(1), ?).
:- trust comp sequence(T,S) : (ground(T), ground(S)) + eval.
:- trust success sequence(T,E) => (ground(T), nonvar(E)).

sequence(T, E) :- T(E).
sequence(T, (E,S)) :-
    T(E),
    sequence(T,S).

:- prop sequence_or_list(T,S) + regtype
   # "@var{S} is a sequence or list of @var{T}s.".
:- trust comp sequence_or_list(T,S) + sideff(free).
:- meta_predicate sequence_or_list(pred(1), ?).
:- trust comp sequence_or_list(T,S) : (ground(T),ground(S)) + eval.
:- trust success sequence_or_list(T,E) => (ground(T),nonvar(E)).

sequence_or_list(T, E) :- list(T,E).
sequence_or_list(T, E) :- sequence(T, E).

:- prop character_code(T) + regtype
   # "@var{T} is an integer which is a character code.".
:- trust comp character_code(T) + sideff(free).
:- trust comp character_code(T) : nonvar(T) + eval.
:- trust success character_code(I) => character_code(I).

% TODO: fix range, 0-0x10FFFF
character_code(I) :- int(I).

:- doc(string/1, "A string is a list of character codes.  The usual
    syntax for strings @tt{\"string\"} is allowed, which is
    equivalent to @tt{[0's,0't,0'r,0'i,0'n,0'g]} or
    @tt{[115,116,114,105,110,103]}.  There is also a special Ciao
    syntax when the list is not complete: @tt{\"st\"||R} is
    equivalent to @tt{[0's,0't|R]}.").

:- prop string(T) + regtype
   # "@var{T} is a string (a list of character codes).".
:- trust comp string(T) + sideff(free).
:- trust comp string(T) : ground(T) + eval.
:- trust success string(T) => string(T).

string(T) :- list(character_code, T). % TODO: fix range of character_code

:- prop bytelist(T) + regtype
   # "@var{T} is list of bytes.".
:- trust comp bytelist(T) + sideff(free).
:- trust comp bytelist(T) : ground(T) + eval.
:- trust success bytelist(T) => bytelist(T).

bytelist(T) :- list(int, T). % TODO: use byte/1 instead of int/1

% The following is commented out because we do not know the impact on analysis.
% TODO: To be decommented together with assertions about put_code and
%       byte_code in io_basic.pl.
% 
% :- prop byte(T) + regtype
%    # "@var{T} is byte.".
% :- trust comp byte(T) + sideff(free).
% :- trust comp byte(T) : nonvar(T) + eval.
% :- trust success byte(I) => byte(I).
% :- trust success byte(I) => int(I).
%
% byte(I) :- between(0, 0xff, I).
%
% :- prop in_byte(T) + regtype
%    # "@var{T} is a byte or the integer.".
% :- trust comp in_byte(T) + sideff(free).
% :- trust comp in_byte(T) : nonvar(T) + eval.
% :- trust success in_byte(I) => in_byte(I).
% :- trust success in_byte(I) => int(I).
%
% in_byte(I) :- byte(I).
% in_byte(-1).


:- doc(predname(P),"@var{P} is a Name/Arity structure denoting
    a predicate name: @includedef{predname/1}").

:- prop predname(P) + regtype # "@var{P} is a predicate name.".
:- trust comp predname(P) + sideff(free).
:- trust comp predname(P) : ground(P) + eval.
:- trust success predname(P) => predname(P).

predname(P/A) :-
    atm(P),
    nnegint(A).

:- prop atm_or_atm_list(T) + regtype
   # "@var{T} is an atom or a list of atoms.".
:- trust comp atm_or_atm_list(T) + sideff(free).
:- trust comp atm_or_atm_list(T) : ground(T) + eval.
:- trust success atm_or_atm_list(T) => atm_or_atm_list(T).

atm_or_atm_list(T) :- atm(T).
atm_or_atm_list(T) :- list(atm, T).


:- doc(compat/2,"This property captures the notion of type or
   @concept{property compatibility}. The instantiation or constraint
   state of the term is compatible with the given property, in the
   sense that assuming that imposing that property on the term does
   not render the store inconsistent. For example, terms @tt{X} (i.e.,
   a free variable), @tt{[Y|Z]}, and @tt{[Y,Z]} are all compatible
   with the regular type @pred{list/1}, whereas the terms @tt{f(a)}
   and @tt{[1|2]} are not.").

:- prop compat(Term,Prop)
   # "@var{Term} is @em{compatible} with @var{Prop}.".
:- meta_predicate compat(?, pred(1)).
% not complety sure that assertiong below is completely correct,
% unless side effects analysis understand pred(1) (metacalls).
%:- trust comp compat(Term,Prop) + sideff(free).
:- trust comp compat(Term,Prop) : (ground(Term),ground(Prop)) + eval.

:- meta_predicate compat(?,pred(1)).

compat(T, P) :- \+ \+ P(T).

% No comment necessary: it is taken care of specially anyway in the
% automatic documenter. (PBC: I guess this comment refers to compat/2)


:- prop inst(Term,Prop)
    # "@var{Term} is instantiated enough to satisfy @var{Prop}.".
:- trust comp inst(Term,Prop) + sideff(free).
:- trust comp inst(Term,Prop) : (ground(Term),ground(Prop)) + eval.

:- meta_predicate inst(?,pred(1)).

inst( X , Prop ) :-
    A = Prop( X ),
    copy_term( A , AC ),
    AC,
    instance( A , AC ).


:- prop iso(G) # "@em{Complies with the ISO-Prolog standard.}".
:- trust comp iso(G) + sideff(free).

:- meta_predicate iso(goal).

iso(Goal) :- call(Goal).

:- doc(deprecated/1, "Specifies that the predicate marked with
   this global property has been deprecated, i.e., its use is not
   recommended any more since it will be deleted at a future
   date. Typically this is done because its functionality has been
   superseded by another predicate.").

:- prop deprecated(G) # "@bf{DEPRECATED.}".
:- trust comp deprecated(G) + sideff(free).

:- meta_predicate deprecated(goal).

deprecated(Goal) :- call(Goal).

:- prop rtc_status(S) + regtype # "@var{S} is the status of the
    runtime-check implementation for a given property. Valid
    values are:
 @begin{itemize}

 @item complete: The implementation of the run-time check for this
             property is complete. i.e., an error is reported
             always if the property is violated. Default.

 @item incomplete: The current run-time check is incomplete, i.e., it
               is possible that no error is reported even if the
               property is violated.

 @item unimplemented: No run-time checker has been implemented (yet)
                  for the property.

 @item unknown: It has not been determined yet whether the current
            implementation of the run-time checker is complete or
            not.

 @item impossible: The property cannot be checked at run time (for
               theoretical or practical reasons).

 @end{itemize}
".

rtc_status(complete). % Was: rtc_status(exhaustive).
rtc_status(incomplete).
rtc_status(unimplemented).
rtc_status(unknown).
rtc_status(impossible).

:- prop rtcheck(G, Status) : callable * rtc_status # "The runtime
    check of this property is @var{Status}.".
:- trust comp rtcheck(G, Status) + sideff(free).

:- meta_predicate rtcheck(goal, ?).

rtcheck(Goal, _) :- call(Goal).

:- prop rtcheck(G) : callable # "Equivalent to rtcheck(G, 
    complete).".
:- trust comp rtcheck(G) + sideff(free).

:- meta_predicate rtcheck(goal).

rtcheck(Goal) :- rtcheck(Goal, complete).

:- doc(no_rtcheck/1,"This comp pseudo-property is used to declare that
    the assertion in which it appears should not be checked at
    run-time.  Equivalent to @tt{rtcheck(G, S)} with @tt{S}
    unimplemented, impossible, etc.").

:- prop no_rtcheck(G) : callable # "@var{G} is not checked during
   run-time checking.".
:- trust comp no_rtcheck(G) + sideff(free).

:- meta_predicate no_rtcheck(goal).

no_rtcheck(Goal) :- rtcheck(Goal, impossible).

:- prop not_further_inst(G,V)
    # "@var{V} is not further instantiated.". % by the predicate
:- trust comp not_further_inst(G,V) + (sideff(free), no_rtcheck).

:- meta_predicate not_further_inst(goal, ?).

not_further_inst(Goal, _) :- call(Goal).

:- doc(sideff(G,X),"Declares that @var{G} is side-effect free
   (if its execution has no observable result other than its success,
   its failure, or its abortion), soft (if its execution may have other
   observable results which, however, do not affect subsequent execution,
   e.g., input/output), or hard (e.g., assert/retract).").

:- prop sideff(G,X) : (callable(G), member(X,[free,soft,hard]))
# "@var{G} is side-effect @var{X}.".
:- trust comp sideff(G,X) + (native, sideff(free), no_rtcheck).

:- meta_predicate sideff(goal,?).

sideff(Goal, _) :- call(Goal).

% Built-in in CiaoPP
:- prop regtype(G) # "Defines a regular type.".
:- trust comp regtype(G) + sideff(free).
:- meta_predicate regtype(goal).

regtype(Goal) :- call(Goal).

% Built-in in CiaoPP
:- prop native(Pred,Key)
   # "This predicate is understood natively by CiaoPP as @var{Key}.".
%%   # "Predicate @var{Pred} is understood natively by CiaoPP as @var{Key}.".
:- trust comp native(P,K) + sideff(free).
:- meta_predicate native(goal,?).

native(Goal, _) :- call(Goal).

% Built-in in CiaoPP
:- prop native(Pred)
   # "This predicate is understood natively by CiaoPP.".
%%   # "Predicate @var{Pred} is understood natively by CiaoPP.".
:- trust comp native(P) + sideff(free).
:- meta_predicate native(goal).

native(X) :- native(X, X).

:- prop eval(Goal) # "@var{Goal} is evaluable at compile-time.".

:- meta_predicate eval(goal).
eval(Goal) :- call(Goal).

:- prop equiv(Goal1,Goal2)
    # "@var{Goal1} is equivalent to @var{Goal2}.".

:- meta_predicate equiv(goal,goal).

equiv(Goal, _) :- call(Goal).

:- prop bind_ins(Goal) # "@var{Goal} is binding insensitive.".

:- meta_predicate bind_ins(goal).
bind_ins(Goal) :- call(Goal).

:- prop error_free(Goal) # "@var{Goal} is error free.".

:- meta_predicate error_free(goal).
error_free(Goal) :- call(Goal).

:- prop memo(Goal) # "@var{Goal} should be memoized (not unfolded).".

:- meta_predicate memo(goal).
memo(Goal) :- call(Goal).

:- prop filter(Vars,Goal) # "@var{Vars} should be filtered during
    global control).".

filter(Goal, _) :- call(Goal).

:- prop flag_values(X) + regtype # "Define the valid flag values".

% TODO: seems wrong?
flag_values(atom).
flag_values(integer).
flag_values(L):- list(atm,L).

:- prop pe_type(Goal) # "@var{Goal} will be filtered in partial
    evaluation time according to the PE types defined in the
    assertion.".

:- meta_predicate pe_type(goal).
pe_type(Goal) :- call(Goal).
