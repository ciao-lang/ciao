% Op declarations.
:- module(operators, [
        op/3, current_op/3,
        % These used by read, write
        current_prefixop/3, current_infixop/4, current_postfixop/3,
	standard_ops/0],
	[assertions,isomodes]).

:- doc(title, "Defining operators").

:- doc(author, "Daniel Cabeza (modifications and documentation,"||
	           " adapted from SICStus 0.6 code)").
:- doc(author, "Manuel Carro (modifications and documentation)").

:- doc(module, "Operators allow writing terms in a more clear way
   than the standard functional notation.  Standard operators in Ciao
   are defined by this predicate (but note that the compiler itself
   defines more operators at compile time):
   @includedef{standard_ops/0}").

:- data current_op/5.

:- doc(op(Precedence,Type,Name), "Declares the atom @var{Name} to be
       an operator of the stated @var{Type} and @var{Precedence} (0 =<
       @var{Precedence} =< 1200).  @var{Name} may also be a list of
       atoms in which case all of them are declared to be operators.  If
       @var{Precedence} is 0 then the operator properties of @var{Name}
       (if any) are cancelled.  Note that, unlike in
       @concept{ISO-Prolog}, it is allowed to define two operators with
       the same name, one infix and the other postfix.").

:- pred op(+int,+operator_specifier,+atm_or_atm_list) + (iso, native).


op(X, Y, ','):- 
	(X \= 1000 ; Y \= xfy), 
	throw(error(permission_error(modify, operator, ','), op/3)). 
op(Prec, Ass, Ops) :-
	nonvar(Ass),
	integer(Prec), 0=<Prec, Prec=<1200,
	op_ass(Ass, Left, Prec, Right, Type),
	op_atoms(Ops, Atoms), !,
	do_ops(Atoms, Left, Prec, Right, Type).
op(Prec, _, _) :- var(Prec), !,
        throw(error(instantiation_error, op/3-1)).
op(Prec, _, _) :-
        (\+ integer(Prec) ; Prec < 0 ; Prec > 1200), !,
        throw(error(domain_error(operator_priority, Prec), op/3-1)).
op(_, Ass, _) :- var(Ass), !,
        throw(error(instantiation_error, op/3-2)).
op(_, Ass, _) :-
        \+ operator_specifier(Ass), !,
        throw(error(domain_error(operator_specifier, Ass), op/3-2)).
op(_, _, Ops) :- var(Ops), !,
        throw(error(instantiation_error, op/3-3)).
op(_, _, Ops) :-
        error_in_ops(Ops).

error_in_ops([X|Xs]) :-
        ( var(X) ->
              throw(error(instantiation_error, op/3-3))
        ; atom(X) ->
              error_in_ops(Xs)
        ; throw(error(type_error(atom,X), op/3-3))
        ).
error_in_ops(X) :-
        throw(error(type_error(list,X), op/3-3)).

op_atoms(X, [X]) :- atom(X).
op_atoms(Ops, Atoms) :- op_atoms_(Ops, Atoms).

op_atoms_([], []) :- !.
op_atoms_([X|Xs], [X|Ys]) :- !, atom(X), op_atoms_(Xs, Ys).

do_ops([], _, _, _, _).
do_ops([X|Xs], Left, Prec, Right, Type) :-
	( retractall_fact(current_op(X,_,_,_,Type)),
          Prec>0,
          assertz_fact(current_op(X,Left,Prec,Right,Type)),
          fail
	; true
	),
	do_ops(Xs, Left, Prec, Right, Type).

:- doc(current_op(Precedence,Type,Op), "The atom @var{Op} is
   currently an operator of type @var{Type} and precedence
   @var{Precedence}.  Neither @var{Op} nor the other arguments need be
   instantiated at the time of the call; i.e., this predicate can be
   used to generate as well as to test.").

:- pred current_op(?int,?operator_specifier,?atm) + (iso, native).

current_op(Prec, Ass, Op) :-
	current_fact(current_op(Op,Left,Prec,Right,Type)),
	op_ass(Ass, Left, Prec, Right, Type).

:- doc(current_prefixop(Op, Less, Precedence), "Similar to
@pred{current_op/3}, but it concerns only the prefix operators.  It
returns @bf{only one solution}.  Not a predicate for general use.").

:- pred current_prefixop(?atm,?int,?int) + native.

current_prefixop(Op, Less, Prec) :-
	current_fact(current_op(Op,0,Less,Prec,pre)), !.

:- doc(current_infixop(Op, LeftLess, Prec, RightLess), "Similar to
@pred{current_op/3}, but it concerns only infix operators.  It returns
@bf{only one solution}.  Not a predicate for general use.").

:- pred current_infixop(?atm,?int,?int,?int) + native.

current_infixop(Op, Left, Prec, Right) :-
	current_fact(current_op(Op,Left,Prec,Right,in)), !.

:- doc(current_postfixop(Op, Less, Precedence), "Similar to
@pred{current_op/3}, but it concerns only the postfix operators.  It
returns @bf{only one solution}.  Not a predicate for general use.").

:- pred current_postfixop(?atm,?int,?int) + native.

current_postfixop(Op, Prec, Less) :-
	current_fact(current_op(Op,Prec,Less,0,post)), !.

op_ass(fy, 0, Prec, Prec, pre).
op_ass(fx, 0, Prec, Less, pre) :- Less is Prec-1.
op_ass(yfx, Prec, Prec, Less, in) :- Less is Prec-1.
op_ass(xfy, Less, Prec, Prec, in) :- Less is Prec-1.
op_ass(xfx, Less, Prec, Less, in) :- Less is Prec-1.
op_ass(yf, Prec, Prec, 0, post).
op_ass(xf, Less, Prec, 0, post) :- Less is Prec-1.

standard_ops :-
	op(1200, xfx,[(:-)]),
	op(1200,  fx,[(:-),(?-)]),
	op(1100, xfy,[';']),
	op(1050, xfy,['->']),
	op(1000, xfy,[',']),
%        op(1000, xfy,[(&&)]),  % used in ite2akl.pl
%        op( 950, xfy,[(&),(\&)]),
%        op( 950, xf, [(&)]),
	op( 900,  fy,[(\+)]),
	op( 700, xfx,[(=),(\=),(==),(\==),(@<),(@>),(@=<),(@>=),
                      (=..),(is),(=:=),(=\=),(<),(=<),(>),(>=)]),
	op( 550, xfx,[(:)]),
	op( 500, yfx,[(+),(-),(/\),(\/),(#)]),
        op( 500,  fy,[(++),(--)]),
	op( 400, yfx,[(*),(/),(//),(rem),(mod),(<<),(>>)]),
	op( 200,  fy,[(+),(-),(\)]),
        op( 200, xfx,['**']),
	op( 200, xfy,[(^)]).

:- initialization(standard_ops).

