:- module(regexp_trans, [pattern_unification/2], []).

% TODO: This may be a special case of functional notation (JFMC)

pattern_unification(:-(Head,Body), :-(NewHead,NewBody)) :-
	!, functor(Head,Name,N),
        functor(NewHead,Name,N),
        find_pattern_unification(Head,N,NewHead,Body,NewBody).

pattern_unification(Fact, NewClause) :-
	!, functor(Fact,Name,N),
        functor(NewFact,Name,N),
	find_first_pattern_unification(Fact,N,NewFact,NCont,Body),
	(
	    nonvar(Body)->
	    find_pattern_unification(Fact,NCont,NewFact,Body,NewBody),
	    NewClause = :-(NewFact,NewBody)
	;
	    NewClause = NewFact
	).

find_first_pattern_unification(_,0,_,_,_) :- !.
find_first_pattern_unification(Fact,N,NewFact,NCont,Body) :-
	arg(N,Fact,Arg),
        (
            nonvar(Arg), Arg = =~(Pattern) ->
            arg(N,NewFact,X),
	    compleat_body(Pattern,X,yes,_,Body),
	    NCont is N - 1
        ;
            arg(N,NewFact,Arg),
	    N1 is N - 1,
	    find_first_pattern_unification(Fact,N1,NewFact,NCont,Body)
        ).


find_pattern_unification(_,0,_,Body,Body) :- !.
find_pattern_unification(Head,N,NewHead,Body,NewBody) :-
        arg(N,Head,Arg),
        (
            nonvar(Arg), Arg = =~(Pattern) ->
            arg(N,NewHead,X),
	    compleat_body(Pattern,X,no,Body,NewBody2)
        ;
            arg(N,NewHead,Arg),
            NewBody2 = Body
        ),
        N1 is N - 1,
        find_pattern_unification(Head,N1,NewHead,NewBody2,NewBody).

exact(on,[]).
exact(off,_).

create_predicate(shell,Pattern,X,R,match_shell(Pattern,X,R)).
create_predicate(posix,Pattern,X,R,match_posix(Pattern,X,R)).
create_predicate(struct,Pattern,X,R,match(Pattern,X,R)).

first_body(yes,_,P,P).
first_body(no,Body,P,','(P,Body)).

compleat_body(Pattern,X,IsFirstBody,Body,New_Body) :-
	current_prolog_flag(regexp_exact,ValueExact),
	exact(ValueExact,R),
	current_prolog_flag(regexp_format,ValueFormat),
	create_predicate(ValueFormat,Pattern,X,R,P),
	first_body(IsFirstBody,Body,P,New_Body).
