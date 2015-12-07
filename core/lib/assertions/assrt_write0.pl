:- module(assrt_write0,
	[ write_assertion/6,
	  write_assertion_as_comment/6
	],
	[assertions, nortchecks, regtypes]).

:- doc(title,"Pretty-printing assertions").

:- doc(module,"This module defines some predicates which are
   useful for writing assertions in a readable form.").

:- doc(author,"Francisco Bueno Carrillo").

% ISO-Prolog compatibility libraries
:- use_module(library(format)).  

% Other libraries
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(messages)).
:- use_module(library(assertions/assertions_props)).

:- regtype status_flag(F) # "@var{F} is @tt{status} or @tt{nostatus}.".

status_flag(status).
status_flag(nostatus).

:- pred write_assertion(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output.".

write_assertion(Goal,Status,Type,Body,Dict,Flag):-
	write_assertion_(Goal,Status,Type,Body,Dict,Flag,no).

:- pred write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output as
           a Prolog comment.".

write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag):-
	write_assertion_(Goal,Status,Type,Body,Dict,Flag,yes).

write_assertion_(Goal,Status,Type,Body,Dict,Flag,AsComm):-
	unify_vars(Dict),
	( Flag=nostatus
	-> write_nostatus_assertion(AsComm,Type,Goal)
	 ; write_status_assertion(AsComm,Status,Type,Goal)
	),
	assertion_body(Goal,Compat,Call,Succ,Comp,Comm,Body),
	write_if_not_empty(Compat,'::',AsComm,conj),
	decide_on_call(Call,FormC),
	write_if_not_empty(Call,' :',AsComm,FormC),
	decide_on_call(Succ,FormS),
	write_if_not_empty(Succ,'=>',AsComm,FormS),
	decide_on_call(Comp,FormP),
	write_if_not_empty(Comp,' +',AsComm,FormP),
	write_comment(Comm,AsComm),
	format(".~n~n",[]),
	!.
write_assertion_(_Goal,Status,Type,Body,_Dict,_Flag,_AsComm):-
	error_message("Error printing assertion:~n:- ~w ~w ~w~n",
               [Status,Type,Body]),
	fail.

write_nostatus_assertion(yes,Type,Goal):-
	format("%% :- ~w ~q",[Type,Goal]).
write_nostatus_assertion(no,Type,Goal):-
	format(":- ~w ~q",[Type,Goal]).

write_status_assertion(yes,Status,Type,Goal):-
	format("%% :- ~w ~w ~q",[Status,Type,Goal]).
write_status_assertion(no,Status,Type,Goal):-
	format(":- ~w ~w ~q",[Status,Type,Goal]).

write_comment([],_AsComm):- !.
write_comment(Comm,AsComm):-
	check_comas_in_comment(Comm,CC ),
	write_comment_as_comment(AsComm,CC).

write_comment_as_comment(yes,Comm):-
	format('~n%% ~8|#  "~s"',[Comm]).
write_comment_as_comment(no,Comm):-
	format('~n~8|#  "~s"',[Comm]).

check_comas_in_comment( [] , [] ).

check_comas_in_comment( [A|Ar] , [A|Br] ) :-
	A \== 0'",
	check_comas_in_comment(Ar,Br).

check_comas_in_comment( [A|Ar] , [0'\\,A|Br] ) :-
	check_comas_in_comment(Ar,Br).



write_if_not_empty([],_Mod,_AsComm,_Always):- !.
write_if_not_empty([true],_Mod,_AsComm,conj):- !.
write_if_not_empty([[]],_Mod,_AsComm,disj):- !.
write_if_not_empty([[true]],_Mod,_AsComm,disj):- !.
write_if_not_empty(List,Mod,AsComm,Form):-
	write_as_comment(AsComm,Mod),
	(
	    List = [L_inside],
	    L_inside = (_;_)
	->
	    disj_to_list_of_list( L_inside , L1 ),
	    print_prop_list(disj, L1 )
	;
	    print_prop_list(Form,List)
	).


disj_to_list_of_list( (A;B) , [A|Bs] ) :-
	!,
	disj_to_list_of_list( B , Bs ).
disj_to_list_of_list( A , [A] ).


write_as_comment(yes,Mod):-
	format("~n%% ~8|~w ",[Mod]).
write_as_comment(no,Mod):-
	format("~n~8|~w ",[Mod]).

print_prop_list(conj,List):-
	print_conjunction(List).
print_prop_list(disj,List):-
	print_disjunction(List).

print_disjunction([]).
print_disjunction([Prop]):- !,
	print_conjunction(Prop).
print_disjunction([Prop|Props]):-
	format("( ",[]),
	print_conjunction(Prop),
	print_tail_disj(Props).

print_tail_disj([]):-
	format(" )",[]).
print_tail_disj([Prop|Props]):-
	format("; ",[]),
	print_conjunction(Prop),
	print_tail_disj(Props).

print_conjunction([]).
print_conjunction([Prop]):- !,
	might_be_qualified(Prop).
print_conjunction([Prop|Props]):-
	format("( ~q",[Prop]),
	print_tail_conj(Props).

print_tail_conj([]):-
	format(" )",[]).
print_tail_conj([Prop|Props]):-
	format(", ~q",[Prop]),
	print_tail_conj(Props).

might_be_qualified(M:Prop):- !,
	format("( ~q )",[M:Prop]).
might_be_qualified(Prop):- !,
	format("~q",[Prop]).

unify_vars([]).
unify_vars([N=V|Dict]):-
	V='$VAR'(N),
	unify_vars(Dict).

decide_on_call(Call,disj):-
	list(Call,list), !.
decide_on_call(_Call,conj).
