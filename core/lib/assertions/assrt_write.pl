:- module(assrt_write,
	[ write_assertion/6,
	  write_assertion/7,
	  write_assertion_as_comment/6,
	  write_assertion_as_comment/7,
	  write_assertion_as_double_comment/6,
	  write_assertion_as_double_comment/7
	],
	[ assertions, regtypes
	]).

:- doc(title,"Pretty-printing assertions").

:- doc(module,"This module defines some predicates which are
   useful for writing assertions in a readable form.").

:- doc(author,"Francisco Bueno").

% ISO-Prolog compatibility libraries
:- use_module(library(format)).  

% Other libraries
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(messages)).
:- use_module(library(assertions/assertions_props)).

:- use_module(library(vndict), [varnamesl2dict/2, rename/2, complete_dict_alpha/3]).


:- regtype status_flag(F) # "@var{F} is @tt{status} or @tt{nostatus}.".

status_flag(status).
status_flag(nostatus).

:- pred write_assertion(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output.".

write_assertion(Goal,Status,Type,Body,Dict,Flag):-
	current_output(CO),
	\+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,no,CO).

:- pred write_assertion(Stream,Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to stream @var{Stream}.".

write_assertion(S,Goal,Status,Type,Body,Dict,Flag):-
	\+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,no,S).

:- pred write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output as
           a Prolog comment.".

write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag):-
	current_output(CO),
	\+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,yes,CO).

:- pred write_assertion_as_comment(Stream,Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to stream @var{Stream} as
           a Prolog comment.".

write_assertion_as_comment(Stream,Goal,Status,Type,Body,Dict,Flag):-
	\+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,yes,Stream).


:- pred write_assertion_as_double_comment(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output as
           a Prolog double comment.".

write_assertion_as_double_comment(Goal,Status,Type,Body,Dict,Flag):-
	current_output(CO),
	\+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,double,CO).


:- pred write_assertion_as_double_comment(Stream,Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to stream @var{Stream} as
           a Prolog double comment.".

write_assertion_as_double_comment(Stream,Goal,Status,Type,Body,Dict,Flag):-
	\+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,double,Stream).

write_assertion_(Goal,Status,Type,Body,Dict,Flag,AsComm,Stream):-	
	% unify_vars(Dict),
 	varnamesl2dict( Dict, VnDict ),
	complete_dict_alpha( VnDict , Goal , D2 ),
	% DTM: Maybe someone instantiated our variables when doing a write!!!
	( rename( Goal , D2 ) -> true ; true ),
	( Flag=nostatus
	-> write_nostatus_assertion(AsComm,Type,Goal,Stream)
	 ; write_status_assertion(AsComm,Status,Type,Goal,Stream)
	),
	complete_dict_alpha( D2, Body, D3 ),
	( rename( Body, D3 ) -> true ; true),
	assertion_body(Goal,Compat,Call,Succ,Comp,Comm,Body),
	write_if_not_empty(Compat,'::',AsComm,conj,Stream),
	decide_on_call(Call,FormC),
	write_if_not_empty(Call,' :',AsComm,FormC,Stream),
	decide_on_call(Succ,FormS),
	write_if_not_empty(Succ,'=>',AsComm,FormS,Stream),
	decide_on_call(Comp,FormP),
	write_if_not_empty(Comp,' +',AsComm,FormP,Stream),
	write_comment(Comm,AsComm,Stream),
	format(Stream,".~n~n",[]),
	!.
write_assertion_(_Goal,Status,Type,Body,_Dict,_Flag,_AsComm,_Stream):-
	error_message("Error printing assertion:~n:- ~w ~w ~w~n",
               [Status,Type,Body]),
	fail.

write_nostatus_assertion(double,Type,Goal,S):-
	format(S,"%% %% :- ~w ~q",[Type,Goal]).
write_nostatus_assertion(yes,Type,Goal,S):-
	format(S,"%% :- ~w ~q",[Type,Goal]).
write_nostatus_assertion(no,Type,Goal,S):-
	format(S,":- ~w ~q",[Type,Goal]).

write_status_assertion(double,Status,Type,Goal,S):-
	format(S,"%% %% :- ~w ~w ~q",[Status,Type,Goal]).
write_status_assertion(yes,Status,Type,Goal,S):-
	format(S,"%% :- ~w ~w ~q",[Status,Type,Goal]).
write_status_assertion(no,Status,Type,Goal,S):-
	format(S,":- ~w ~w ~q",[Status,Type,Goal]).

write_comment([],_AsComm,_):- !.
write_comment(Comm,AsComm,S):-
	check_comas_in_comment(Comm,CC ),
	write_comment_as_comment(AsComm,CC,S).

write_comment_as_comment(double,Comm,S):-
	format(S,'~n%% %% ~8|#  "~s"',[Comm]).
write_comment_as_comment(yes,Comm,S):-
	format(S,'~n%% ~8|#  "~s"',[Comm]).
write_comment_as_comment(no,Comm,S):-
	format(S,'~n~8|#  "~s"',[Comm]).


check_comas_in_comment( [] , [] ).
check_comas_in_comment( [A|Ar] , [A|Br] ) :-
	A \== 0'",
	check_comas_in_comment(Ar,Br).
check_comas_in_comment( [A|Ar] , [0'\\,A|Br] ) :-
	check_comas_in_comment(Ar,Br).


write_if_not_empty([]      ,_Mod,_AsComm,_Always,_):- !.
write_if_not_empty([true]  ,_Mod,_AsComm,conj,_):- !.
write_if_not_empty([[]]    ,_Mod,_AsComm,disj,_):- !.
write_if_not_empty([[true]],_Mod,_AsComm,disj,_):- !.
write_if_not_empty(List    ,Mod,AsComm,Form,Stream):-
	write_as_comment(AsComm,Mod,Stream),
	(
	    List = [(C1;C2)]
	->
%	    conj_to_list_of_list( (C1;C2) , L1 , [] ),
	    conj_to_list_of_list( (C1;C2) , L1 ),
	    print_prop_list(Form, L1 ,Stream)
	;
	    print_prop_list(Form,List,Stream)
	).




% conj_to_list_of_list( (A,B) ,  Ac  , TAc ) :-
% 	!,
% 	conj_to_list_of_list( A , Ac , T   ),
% 	conj_to_list_of_list( B , T  , TAc ).

% conj_to_list_of_list( (A;B) , [ [ AC , BC ] | T ] , T ) :-
% 	!,
% 	conj_to_list( A , AC ),
% 	conj_to_list( B , BC ).

% conj_to_list_of_list( A , [ A | T ] , T ).




% conj_to_list( (A,B) , [A|Bs] ) :-
% 	!,
% 	conj_to_list( B , Bs ).
% conj_to_list( A , [A] ).



conj_to_list_of_list( (A;B) , [A|Bs] ) :-
	list( A ),
	!,
	conj_to_list_of_list( B , Bs ).
conj_to_list_of_list( (A;B) , [AL|Bs] ) :-
	!,
	conj_to_list( A , AL ),
	conj_to_list_of_list( B , Bs ).
conj_to_list_of_list( A , [A] ) :-
	list( A ),
	!.
conj_to_list_of_list( A , [AL] ) :-
	conj_to_list( A , AL ).


conj_to_list( (A,B) , [A|Bs] ) :-
	!,
	conj_to_list( B , Bs ).
conj_to_list( A , [A] ).


% disj_to_list( (A,B) , [A|Bs] ) :-
% 	!,
% 	disj_to_list( B , Bs ).
% disj_to_list( A , A ).





write_as_comment(double,Mod,S):-
	format(S,"~n%% %% ~8|~w ",[Mod]).
write_as_comment(yes,Mod,S):-
	format(S,"~n%% ~8|~w ",[Mod]).
write_as_comment(no,Mod,S):-
	format(S,"~n~8|~w ",[Mod]).

print_prop_list(conj,List,S):-
	print_conjunction(List,S).
print_prop_list(disj,List,S):-
	print_disjunction(List,S).

print_disjunction([],_).
print_disjunction([Prop],S):- !,
	print_conjunction_1(Prop,S).
print_disjunction([Prop|Props],S):-
	format(S,"( ",[]),
	print_conjunction_1(Prop,S),
	print_tail_disj(Props,S).

print_tail_disj([],S):-
	format(S," )",[]).
print_tail_disj([Prop|Props],S):-
	format(S,"; ",[]),
	print_conjunction_1(Prop,S),
	print_tail_disj(Props,S).

print_conjunction_1([],S) :-
	!,
	format(S,"true",[]).
print_conjunction_1(A,S) :-
	print_conjunction(A,S).


print_conjunction([],_S).
print_conjunction([Prop],S):- !,
	( Prop = _ : _ ->
	  format(S,"(~q)",[Prop])
	; format(S, "~q" ,[Prop])).
print_conjunction([Prop|Props],S):-
	format(S,"( ~q",[Prop]),
	print_tail_conj(Props,S).

print_tail_conj([],S):-
	format(S," )",[]).
print_tail_conj([Prop|Props],S):-
	format(S,", ~q",[Prop]),
	print_tail_conj(Props,S).

/*
unify_vars([]).
unify_vars([N=V|Dict]):-
	V='$VAR'(N),
	unify_vars(Dict).
*/

% DTM: this case appears in :- calls p(X): (ground(X);var(X)).
decide_on_call([(_;_)],disj):-
	!.
decide_on_call(Call,disj):-
	list(Call,list), !.
decide_on_call(_Call,conj).
