:- module(formulae,
	[ list_to_conj/3,
	  list_to_conj/2,
	  conj_to_list/2,
	  list_to_disj/2,
	  disj_to_list/2,
	  conj_to_llist/2,
	  llist_to_conj/2,
	  disj_to_llist/2,
	  llist_to_disj/2,
	
	  body2list/2,
	  asbody_to_conj/2,

          % types
	  assert_body_type/1,
	  conj_disj_type/1,
	  t_conj/1,
	  t_disj/1,

	  list_to_disj2/2

% for debugging  
%	  list_of_list_to_conj/2,
%	  conj_to_list_of_list/3
	],
	[ assertions, regtypes
	] ).

:- use_module(library(messages)).

:- doc(title,"Lists and conjunctions and disjunctions").

:- doc(list_to_conj(List,Conj,End),"
	@var{Conj} is the conjunction made up of the elements of @var{List}
        plus a final element @var{End}.").

list_to_conj([X|More],(X,Next),End):-
        list_to_conj(More,Next,End).
list_to_conj([],End,End).

:- push_prolog_flag(multi_arity_warnings,off).

:- pred list_to_conj( A , B )
	: ( list( A ) , var( B ) )
        => t_conj( B )

# "@var{Conj} is the conjunction made up of the elements of @var{List}.
  (@tt{[]} is @tt{true}). It runs in both ways. 

@begin{verbatim}
?- list_to_conj( A , a ).

A = [a] ? ;

no
?- list_to_conj( A , (a,V) ).

A = [a,V] ? ;

no
?- list_to_conj( A , (a,V,b) ).

A = [a,V,b] ? ;

no
?- list_to_conj( [A] , B ).

B = A ? ;

no
?- list_to_conj( [a,A] , B ).

B = (a,A) ? ;

no
?- list_to_conj( [a,A,b] , B ).

B = (a,A,b) ? ;

no
?- list_to_conj( [] , B ).

B = true ? ;

no
@end{verbatim}
".

:- pred list_to_conj( A , B )
	: ( var( A ) , t_conj( B ) )
         => list( A ).

list_to_conj( []    , true  ) :-
	!.
list_to_conj( [A|B] , (A,Br) ) :-
	B \== [],
	!,
	list_to_conj_( B , Br ).
list_to_conj( [A]   , A      ) :-
	!.
list_to_conj( A    , _  ) :-
	error_message(
		"Internal Error: Bad Arguments ~w when calling list_to_conj/2",
	        [ A ] ),
	fail.


list_to_conj_( B , Br ) :-
	var( B ),
	var( Br ),
	!,
	[Br] = B.
list_to_conj_( B , Br ) :-
	list_to_conj( B , Br ).


%% was:
%% :- doc(list_to_conj(List,Conj),"
%% 	@var{Conj} is the conjunction made up of the elements of
%% 	@var{List} (@tt{[]} is @tt{true}).").
%%
%% list_to_conj([X],X):- !.
%% list_to_conj([X|More],(X,Next)):-
%% 	list_to_conj(More,Next).
%% list_to_conj([],true).

:- pop_prolog_flag(multi_arity_warnings).

:- doc(conj_to_list(Conj,List),"
	@var{List} is the list made up of the elements of conjunction 
	@var{Conj} (@tt{true} is @tt{[]}).").

conj_to_list( A , B ) :- list_to_conj( B , A ).

%% was:
%% conj_to_list((A,B),[A|List]):- !,
%%  	conj_to_list(B,List).
%% conj_to_list(true,[]):- !.
%% conj_to_list(A,[A]).

%%
%% list_to_disj
%%

:- pred list_to_disj( A , B )
	: ( list( A ) , var( B ) )
        => t_disj( B )

# "@var{Disj} is the disjunction made up of the elements of @var{List}.
  (@tt{[]} is @tt{true}). It runs in both ways. Examples:

@begin{verbatim}
?- list_to_disj( [a] , A ).

A = a ? ;

no
?- list_to_disj( [a,b] , A ).

A = (a;b) ? ;

no
?- list_to_disj( [a,B,b] , A ).

A = (a;B;b) ? ;

no
?- list_to_disj( [a,b,B] , A ).

A = (a;b;B) ? ;

no
?- list_to_disj( A , (a) ).

A = [a] ? ;

no
?- list_to_disj( A , (a;b) ).

A = [a,b] ? ;

no
?- list_to_disj( A , (a;B;b) ).

A = [a,B,b] ? ;

no
?- 
@end{verbatim}".

:- pred list_to_disj( A , B )
	: ( var( A ) , t_disj( B ) )
         => list( A ).

list_to_disj( []    , false  ) :-
	!.
list_to_disj( [A|B] , (A;Br) ) :-
	B \== [],
	!,
	list_to_disj_( B , Br ).
list_to_disj( [A]   , A      ) :-
	!.
list_to_disj( A    , _  ) :-
	error_message(
		"Internal Error: Bad Arguments ~w when calling list_to_disj/2",
	        [ A ] ),
	fail.


list_to_disj_( B , Br ) :-
	var( B ),
	var( Br ),
	!,
	[Br] = B.
list_to_disj_( B , Br ) :-
	list_to_disj( B , Br ).

%% :- doc(list_to_disj(List,Disj),"
%% 	@var{Disj} is the disjunction made up of the elements of
%% 	@var{List} (@tt{[]} is @tt{true}).").
%%
 list_to_disj2([],false):- !.
 list_to_disj2([X],X):- !.
 list_to_disj2([X|Xs],(X;Ys)):-
 	list_to_disj2(Xs,Ys).

:- doc(disj_to_list(Disj,List),"
	@var{List} is the list made up of the elements of disjunction
	@var{Disj} (@tt{true} is @tt{[]}).").


disj_to_list( D , L ) :- 
	list_to_disj( L , D ).

% was:
% % disj_to_list( (A;Br) , [A|B] ) :-
% % 	!,
% % 	disj_to_list( Br , B ).
% % disj_to_list( A   , [A]      ) :- 
% % 	!.
%
% disj_to_list( (A;B) , [A|List] ) :- !,
%  	disj_to_list( B , List ).
% disj_to_list( true , [ ]   ) :- !.
% disj_to_list( A    , [ A ] ).

:- doc(conj_to_llist/2,"Turns a conjunctive (normal form) formula
	into a list (of lists of ...). As a side-effect, inner 
	conjunctions get flattened. No special care for @tt{true}.").

conj_to_llist(D,L):-
	conj_to_llist_diff(D,L,[]).

conj_to_llist_diff((A,B),LL,LT):- !,
	conj_to_llist_diff(A,LL,LA),
	conj_to_llist_diff(B,LA,LT).
conj_to_llist_diff((A;B),[LL|LT],LT):- !,
	disj_to_llist_diff(A,LL,LA),
	disj_to_llist_diff(B,LA,[]).
conj_to_llist_diff(A,[A|LT],LT).

:- doc(llist_to_conj/2,"Inverse of @tt{conj_to_llist/2}. No provisions
	for anything else than a non-empty list on input (i.e., they will
	go `as are' in the output.").

llist_to_conj([LL],C):- !,
	llist_to_disj(LL,C).
llist_to_conj([LL|LLs],(C,Cs)):- !,
	llist_to_disj(LL,C),
	llist_to_conj(LLs,Cs).
llist_to_conj(C,C).

:- doc(disj_to_llist/2,"Turns a disjunctive (normal form) formula 
	into a list (of lists of ...). As a side-effect, inner 
	disjunctions get flattened. No special care for @tt{true}.").

disj_to_llist(D,L):-
	disj_to_llist_diff(D,L,[]).

disj_to_llist_diff((A;B),LL,LT):- !,
	disj_to_llist_diff(A,LL,LA),
	disj_to_llist_diff(B,LA,LT).
disj_to_llist_diff((A,B),[LL|LT],LT):- !,
	conj_to_llist_diff(A,LL,LA),
	conj_to_llist_diff(B,LA,[]).
disj_to_llist_diff(A,[A|LT],LT).

:- doc(llist_to_disj/2,"Inverse of @tt{disj_to_llist/2}. No provisions
	for anything else than a non-empty list on input (i.e., they will
	go `as are' in the output.").

llist_to_disj([LL],D):- !,
	llist_to_conj(LL,D).
llist_to_disj([LL|LLs],(D;Ds)):- !,
	llist_to_conj(LL,D),
	llist_to_disj(LLs,Ds).
llist_to_disj(D,D).

%% :- pred appendconj( A , B , C )
%% # "Appends conjuntions."
/*
appendconj( (A,B) , R , (A,Bs) ) :-
	!,
	appendconj( B , R , Bs ).
	
appendconj( B  , R , (B,R) ).
*/

:- pred asbody_to_conj( A , B )
	: ( assert_body_type( A ) , var( B ) )
        => conj_disj_type( B )

# "Transforms assertion body @var{A} into a conjuntion (@var{B}). It
  runs in both ways".

:- pred asbody_to_conj( A , B ) 
	:  ( var( A ) , conj_disj_type( B ) )
        => assert_body_type( A ).


asbody_to_conj( A , B ) :-
	var( A ),
	!,
	( 
	    var( B ) 
	->
	    A = B
	;
	    conj_to_list_of_list( B ,  A  , [] )
	).
asbody_to_conj( A , B ) :-
	list_of_list_to_conj( A ,  B ).

list_of_list_to_conj( [ ( A ; B ) | C ] , Out ) :-
	!,
 	list_to_conj( A , AC ),
 	list_of_list_to_conj( [B] , BC ),
 	list_of_list_to_conj( C , CC ),
	( 
	    CC == true
	->
	    Out = (AC;BC)
	;
	    Out = ( (AC;BC) , CC )
	).
list_of_list_to_conj( [ A | B ] , (AC , BC ) ) :-
	B \== [],
	!,
	list_of_list_to_conj( A , AC ),
	list_of_list_to_conj( B , BC ).
list_of_list_to_conj( [AL] , A ) :-
	list_of_list_to_conj( AL , A ),
	!.
list_of_list_to_conj( AL , A ) :-	
	list( AL ),
	!,
	list_to_conj( AL , A ).
list_of_list_to_conj( A , A ).



conj_to_list_of_list( (A,B) ,  Ac  , TAc ) :-
	!,
	conj_to_list_of_list( A , Ac , T   ),
	conj_to_list_of_list( B , T  , TAc ).
%conj_to_list_of_list( (A;B) , [ [ AC , BC ] | T ] , T ) :-
conj_to_list_of_list( (A;B) , [ ( AC ; BC ) | T ] , T ) :-
	!,
	conj_to_list__( A , AC ),
	conj_to_list__( B , BC ).
conj_to_list_of_list( true , T , T ) :-
	!.
conj_to_list_of_list( A , [ A | T ] , T ).

conj_to_list__( (A,B) , [A|Bs] ) :-
	!,
	conj_to_list( B , Bs ).
conj_to_list__( (A;B) , (As;Bs) ) :-
	!,
	conj_to_list( A , As ),
	conj_to_list__( B , Bs ).
conj_to_list__( A , [A] ).

%-------------------------------------------------------------------%
% body2list(+,-)                                                    %
% body2list(Body,List)                                              %
%  Transform the body of a clause into a list of goals              %
%-------------------------------------------------------------------% 
body2list( (First,Rest) , [ NewFirst | More ] ) :-
        !,
	p_exp2list( First , NewFirst ),
        body2list( Rest , More ).
body2list( Last , [ NewLast ] ) :-
	p_exp2list( Last , NewLast ).

%-------------------------------------------------------------------%
% p_exp2list(+,-)                                                   %
% p_exp2list(Parall_expression,List)                                %
%  Transform a set of parallel goals to a list of goals             %
%-------------------------------------------------------------------%
p_exp2list( '&'(G,Goals) , [ G | More ] ) :-
	!,
	p_exp2list__( Goals , More ).
p_exp2list( Goal , Goal ).

p_exp2list__( '&'(G,Goals) , [ G | More ] ) :-
	p_exp2list__( Goals , More ).
p_exp2list__( Goal , [ Goal ] ).


                     %%%%%%%%%%%
                     %% TYPES %%
                     %%%%%%%%%%%


:- prop assert_body_type/1.

assert_body_type(  X  ) :- list( X , assert_body_type__ ).


assert_body_type__( A ) :-
	A = ( _ ; _ ),
	!,
	abt_only_disj( A ).
assert_body_type__( _ ).


abt_only_disj( (A;B) ) :-
	!,
	list(A),
	abt_only_disj__2( B ).


abt_only_disj__2( (A;B) ) :-
	!,
	list(A),
	abt_only_disj__2( B ).
abt_only_disj__2( A ) :- list( A ).



:- regtype conj_disj_type/1

# "The usual prolog way of writing conjuntions and disjuntions in a
  body using ',' and ';'".

% conjuntion
conj_disj_type( ( _ , B) ) :-
	conj_disj_type( B ).
% disjuntion
conj_disj_type( ( _ ; B) ) :-
	conj_disj_type( B ).
% a goal
conj_disj_type( _ ).




:- regtype t_conj/1

# "Conjuntions.".

% conjuntion
t_conj( ( _ , B) ) :-
	t_conj( B ).
% a goal
t_conj( _ ).




:- regtype t_disj/1

# "Disjunctions.".

% disjunction
t_disj( ( _ ; B) ) :-
	t_disj( B ).
% a goal
t_disj( _ ).
