:- module(hprolog,
	  [ append/2,		        % +ListOfLists, -List
%% Ciao begin
%	    nth/3,			% ?Index, ?List, ?Element
            memberchk/2,
%% Ciao end
	    substitute/4,		% +OldVal, +OldList, +NewVal, -NewList
	    memberchk_eq/2,		% +Val, +List
	    intersect_eq/3,		% +List1, +List2, -Intersection
	    list_difference_eq/3,	% +List, -Subtract, -Rest
	    take/3,			% +N, +List, -FirstElements
	    drop/3,			% +N, +List, -LastElements
	    max_go_list/2,		% +List, -Max
	    or_list/2,			% +ListOfInts, -BitwiseOr
	    sublist/2,			% ?Sublist, +List
	    bounded_sublist/3,		% ?Sublist, +List, +Bound
	    min_list/2,
	    chr_delete/3,
	    init_store/2,
	    get_store/2,
	    update_store/2,
	    make_get_store_goal/3,
	    make_update_store_goal/3,
	    make_init_store_goal/3,

	    empty_ds/1,
	    ds_to_list/2,
	    get_ds/3,
	    put_ds/4

%% Ciao begin
	  , term_variables/2
	  , term_variables/3
          , permutation/2
          , nb_getval/2
          , nb_setval/2
          , b_getval/2
          , b_setval/2
          , is_list/1
          , writeln/1
          , ord_empty/1
          , list_to_ord_set/2
          , ord_memberchk/2
	  , ord_add_element/3
	  , ord_del_element/3
          , flatten/2
	  , maplist/3
	  , maplist/2
          , predsort/3
          ],
	  [ hiord
%% Ciao end
    
	  ]).
:- use_module(library(lists)).
:- use_module(library(chr/local_assoc)).

empty_ds(DS) :- empty_assoc(DS).
ds_to_list(DS,LIST) :- assoc_to_list(DS,LIST).
get_ds(A,B,C) :- get_assoc(A,B,C).
put_ds(A,B,C,D) :- put_assoc(A,B,C,D).


init_store(Name,Value) :- nb_setval(Name,Value).

get_store(Name,Value) :- nb_getval(Name,Value).

update_store(Name,Value) :- b_setval(Name,Value).

make_init_store_goal(Name,Value,Goal) :- Goal = nb_setval(Name,Value).

make_get_store_goal(Name,Value,Goal) :- Goal = nb_getval(Name,Value).

make_update_store_goal(Name,Value,Goal) :- Goal = b_setval(Name,Value).


		 /*******************************
		 *      MORE LIST OPERATIONS	*
		 *******************************/

%	append(+ListOfLists, -List)
%	
%	Convert a one-level nested list into a flat one.  E.g.
%	append([[a,b], [c]], X) --> X = [a,b,c].  See also
%	flatten/3.

append([],[]).
append([X|Xs],L) :-
	append(X,T,L),
	append(Xs,T).

%% Ciao begin
%	nth(?Index, ?List, ?Element)
%	
%	Same as nth1/3

% nth(Index, List, Element) :-
% 	nth1(Index, List, Element).

:- reexport(library(lists), [nth/3]).
%% Ciao end


%	substitute(+OldVal, +OldList, +NewVal, -NewList)
%	
%	Substitute OldVal by NewVal in OldList and unify the result
%	with NewList.  JW: Shouldn't this be called substitute_eq/4?

substitute(_, [], _, []) :- ! .
substitute(X, [U|Us], Y, [V|Vs]) :-
        (   X == U
	->  V = Y,
            substitute(X, Us, Y, Vs)
        ;   V = U,
            substitute(X, Us, Y, Vs)
        ).

%	memberchk_eq(+Val, +List)
%	
%	Deterministic check of membership using == rather than
%	unification.

memberchk_eq(X, [Y|Ys]) :-
	(   X == Y
	->  true
	;   memberchk_eq(X, Ys)
	).

%	list_difference_eq(+List, -Subtract, -Rest)
%	
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using ==/2.

list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
	(   memberchk_eq(X,Ys)
	->  list_difference_eq(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_eq(Xs,Ys,T)
	).

%	intersect_eq(+List1, +List2, -Intersection)
%	
%	Determine the intersection of two lists without unifying values.

intersect_eq([], _, []).
intersect_eq([X|Xs], Ys, L) :-
	(   memberchk_eq(X, Ys)
	->  L = [X|T],
	    intersect_eq(Xs, Ys, T)
	;   intersect_eq(Xs, Ys, L)
	).


%	take(+N, +List, -FirstElements)
%	
%	Take the first  N  elements  from   List  and  unify  this  with
%	FirstElements. The definition is based   on the GNU-Prolog lists
%	library. Implementation by Jan Wielemaker.

take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).

%	Drop the first  N  elements  from   List  and  unify  the remainder  with
%	LastElements.

drop(0,LastElements,LastElements) :- !.
drop(N,[_|Tail],LastElements) :-
	N > 0,
	N1 is N  - 1,
	drop(N1,Tail,LastElements).


%% Ciao begin
:- push_prolog_flag( multi_arity_warnings , off ).
%% Ciao end

%	max_go_list(+List, -Max)
%	
%	Return the maximum of List in the standard order of terms.

max_go_list([H|T], Max) :-
	max_go_list(T, H, Max).

max_go_list([], Max, Max).
max_go_list([H|T], X, Max) :-
        (   H @=< X
	->  max_go_list(T, X, Max)
        ;   max_go_list(T, H, Max)
        ).

%	or_list(+ListOfInts, -BitwiseOr)
%	
%	Do a bitwise disjuction over all integer members of ListOfInts.

or_list(L, Or) :-
	or_list(L, 0, Or).

or_list([], Or, Or).
or_list([H|T], Or0, Or) :-
	Or1 is H \/ Or0,
	or_list(T, Or1, Or).

%% Ciao begin
:- pop_prolog_flag( multi_arity_warnings ).
%% Ciao end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sublist(L, L).
sublist(Sub, [H|T]) :-
	'$sublist1'(T, H, Sub).

'$sublist1'(Sub, _, Sub).
'$sublist1'([H|T], _, Sub) :-
	'$sublist1'(T, H, Sub).
'$sublist1'([H|T], X, [X|Sub]) :-
	'$sublist1'(T, H, Sub).

bounded_sublist(Sublist,_,_) :-
	Sublist = [].
bounded_sublist(Sublist,[H|List],Bound) :-
	Bound > 0,
	(
		Sublist = [H|Rest],
		NBound is Bound - 1,
		bounded_sublist(Rest,List,NBound)
	;
		bounded_sublist(Sublist,List,Bound)
	).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_list([H|T], Min) :-
	'$min_list1'(T, H, Min).

'$min_list1'([], Min, Min).
'$min_list1'([H|T], X, Min) :-
        (   H>=X ->
            '$min_list1'(T, X, Min)
        ;   '$min_list1'(T, H, Min)
        ).

chr_delete([], _, []).
chr_delete([H|T], X, L) :-
        (   H==X ->
            chr_delete(T, X, L)
        ;   L=[H|RT],
            chr_delete(T, X, RT)
        ).
    
%% CiaoStart

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GENERAL PORTABILITY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_list( [_|_] ).


:- use_module(library(terms_vars)).

term_variables( Term, Vs) :-
	varset( Term, Vs).

term_variables( Term, Vs, Tail) :-
	varset( Term, Vs0),
	append( Vs0 , Tail , Vs ).


raise_exception( A ) :- throw( A ).

:- meta_predicate on_exception( ? , goal , goal ).

on_exception( A , B , C ) :- catch( B , A , C ).

:- use_module(library(terms_check), [ask/2]).

subsumes( General , Specific ) :-
	ask( Specific , General ),
	Specific = General.

subsumes_chk( General , Specific ) :- 
	ask( Specific , General ),
	\+ \+ Specific = General.



print_attribute_goals( [] ).
print_attribute_goals( [A|As] ) :-
	!,
	print( A ), display( ',\n' ),
	print_attribute_goals( As ).
print_attribute_goals( (A,As) ) :-
	!,
	print( A ), display( ',\n' ),
	print_attribute_goals( As ).
print_attribute_goals( A ) :-
	print( A ).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :- memberchk(X, L).


:- use_module(engine(internals), ['$global_vars_get'/2]).
:- use_module(library(mutables)).
:- multifile(initial_gv_value/2).

b_setval(Name,Value) :-
	'$global_vars_get'(10, GV),
	(
	    var(GV) 
	->
            create_mutable(Value, Mutable), 
	    empty_assoc(Assoc),
	    put_assoc(Name, Assoc, Mutable, Assoc1), 
            create_mutable(Assoc1, GV)
	;
            get_mutable(Assoc, GV),
            (
                get_assoc(Name, Assoc, Mutable) ->
                update_mutable(Value, Mutable)
            ;
                create_mutable(Value, Mutable),
 	        put_assoc(Name, Assoc, Mutable, Assoc1),
                update_mutable(Assoc1, GV)
            )
	).

b_getval(Name,Value) :-
	'$global_vars_get'(10, GV),
	(
	    var(GV) 
	->
	    initial_gv_value(Name, Value),
            create_mutable(Value, Mutable), 
	    empty_assoc(Assoc),
	    put_assoc(Name, Assoc, Mutable, Assoc1), 
            create_mutable(Assoc1, GV)
	;
            get_mutable(Assoc, GV), 
	    (
                get_assoc(Name, Assoc, Mutable) 
	    ->
                get_mutable(Value, Mutable)
	    ; 
	        initial_gv_value(Name, Value),
                create_mutable(Value, Mutable),
                put_assoc(Name, Assoc, Mutable, Assoc1),
                update_mutable(Assoc1, GV)
	    )
	).


%% yahoo!
nb_setval(Name,Value) :-
%	message( note , [ 'Initial GV value: ' , Name , '  ', Value ] ),
	(
	    b_setval(Name,Value) -> true
	;
	    message(error, 
	            ['This should not happen man... Murphy reloaded :(' ,
	             Name, ' ', Value]),
	    '$global_vars_get'(10, GV),
	    message(error, 
	            ['The variable is ', GV])
	).

nb_getval(Name,Value) :-
	(
	    b_getval(Name,Value) -> true
	;
	    message(error , 
	            ['This should not happen man... Murphy resurrection :(',
                     Name, ' ', Value]),
	    '$global_vars_get'(10, GV),
	    message(error , 
	            ['The variable is ', GV])
	).


:- use_module(library(write)).
:- use_module(library(sort)).

writeln( X ) :-
	write( X ), nl.

:- push_prolog_flag( multi_arity_warnings , off ).

permutation(Xs, Ys) :-
	permutation(Xs, Ys, Ys).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Bound]) :-
	permutation(Xs, Ys, Bound),
	select(X, Ys1, Ys).

list_to_ord_set( X , XS ) :-
	sort( X , XS ).

%	flatten(+List1, ?List2)
%
%	Is true when Lis2 is a non nested version of List1.

flatten(List, FlatList) :-
	flatten(List, [], FlatList0), !,
	FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
	var(Var), !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
	flatten(Hd, FlatHeadTail, List), 
	flatten(Tl, Tail, FlatHeadTail).
flatten(Atom, Tl, [Atom|Tl]).


:- push_prolog_flag( multi_arity_warnings , on ).

:- use_module(library(sets)).

ord_empty( [] ).

ord_add_element(A,B,C) :-
	insert(A,B,C).

ord_del_element(A,B,C) :-
	ord_delete(A,B,C).

ord_memberchk( X , A ) :-
	ord_member( X , A ).



:- use_module(library(hiordlib), [map/3, foldl/4]).

:- meta_predicate maplist( pred(2) , ? , ? ).

maplist( P , L , NL ) :-
	map( L , P , NL ).

:- meta_predicate maplist( pred(1) , ? ).

maplist( P , L ) :-
	map_list1( L , P ).
	

% :- meta_predicate map_list( ? , pred(1) ).

map_list1( [] , _ ).
map_list1( [A|As] , P ) :-
	P(A),
	map_list1( As , P ).

:- meta_predicate predsort( pred(3) , ? , ? ).

predsort( P, L, NL ) :-
	predsort1( L, P, [] , NL ).
	

predsort1( [] , _P, L , L ).
predsort1( [A|As] , P, B , L ) :-
	my_insert( P , A , B , NB ),
	predsort1( As , P, NB , L ).
 


%	foldl( L , [] , my_insert(P) , NL ).

my_insert( _ , A , [] , [A] ) :-
	!.
my_insert( P , A , [B|Bs] , C ) :-
	P(R,A,B),
	!,
	( R = < -> C = [A,B|Bs] ; C = [B|C1], my_insert( P , A , Bs , C1 ) ).
	
	
%% CiaoEnd
