:- module(fd_rt, 
	[
	    '$in'/2, 
	    '$in_dom'/2, 
	    retrieve_list_of_values/2,
	    retrieve_range/2,
	    glb/2,
	    lub/2,
	    bounds/3,
	    retrieve_store/2,
	    labeling/1,
	    choose_var/3,
	    choose_free_var/2,
	    choose_var_nd/2,
	    choose_value/2,
	    pitm/2,
	    differ_on_instantiation/2,
	    equals_on_instantiation/2
	],
	[]).

:- use_module(library(fd/fd_bounds)).
:- use_module(library(lists), [append/3, nocontainsx/2]).
:- use_module(engine(attributes)).


:- multifile verify_attribute/2, combine_attributes/2.

arith_exp(0.6).

verify_attribute(Attr, Value) :-
	integer(Value),
	Attr = '$fd'(Var, Bounds, RestV),
	belongsto(Bounds, Value), 
	detach_attribute(Var),
	Var = Value,
	fd_tell(RestV, Var, [[Var|Var]]).

combine_attributes(Attr1, Attr2) :-
	Attr1 = '$fd'(V1, Bounds1, RestV1),
	Attr2 = '$fd'(V2, Bounds2, RestV2),
	get_whole_intersection(Bounds1, Bounds2, NewBounds), !,
	detach_attribute(V1),
	detach_attribute(V2),
	update_constraints(RestV2, RestV1, NewConstraints),
	V1 = V2,
	attach_attribute(V1 ,'$fd'(V1, NewBounds, NewConstraints)),
	fd_tell(NewConstraints, V1, NewBounds).


'$in'(V, Bounds) :-
	all_disjunct(Bounds),
	in_aux(Bounds, V, NormBounds),
       (var(V) ->
	 test_if_free(V, NormBounds),
	 get_attribute(V, '$fd'(V, VBounds, _)),
	 check_if_solution(VBounds, V, _)
       ;
        integer(V), %% Really check if number is in domain Term
	 belongsto(NormBounds, V)).

check_if_solution([[Min|Min]], V, true) :- !,
	V is integer(Min).
check_if_solution(_,_,false).
	
all_disjunct([_]):- !.
all_disjunct([Interval|L]) :-
	L = [_|_],
	disjunct(Interval, L),
	all_disjunct(L).

disjunct(_, []).
disjunct([Min|Max], [[Min1|Max1]|_R]) :-
	number(Min), 
	number(Max),
	number(Min1), 
	number(Max1), !,
	Min < Min1, 
	Max < Min1.
disjunct(_,_).

belongsto([Interval|_], V) :-
	belongs(V, Interval), !.
belongsto([_|R], V) :-
	belongsto(R, V).

belongs(V, [Min|Max]) :-
	number(Min), 
	number(Max), !,
	V >= Min,
	V =< Max.
belongs(_,_).

in_aux([],_,[]).
in_aux([SubDomain|Ds], V, AllBounds):-
	get_min_max(V, SubDomain, Range), !,
	in_aux(Ds, V, Bounds),
	(number_list(Range) ->
	 append(Range, Bounds, AllBounds)
	;
	 AllBounds=Bounds
	).

number_list([]).
number_list([[A|B]|Rs]) :-
	number(A),
	number(B),
	A =< B,
	number_list(Rs) .


'$in_dom'(X, Y) :-
	attach_to_dom_of(X, Y),
	(var(X) ->	 
	 get_attribute(X, '$fd'(X, XBounds, _)), !,
	 check_if_solution(XBounds, X, _)
	;
	 true).

attach_to_dom_of(X, Y) :-
	number(Y), !,
	X = Y.
attach_to_dom_of(X, Y) :-
	var(X),
	get_attribute(Y, AttrY),
	AttrY='$fd'(Y, YBounds, RestY), 
	(get_attribute(X, AttrX) ->
	 AttrX='$fd'(X, XBounds, RestX),
	 get_whole_intersection(XBounds, YBounds, NewXBounds), !,
	 detach_attribute(X)
	;    
	 NewXBounds=YBounds, 
	 RestX=[]
	),
	attach_attribute(X, '$fd'(X, NewXBounds, RestX)),
	detach_attribute(Y),
	attach_attribute(Y, '$fd'(Y, YBounds, [min(X), max(X)|RestY])).
attach_to_dom_of(X, Y) :-
	integer(X), 
	get_attribute(Y, AttrY),
	AttrY='$fd'(Y, YBounds, _),
	belongsto(YBounds, X).

equals_on_instantiation(X, Y) :-
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_attribute(Y, '$fd'(Y, YBounds, RestY)),
	update_constraints([equals_i(Y)], RestX, NRestX),
	update_constraints([equals_i(X)], RestY, NRestY),
	grow_restrictions_chain(X, _, '$fd'(_, XBounds, NRestX)),
	grow_restrictions_chain(Y, _, '$fd'(_, YBounds, NRestY)).	

differ_on_instantiation(X, Y) :-
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_attribute(Y, '$fd'(Y, YBounds, RestY)),
	update_constraints([differ_i(Y)], RestX, NRestX),
	update_constraints([differ_i(X)], RestY, NRestY),
	grow_restrictions_chain(X, _, '$fd'(_, XBounds, NRestX)),
	grow_restrictions_chain(Y, _, '$fd'(_, YBounds, NRestY)).

get_min_max(_, [X|Y], [[X|Y]]) :-
	integer(X),
	integer(Y), !.
get_min_max(V, [X|Y], [[X|Max]]) :-
	number(X),
	(Y=min(_); Y=max(_); Y=max_sub_c(_,_)), !,
	get_min_max1(V, Y, Max).
get_min_max(V, [X|Y], [[Inf|Max]]) :-
	(Y=min(_); Y=max(_)),
 	var(X),!, 
	get_min_max1(V, Y, Max),
	get_attribute(X, '$fd'(X, XBounds, _)), 
	get_infimus(XBounds, Inf).
get_min_max(V, [X|Y], [[Min|Y]]) :-
	(X=min(_); X=max(_); 
	 X=min_plus_c(_,_)),
 	number(Y),!,
	get_min_max1(V, X, Min).
get_min_max(V, [X|Y], [[Min|Sup]]) :-
	(X=min(_); X=max(_)),
 	var(Y),!, 
	get_min_max1(V, X, Min),
	get_attribute(Y, '$fd'(Y, YBounds, _)), 
	get_supreme(YBounds, Sup).
get_min_max(V, [X|Y], [[Min|Max]]) :-
	(X=min(_); X=max(_)),
	(Y=min(_); Y=max(_)), !,
	get_min_max1(V, X, Min),
	(X==Y ->
	 Max=Min
	;
	get_min_max1(V, Y, Max)).
get_min_max(V, [min_plus_c(X, C)|max_plus_c(X, C)], BoundsPlusC) :- !,
	(integer(X) ->
	 boundsPlusC(C, [[X|X]], BoundsPlusC)
	;
	 get_attribute(X, AttrX),	
	 AttrX = '$fd'(X, Bounds, RestX),
	 (integer(V) -> true
	 ;	  
	  update_constraints([min_plus_c(V,C),max_plus_c(V,C)],RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX))),
	  boundsPlusC(C, Bounds, BoundsPlusC)).
get_min_max(V, [min_sub_c(X, C)|max_sub_c(X, C)], BoundsSubC) :- !,
	(integer(X) ->
	 boundsSubC(C, [[X|X]], BoundsSubC)
	;
	 get_attribute(X, AttrX),
	 AttrX = '$fd'(X, Bounds, RestX),
	 (integer(V) -> true
	 ;
	  update_constraints([min_sub_c(V,C),max_sub_c(V,C)],RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX))),
	 boundsSubC(C, Bounds, BoundsSubC)).
get_min_max(V, [min_sub_max(X, Y)|max_sub_min(X, Y)], BoundsSub) :- !,
	(integer(X), integer(Y) ->
	 boundsSubC(Y, [[X|X]], BoundsSub)
	;
	 get_attribute(X, '$fd'(X, XBounds, RestX)),
	 get_attribute(Y, '$fd'(Y, YBounds, RestY)),
	 (integer(V) -> true
	 ;
	  NewRest=[min_sub_max(V,X,Y),max_sub_min(V,X,Y)],
	  update_constraints_xy(NewRest, RestX, RestY, NRestX, NRestY),
	  grow_restrictions_chain_xy(X,Y,V, XBounds, YBounds, NRestX, NRestY)
	 ),
	 get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	 (number_list([[MinX|MaxX],[MinY|MaxY]]) ->
	  Min is MinX - MaxY,
	  Max is MaxX - MinY,
	  BoundsSub = [[Min|Max]]
	 ;
	  BoundsSub = empty)).
get_min_max(V, [min_plus_min(X, Y)|max_plus_max(X, Y)], BoundsPlus) :- !,
	(integer(X), integer(Y) ->
	 boundsPlusC(Y, [[X|X]], BoundsPlus)
	;
	 get_attribute(X, '$fd'(X, XBounds, RestX)),
	 get_attribute(Y, '$fd'(Y, YBounds, RestY)),
	 (integer(V) -> true
	 ;
	  NewRest=[min_plus_min(V,X,Y),max_plus_max(V,X,Y)],
	  update_constraints_xy(NewRest, RestX, RestY, NRestX, NRestY),
	  grow_restrictions_chain_xy(X,Y,V, XBounds, YBounds, NRestX, NRestY)
	 ),
	 get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	 (number_list([[MinX|MaxX],[MinY|MaxY]]) ->
	  Min is MinX + MinY,
	  Max is MaxX + MaxY,
	  BoundsPlus = [[Min|Max]]
	 ;
	  BoundsPlus = empty)).

get_min_max(V, [min_div_max(X, Y)|max_div_min(X, Y)], BoundsDiv) :- !,
	(number(X), number(Y) ->
	 C is 1/Y,
	 boundsMultC(C, [[X|X]], BoundsDiv)
	;
	 get_attribute(X, '$fd'(X, XBounds, RestX)),
	 get_attribute(Y, '$fd'(Y, YBounds, RestY)),
	 (number(V) -> true
	 ;
	  NewRest=[min_div_max(V,X,Y),max_div_min(V,X,Y)],
	  update_constraints_xy(NewRest, RestX, RestY, NRestX, NRestY),
	  grow_restrictions_chain_xy(X,Y,V, XBounds, YBounds, NRestX, NRestY)
	 ),
	 get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	 (number_list([[MinX|MaxX],[MinY|MaxY]]) ->
	  min_max(/, MinX, MaxX, MinY, MaxY, BoundsDiv)
	 ;
	  BoundsDiv = empty)).
get_min_max(V, [min_mult_min(X, Y)|max_mult_max(X, Y)], BoundsMult) :- !,
	(number(X), number(Y) ->
	 boundsMultC(Y, [[X|X]], BoundsMult)
	;
	 get_attribute(X, '$fd'(X, XBounds, RestX)),
	 get_attribute(Y, '$fd'(Y, YBounds, RestY)),
	 (number(V) -> true
	 ;
	  NewRest=[min_mult_min(V,X,Y),max_mult_max(V,X,Y)],
	  update_constraints_xy(NewRest, RestX, RestY, NRestX, NRestY),
	  grow_restrictions_chain_xy(X,Y,V, XBounds, YBounds, NRestX, NRestY)
	 ),
	 get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	 (number_list([[MinX|MaxX],[MinY|MaxY]]) ->
	  min_max(*, MinX, MaxX, MinY, MaxY, BoundsMult)
	 ;
	  BoundsMult = empty)).

get_min_max(V, [min_mult_c(X, C)|max_mult_c(X, C)], BoundsMultC) :- !,
	(number(X) ->
	 boundsMultC(C, [[X|X]], BoundsMultC)
	;
	 get_attribute(X, AttrX),	
	 AttrX = '$fd'(X, Bounds, RestX),
	 (number(V) -> true
	 ;	  
	  update_constraints([min_mult_c(V,C),max_mult_c(V,C)],RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX))),
	  boundsMultC(C, Bounds, BoundsMultC)).
get_min_max(_V, [X|Y], [[X|Y]]) .

get_min_max1(V, min(X), Min) :- !,
	(integer(X) -> 
	 Min=X
	;
	 get_attribute(X, AttrX),
	 AttrX = '$fd'(_X, Bounds, RestX),
	 Bounds = [[Min|_Max]|_],
	 (integer(V) -> true
	 ;
	  update_constraints([min(V)], RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX)))).
get_min_max1(V, max(X), Max) :- !,
	(integer(X) ->
	 Max=X
	;
	 get_attribute(X, AttrX),
	 AttrX = '$fd'(X, Bounds, RestX),
	 (mylast(Bounds, [_Min|Max]); true),
	 (integer(V) -> true
	 ;
	  update_constraints([max(V)], RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX)))).
get_min_max1(V, min_plus_c(X, C), Min) :- !,
	(integer(X) -> 
	 Min is X + C
	;
	 get_attribute(X, AttrX),
	 AttrX = '$fd'(_X, Bounds, RestX),
	 Bounds = [[Lb|_Max]|_],
	 Min is Lb + C,
	 (integer(V) -> true
	 ;
	  update_constraints([min_plus_c(V, C)], RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX)))).
get_min_max1(V, max_sub_c(X, C), Max) :- !,
	(integer(X) ->
	 Max is X - C
	;
	 get_attribute(X, AttrX),
	 AttrX = '$fd'(X, Bounds, RestX),
	 ((mylast(Bounds, [_Min|Ub]), Max is Ub - C); true),
	 (integer(V) -> true
	 ;
	  update_constraints([max_sub_c(V, C)], RestX, NRestX),
	  grow_restrictions_chain(X, V, '$fd'(_, Bounds, NRestX)))).

min_max(Op, N1, N2, M1, M2, [[Min|Max]]) :-
	operate(Op, N1, M1, [], S1),
	operate(Op, N1, M2, S1, S2),
	operate(Op, N2, M1, S2, S3),
	operate(Op, N2, M2, S3, S4),
	(S4=[S|_] ->
	 my_min_max(S4, [S|S], [Min|Max])
	;
	 absolute_lower_bound(Min),
	 absolute_upper_bound(Max)).

operate(_, I, _, Acc, [I|Acc]) :- absolute_lower_bound(I), !.
operate(_, _, I, Acc, [I|Acc]) :- absolute_lower_bound(I), !.
operate(_, S, _, Acc, [S|Acc]) :- absolute_upper_bound(S), !.
operate(_, _, S, Acc, [S|Acc]) :- absolute_upper_bound(S), !.
operate(*, N, M, Acc, [S|Acc]) :- S is N*M, !.
operate(/, 0, 0, Acc, Acc) :- !.
operate(/, N, M, Acc, [S|Acc]) :- S is N/M.

my_min_max([], L, L) .
my_min_max([S|Ss], [S1|S2], [Min|Max]) :-
	(S < S1 ->
	 my_min_max(Ss, [S|S2], [Min|Max])
	;
	 (S > S2 ->
	  my_min_max(Ss, [S1|S], [Min|Max])
	 ;
	  my_min_max(Ss, [S1|S2], [Min|Max]))).

boundsSubC(_, [], []) .
boundsSubC(C, [[Bm|BM]|RBs], NewBounds) :- !,
	BCm is Bm - C, 
	BCM is BM - C,
	NewBounds = [[BCm|BCM]|RBCs],
	boundsSubC(C, RBs, RBCs). 
boundsSubC(_,_,_) .

boundsPlusC(_, [], []) .
boundsPlusC(C, [[Bm|BM]|RBs], NewBounds) :- !,
 	BCm is Bm + C, 
 	BCM is BM + C,
	NewBounds = [[BCm|BCM]|RBCs],
	boundsPlusC(C, RBs, RBCs). 
boundsPlusC(_,_,_) .

boundsMultC(_, [], []) .
boundsMultC(C, [[Bm|BM]|RBs], NewBounds) :- !,
 	BCm is Bm * C, 
 	BCM is BM * C,
	NewBounds = [[BCm|BCM]|RBCs],
	boundsMultC(C, RBs, RBCs). 
boundsMultC(_,_,_) .

get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY) :-
	XBounds = [[MinX|_MaxX]|_],
	mylast(XBounds, [_MinX|MaxX]),
	YBounds = [[MinY|_MaxY]|_],
	mylast(YBounds, [_MinY|MaxY]).

update_constraints_xy(NewRest, RestX, RestY, NRestX, NRestY) :-
	  update_constraints(NewRest, RestX, NRestX),
	  update_constraints(NewRest, RestY, NRestY).

update_constraints([], RestX, RestX) .
update_constraints([R1|Rs], RestX, NRestX) :-
	(nocontainsx(RestX, R1) ->
	 NRestX = [R1|NRs],
	 update_constraints(Rs, RestX, NRs)
	;
	 update_constraints(Rs, RestX, NRestX)
	).

mylast([X], X) :- !.
mylast([_|Xs], Last) :- 
	mylast(Xs, Last).

test_if_free(V, Bounds):-
       (get_attribute(V, Attr) ->
	 Attr='$fd'(V, CurrentRange, RestV),
	 get_whole_intersection(CurrentRange, Bounds, Range), !,
	 do_test_if_free(CurrentRange, Range, V, RestV)
       ;
	 attach_attribute(V, '$fd'(V, Bounds, []))
	).

do_test_if_free(Range, Range,_,_) :- !.
do_test_if_free(_CurrentRange, Range, V, RestV) :-
	detach_attribute(V),
	attach_attribute(V, '$fd'(V, Range, RestV)),
	fd_tell(RestV, V, Range).

get_whole_intersection(CurrentRange, Bounds, Range) :-
	get_whole_intersection_aux(CurrentRange, Bounds, Range),
	Range=[_|_].
	
get_whole_intersection_aux([CB1|CBs], Bounds, WholeRange) :-
	get_part_intersection(Bounds, CB1, PartRange),
	get_whole_intersection_aux(CBs, Bounds, Ranges), 
	append(PartRange, Ranges, WholeRange).
get_whole_intersection_aux([], _, []).
	
get_part_intersection([Bound|Bs], CB1, ResultList) :-
	get_intersection(CB1, Bound, Result),
	(Result = [] -> 
	 ResultList = Rs
	;
	 ResultList = [Result|Rs]
	),
	get_part_intersection(Bs, CB1, Rs).
get_part_intersection([], _, []) .

get_intersection([MinV|MaxV], [Min|Max], [NMinV|NMaxV]) :-
	get_new_range_dom(MinV, MaxV, Min, Max, NMinV, NMaxV), ! .	
get_intersection(_,_,[]) .

get_new_range_dom(MinX, MaxX, MinY, MaxY, NMinX, NMaxX) :-
	(MinX =< MaxY, MaxX >= MinY),
	(MinX < MinY ->
	 NMinX = MinY
	;
	 NMinX=MinX),
	(MaxX > MaxY ->
	 NMaxX = MaxY
	;
	 NMaxX = MaxX).

fd_tell([],_,_).
fd_tell([R1|Rs], V, NewRange) :-
   	(prop_number(R1) -> 
	 process_propagation_const(R1, V, NewRange)
   	;
	 process_propagation(R1, V, NewRange)
	),
	fd_tell(Rs, V, NewRange).

prop_number(min_sub_max(X,_,_)) :-  !, integer(X).
prop_number(max_sub_min(X,_,_)) :-  !, integer(X).
prop_number(min_plus_min(X,_,_)) :- !, integer(X).
prop_number(max_plus_max(X,_,_)) :- !, integer(X).
prop_number(min_mult_c(X,_)) :-     !, integer(X).
prop_number(max_mult_c(X,_)) :-     !, integer(X).
prop_number(min_plus_c(X,_)) :-     !, integer(X).
prop_number(max_plus_c(X,_)) :-     !, integer(X).
prop_number(min_sub_c(X,_)) :-      !, integer(X).
prop_number(max_sub_c(X,_)) :-      !, integer(X).
prop_number(min(X)) :-              !, integer(X).
prop_number(max(X)) :-              !, integer(X).
prop_number(min_div_max(X,_,_)) :-  !, integer(X).
prop_number(max_div_min(X,_,_)) :-  !, integer(X).
prop_number(min_mult_min(X,_,_)) :- !, integer(X).
prop_number(max_mult_max(X,_,_)) :- !, integer(X).

process_propagation(min(X), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_infimus(NewBounds, Infimus),
	compatible_bounds_min(Infimus, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max(X), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_supreme(NewBounds, Supreme), 
	compatible_bounds_max(Supreme, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_plus_c(X, C), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
       get_infimus(NewBounds, Infimus),
       Inf_plus_c is Infimus + C,
       compatible_bounds_min(Inf_plus_c, XBounds, NewXBounds),
       continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_sub_c(X, C), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_infimus(NewBounds, Infimus),
	Inf_sub_c is Infimus - C,
	compatible_bounds_min(Inf_sub_c, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_plus_c(X, C), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_supreme(NewBounds, Supreme), 
	Sup_plus_c is Supreme + C,
	compatible_bounds_max(Sup_plus_c, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_sub_c(X, C), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_supreme(NewBounds, Supreme), 
	Sup_sub_c is Supreme - C,
	compatible_bounds_max(Sup_sub_c, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_sub_max(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_sub_max, YBounds, ZBounds, NewInfimus),
	compatible_bounds_min(NewInfimus, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_plus_min(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_plus_min, YBounds, ZBounds, NewInfimus),
	compatible_bounds_min(NewInfimus, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_sub_min(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_sub_min, YBounds, ZBounds, NewSupreme),
	compatible_bounds_max(NewSupreme, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_plus_max(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_plus_max, YBounds, ZBounds, NewSupreme),
	compatible_bounds_max(NewSupreme, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_mult_c(X, C), _, NewBounds) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_infimus(NewBounds, Infimus),
	Inf_mult_c_aux is Infimus * C,
	check_integer(inf, Inf_mult_c_aux, Inf_mult_c),
	compatible_bounds_min(Inf_mult_c, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_mult_c(X, C), _, NewBounds) :- !,
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_supreme(NewBounds, Supreme), 
	Sup_mult_c_aux is Supreme * C,
	check_integer(sup, Sup_mult_c_aux, Sup_mult_c),
	compatible_bounds_max(Sup_mult_c, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_div_max(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_div_max, YBounds, ZBounds, NewInfimus),
	compatible_bounds_min(NewInfimus, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(min_mult_min(X, Y, Z),_, _) :- 
	!, get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_mult_min, YBounds, ZBounds, NewInfimus),
	compatible_bounds_min(NewInfimus, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_div_min(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_div_min, YBounds, ZBounds, NewSupreme),
	compatible_bounds_max(NewSupreme, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(max_mult_max(X, Y, Z),_, _) :- !, 
	get_attribute(X, '$fd'(X, XBounds, RestX)),
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_mult_max, YBounds, ZBounds, NewSupreme),
	compatible_bounds_max(NewSupreme, XBounds, NewXBounds),
	continue_propagation(XBounds, NewXBounds, X, RestX).
process_propagation(equals_i(X), _, [[Min|Min]]) :- 
	get_attribute(X, '$fd'(X, Bounds, _Rest)),!,
	get_whole_intersection(Bounds, [[Min|Min]], [[Min|Min]]),
	X = Min.
process_propagation(equals_i(X),V, _) :- 
	integer(X), !,
	V = X.
process_propagation(differ_i(X), _, [[Min|Min]]) :- 
	get_attribute(X, '$fd'(X, Bounds, Rest)), !,
	UB is Min + 1,
	LB is Min - 1,
	absolute_bounds(U,L),
	get_whole_intersection(Bounds, [[L|LB],[UB|U]], NewBounds),
	continue_propagation(Bounds, NewBounds, X, Rest).
process_propagation(differ_i(X), V, Range) :- 
	integer(X), !,
	(get_attribute(V, '$fd'(V,_,Rest)) -> 
	 UB is X + 1,
	 LB is X - 1,
	 absolute_bounds(U,L),
	 get_whole_intersection(Range, [[L|LB],[UB|U]],
	 NewRange),
	 continue_propagation(Range, NewRange, V, Rest)
	;
	 X =\= V).
process_propagation(_,_,_) .

continue_propagation(XBounds, XBounds, _X, _RestX) :- !.
continue_propagation(_XBounds, NewXBounds, X, _RestX) :-
	check_if_solution(NewXBounds, X, true), !.
continue_propagation(_XBounds, NewXBounds, X, RestX) :-
	detach_attribute(X),
	attach_attribute(X, '$fd'(X, NewXBounds, RestX)),
	fd_tell(RestX, X, NewXBounds).

get_bounds(V, Bounds):- 
	var(V), !, 
	get_attribute(V, '$fd'(_,Bounds,_)).
get_bounds(N, [[N|N]]).

get_complex_infimus(min_sub_max, XBounds, YBounds, NewInfimus) :- !,
	get_infimus(XBounds, Infimus),
	get_supreme(YBounds, Supreme),
	NewInfimus is Infimus - Supreme.
get_complex_infimus(min_plus_min, XBounds, YBounds, NewInfimus) :- !,
	get_infimus(XBounds, Infimus1),
	get_infimus(YBounds, Infimus2),
	NewInfimus is Infimus1 + Infimus2.
get_complex_infimus(min_div_max, XBounds, YBounds, NewInfimus) :- !,
	get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	min_max(/, MinX, MaxX, MinY, MaxY, [[NewInfimus|_NewSupreme]]).
get_complex_infimus(min_mult_min, XBounds, YBounds, NewInfimus) :- !,
	get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	min_max(*, MinX, MaxX, MinY, MaxY, [[NewInfimus|_NewSupreme]]).

get_infimus([[Inf|_Max]|_], Inf) .

get_supreme(Bounds, Sup) :-
	mylast(Bounds, [_Min|Sup]).

compatible_bounds_min(Inf, Bounds,  NewBounds) :-
	compatible_bounds_min_aux(Bounds, Inf, NewBounds),
	NewBounds=[_|_].

compatible_bounds_min_aux([[Min|Max]|XBs], Inf, NewXBounds) :-
	(Inf =< Max -> 
	 (Inf > Min ->
	  NMin=Inf
	 ;
	  NMin=Min
	 ),
	 NewXBounds=[[NMin|Max]|NXBs]
	;
	 NewXBounds=NXBs
	),
	compatible_bounds_min_aux(XBs, Inf, NXBs).	 
compatible_bounds_min_aux([], _, []).

get_complex_supreme(max_sub_min, XBounds, YBounds, NewSupreme) :- !,
	get_supreme(XBounds, Supreme),
	get_infimus(YBounds, Infimus),
	NewSupreme is Supreme - Infimus.
get_complex_supreme(max_div_min, XBounds, YBounds, NewSupreme) :- !,
	get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	min_max(/, MinX, MaxX, MinY, MaxY, [[_Infimus|NewSupreme]]).
get_complex_supreme(max_plus_max, XBounds, YBounds, NewSupreme) :-
	get_supreme(XBounds, Supreme1),
	get_supreme(YBounds, Supreme2),
	NewSupreme is Supreme1 + Supreme2.
get_complex_supreme(max_mult_max, XBounds, YBounds, NewSupreme) :-
	get_extreme_bounds(XBounds, YBounds, MinX, MaxX, MinY, MaxY),
	min_max(*, MinX, MaxX, MinY, MaxY, [[_Infimus|NewSupreme]]).

check_integer(_BoundType, M, M) :- 
	integer(M), !.
check_integer(BoundType, M, Result) :- 
	IntM is integer(M),
	arith_exp(P),
	check_integer_aux(BoundType, M, IntM, P, Result).

check_integer_aux(inf, M, IntM, P, R) :-
	Trunc is abs(M - IntM),
	(Trunc < P ->
	 R = IntM
	;
	 (IntM >= 0 ->
	  R is IntM + 1
	 ;
	  R = IntM)
	).
check_integer_aux(sup, M, IntM, P, R) :-
	Trunc is abs(M - IntM),
	(Trunc < P ->
	 R = IntM
	;
	 (IntM >= 0 ->
	  R = IntM
	 ;
	  R is IntM - 1)
	).

grow_restrictions_chain_xy(X,Y,V, XBounds, YBounds, NRestX, NRestY) :-
	grow_restrictions_chain(X, V, '$fd'(_, XBounds, NRestX)),
	grow_restrictions_chain(Y, V, '$fd'(_, YBounds, NRestY)).

grow_restrictions_chain(X, _, '$fd'(_, Bounds, Restrictions)) :-
	detach_attribute(X),
	attach_attribute(X, '$fd'(X, Bounds, Restrictions)).

compatible_bounds_max(Sup, Bounds,  NewBounds) :-
	compatible_bounds_max_aux(Bounds,  Sup, NewBounds),
	NewBounds = [_|_].

compatible_bounds_max_aux([], _, []).
compatible_bounds_max_aux([[Min|Max]|XBs], Sup, NewXBounds) :-
	(Sup >= Min -> 
	 (Sup < Max ->
	  NMax=Sup
	 ;
	  NMax=Max
	 ),
	 NewXBounds=[[Min|NMax]|NXBs]
	;
	 NewXBounds=NXBs
	),
	compatible_bounds_max_aux(XBs, Sup, NXBs).

%%%% Not compulsory in some cases:
process_propagation_const(min(X),_, Range) :- !, 
	get_infimus(Range, Infimus),
	Infimus =< X.
process_propagation_const(max(X),_,Range) :- !, 
	get_supreme(Range, Supreme),
	Supreme >= X.
process_propagation_const(min_plus_c(X, C),_,Range) :- !,
	get_infimus(Range, Infimus),
	Inf_plus_c is Infimus + C,
	Inf_plus_c =< X.
process_propagation_const(min_sub_c(X, C),_,Range) :- !,
	get_infimus(Range, Infimus),
	Inf_sub_c is Infimus - C,
	Inf_sub_c =< X.
process_propagation_const(max_plus_c(X, C),_,Range) :- !,
	get_supreme(Range, Supreme),
	Sup_plus_c is Supreme + C,
	Sup_plus_c >= X.
process_propagation_const(max_sub_c(X, C),_,Range) :- !,
	get_supreme(Range, Supreme),
	Sup_sub_c is Supreme - C,
	Sup_sub_c >= X.
process_propagation_const(min_sub_max(X, Y, Z),_,_) :- !, 
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_sub_max, YBounds, ZBounds, Infimus),
	Infimus =< X.
process_propagation_const(min_plus_min(X, Y, Z),_,_) :- !, 
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_plus_min, YBounds, ZBounds, Infimus),
	Infimus =< X.
process_propagation_const(max_sub_min(X, Y, Z),_,_) :- !, 
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_sub_min, YBounds, ZBounds, Supreme),
	Supreme >= X.
process_propagation_const(max_plus_max(X, Y, Z),_,_) :- !,
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_plus_max, YBounds, ZBounds, Supreme),
	Supreme >= X.
%%%% Kernel checks:
process_propagation_const(min_mult_c(X, C), _, Range) :- !,
	get_infimus(Range, Infimus),
	Inf_mult_c_aux is Infimus * C,
	check_integer(inf, Inf_mult_c_aux, Inf_mult_c),
	Inf_mult_c =< X.
process_propagation_const(max_mult_c(X, C), _, Range) :- !,
	get_supreme(Range, Supreme),
	Sup_mult_c_aux is Supreme * C,
	check_integer(sup, Sup_mult_c_aux, Sup_mult_c),
	Sup_mult_c >= X.
process_propagation_const(max_div_min(X, Y, Z),_,_) :- !,
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_supreme(max_div_min, YBounds, ZBounds, Supreme),
	Supreme >= X.
process_propagation_const(min_div_max(X, Y, Z),_,_) :- !,
	get_bounds(Y, YBounds),
	get_bounds(Z, ZBounds),
	get_complex_infimus(min_div_max, YBounds, ZBounds, Infimus),
	Infimus =< X.
process_propagation_const(_A,_,_). % :- display(unserved_ppc(A)), nl.

pitm(V, Point) :-
	get_attribute(V, '$fd'(V, Bounds,_)),
	get_infimus(Bounds, Infimus),
	get_supreme(Bounds, Supreme),
	Paux is (Infimus + Supreme)/2,
	Point is integer(Paux).


labeling([]).
labeling(L) :- 
	choose_var(L, V, R),
 	choose_value(V, Value),
	add_constraint(V, Value),
	labeling(R).

add_constraint(V, Value) :-
	V = Value.


choose_var([X|Xs], X, Xs).


choose_free_var(Vs, V) :-
	choose_var(Vs, Var, R),
	(ground(Var) ->
	 choose_free_var(R, V)
	;
	 V = Var).


choose_var_nd(L, X) :-
	member(X, L).


choose_value(V, V) :-
  	number(V), !.
choose_value(V, Value) :-
  	get_attribute(V, '$fd'(V, Bounds,_)),
  	enumerate_all(Bounds, Value).

enumerate_all([[Min0|Max]|_], R):-
	Min is integer(Min0),
	enum(Min, Max, R).
enumerate_all([_|L], R):- 
	enumerate_all(L, R).

enum(M, N, M):- 
	M =< N.
enum(M, N, R):- 
	M < N,
	M1 is M + 1,
	enum(M1, N, R).

retrieve_range(X, Range) :-
	var(X), !,
	get_attribute(X, '$fd'(X, Range,_)).
retrieve_range(X, [[X|X]]).

retrieve_store(X, Store) :-
	var(X), !,
	get_attribute(X, '$fd'(X,_, Store)).
retrieve_store(_, []).

glb(X, X) :- number(X), !.
glb(X, L) :- 
	get_attribute(X, '$fd'(X, Range,_)),
	Range = [[L|_U]|_].

lub(X, X) :- number(X), !.
lub(X, U) :- 
	get_attribute(X, '$fd'(X, Range,_)),
	mylast(Range, I),
	I = [_L|U].

bounds(X, L, U) :-
	glb(X,L),
	lub(X,U).

retrieve_list_of_values(V, L) :-
%	display('Retrieving list of values: '),
  	get_attribute(V, '$fd'(V, Bounds,_)),
%	display(retrieve_list_of_values(Bounds)),nl,
	enumerate_all(Bounds,L).
