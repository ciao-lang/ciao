
:- module(lsets,
	[ merge_list_of_lists/2,
	  merge_lists/3, 
	  merge_each/3,
	  ord_member_list_of_lists/2,
	  delete_var_from_list_of_lists/4,
	  setproduct_lists/4,
	  ord_intersect_lists/2,
	  ord_intersect_all/2,
	  sort_list_of_lists/2,
	  transitive_closure_lists/3,
	  closure_under_union/2,
	  ord_split_lists/4,
	  ord_split_lists_from_list/4,
	  split_lists_from_list/4,
	  powerset_of_set_of_sets/2
	],[assertions]).

:- use_module(library(lists), [powerset/2]).
:- use_module(library(sets), 
	[merge/3,
	 ord_delete/3,ord_intersect/2,ord_intersection/3,
	 ord_member/2,ord_test_member/3
	]).
:- use_module(library(sort)).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

merge_list_of_lists([],[]).
merge_list_of_lists([Xs,Ys|Rest],Result) :-
	merge(Xs,Ys,Zs),
	merge_list_of_lists([Zs|Rest],Result).
merge_list_of_lists([Xs],Xs).

%-------------------------------------------------------------------------
% merge_lists(+,+,-)                                                     |
% It merges each list in the first argument with each list contained     |
% in the second argument and later make an append (difference lists)     |
%-------------------------------------------------------------------------

merge_lists([],Y,Y).
merge_lists([X|Xs],Y,Z):-
	merge_lists_dl(Xs,X,Y,Temp_Z-[]),
	sort(Temp_Z,Z).

merge_lists_dl([],L,LL,Merged):- 
	merge_each_dl(LL,L,Merged).
merge_lists_dl([L1|Ls],L,LL,Merged):- 
	merge_each_dl(LL,L,First),
	merge_lists_dl(Ls,L1,LL,More),
	append_dl(First,More,Merged).

append_dl(X-Y,Y-Z,X-Z).

% It merges L with each list contained in the second argument

merge_each(X,Y,Z):-
	merge_each_dl(Y,X,Z-[]).

merge_each_dl([],L,[L|X]-X).
merge_each_dl([L1|Ls],L,Merged):- 
	merge_each_dl2(Ls,L1,L,Merged).

merge_each_dl2([],L1,L,[Merge|X]-X):- 
	merge(L,L1,Merge).
merge_each_dl2([LL|Ls],L1,L,[L2|More]-X):-
	merge(L,L1,L2),
	merge_each_dl2(Ls,LL,L,More-X).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

ord_member_list_of_lists(X,[Xs|_]) :-
	ord_member(X,Xs), !.
ord_member_list_of_lists(X,[_|Xss]) :-
	ord_member_list_of_lists(X,Xss).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

delete_var_from_list_of_lists(X,[Ys|Yss],List_of_lists,Ans) :-
	ord_delete(Ys,X,New_Ys),
	( New_Ys = [] ->
	    Ans = yes,
	    List_of_lists = New_Yss
	; Ans = Ans1,
	  List_of_lists = [New_Ys|New_Yss]
        ),
	delete_var_from_list_of_lists(X,Yss,New_Yss,Ans1).
delete_var_from_list_of_lists(_,[],[],no).

%-------------------------------------------------------------------------
% setproduct_lists(+,+,-)                                                |
% setproduct_lists(Xss,Yss,Zss,Tail)                                     |
% It computes Zss = { merge(Xs,Ys) | Xs in Xss and Ys in Yss }.          |
%-------------------------------------------------------------------------

setproduct_lists([], _, Zss,Zss).
setproduct_lists([Xs|Xss], Yss, Zss,Tail)  :-
	setproduct_lists0(Yss, Xs, Zss, Rest),
	setproduct_lists(Xss, Yss, Rest, Tail).

setproduct_lists0([], _, Zss, Zss).
setproduct_lists0([Ys|Yss], Xs, [Zs|Zss], Tail) :-
	merge(Ys,Xs,Zs),
	setproduct_lists0(Yss, Xs, Zss, Tail).

%-------------------------------------------------------------------------
% ord_intersect_lists(+,+)                                               |
% ord_intersect_lists(Vars,Sh)                                           |
% Satisfied if at least one variable in the nonempty set of variables    |
% Vars is in one list of the list of lists of variables Sh. Both ordered |
%-------------------------------------------------------------------------

ord_intersect_lists([X|Xs],[Ls|Lss]):- 
	Ls = [Y|Ys],
	compare(D,X,Y),
	has_intersection_compared(D,X,Xs,Y,Ys,Flag,NewVars),
	ord_intersect_lists1(Flag,NewVars,Lss).

ord_intersect_lists1(yes,_NewVars,_Lss).
ord_intersect_lists1(no,NewVars,Lss):-
	ord_intersect_lists(NewVars,Lss).

%-------------------------------------------------------------------------
% ord_intersect_all(+,-)                                                 |
% ord_intersect_all(Xss,Int)                                             |
% Int is the ordered list of common elements between all ordered lists   |
% in Xss                                                                 |
%-------------------------------------------------------------------------

ord_intersect_all([],[]).
ord_intersect_all([E],E).
ord_intersect_all([E1,E2|More],Intersection):-
	ord_intersection(E1,E2,E),
	ord_intersect_all([E|More],Intersection).

%-------------------------------------------------------------------------
% sort_list_of_lists(+,-)                                                |
% sort_list_of_lists(Xss,Xss_s)                                          |
% sorts!                                                                 |
%-------------------------------------------------------------------------

sort_list_of_lists(Xss,Xss_s):-
	sort_each_list(Xss,Yss),
	sort(Yss,Xss_s).

sort_each_list([],[]).
sort_each_list([Xs|Xss],[Ys|Yss]):-
	sort(Xs,Ys),
	sort_each_list(Xss,Yss).


%-------------------------------------------------------------------------
% transitive_closure_lists(+,+,-)                                        |
% transitive_closure_lists(Xss,Singletons,Closure)                       |
% It starts with a list of Singletons and obtains in Closure the         |
% transitive closure of the list of lists LL.                            |
%-------------------------------------------------------------------------

transitive_closure_lists([],Closure,Closure).
transitive_closure_lists([Xs|Xss],TempClosure,Closure) :-
	split_lists_from_list(Xs,TempClosure,Intersect,Not_intersect),
	merge_list_of_lists(Intersect,Merged),
	transitive_closure_lists(Xss,[Merged|Not_intersect],Closure).

%-------------------------------------------------------------------------
% closure_under_union(+,-)                                               |
% closure_under_union(Xss,Star)                                          |
% Star is the closure under union of Xss                                 |
%-------------------------------------------------------------------------

closure_under_union(Sh,Star) :-
	star1(Sh,[],Star).

star1([],L,L).
star1([Xs|Xss],Temp,Star) :-
	add_to_star(Temp,Xs,Temp1),
	sort(Temp1,Temp2),
	star1(Xss,Temp2,Star).

add_to_star([],Xs,[Xs]).
add_to_star([Ys|Yss],Xs,[Ys,Zs|Arg_share_star]) :-
	merge(Ys,Xs,Zs),
	add_to_star(Yss,Xs,Arg_share_star).

%-------------------------------------------------------------------------
% ord_split_lists(+,+,-,-)                                               |
% ord_split_lists(OrdXss,X,Intersect,Disjunct)                           |
% Split the ordered list of lists Xss into two lists: Intersect contains |
% the elements in Xss containing X, Disjunct those not containing X      |
%-------------------------------------------------------------------------

ord_split_lists([],_,[],[]).
ord_split_lists([[L1|Ls1]|Ls],X,Intersect,Disjoint):-
	compare(D,X,L1),
	split_lists1(D,X,L1,Ls1,Ls,Intersect,Disjoint).

split_lists1(<,_,L1,Ls1,Ls,[],[[L1|Ls1]|Ls]).
split_lists1(=,X,L1,Ls1,Ls,[[L1|Ls1]|Intersect],Disjoint):-
	ord_split_lists(Ls,X,Intersect,Disjoint).
split_lists1(>,X,L1,Ls1,Ls,Intersect,Disjoint):-
	ord_test_member(Ls1,X,Flag),
	split_lists2(Flag,X,L1,Ls1,Ls,Intersect,Disjoint).

split_lists2(yes,X,L1,Ls1,Ls,[[L1|Ls1]|Intersect],Disjoint):-
	ord_split_lists(Ls,X,Intersect,Disjoint).
split_lists2(no,X,L1,Ls1,Ls,Intersect,[[L1|Ls1]|Disjoint]):-
	ord_split_lists(Ls,X,Intersect,Disjoint).

%-------------------------------------------------------------------------
% ord_split_lists_from_list(+,+,-,-)
% ord_split_lists_from_list(Vars,Xss,Intersect,Disjunct)
% Split the list of lists in the second argument into two lists: in
% the third argument gives the lists containing at least one variable
% of the list in the first argument, in the fourth  argument gives the
% lists which do not contain any variable of it (Not necessarily ordered)
%-------------------------------------------------------------------------

ord_split_lists_from_list([],Xss,[],Xss):- !.
ord_split_lists_from_list(Xs,Xss,Intersect,Disjunct):-
	ord_split_lists_from_list1(Xss,Xs,Intersect,Disjunct).
	
ord_split_lists_from_list1([],_,[],[]).
ord_split_lists_from_list1([L|Ls],Vars,Intersect,Disjunct):-
	Vars = [X|Xs],
	L = [Y|Ys],
	compare(D,X,Y),
	has_intersection_compared(D,X,Xs,Y,Ys,Flag,NewVars),
	ord_split_lists_from_list2(Flag,NewVars,L,Ls,Intersect,Disjunct).

ord_split_lists_from_list2(end,_NewVars,L,Ls,[],[L|Ls]).
ord_split_lists_from_list2(yes,NewVars,L,Ls,[L|Intersect],Disjunct):-
	ord_split_lists_from_list1(Ls,NewVars,Intersect,Disjunct).
ord_split_lists_from_list2(no,NewVars,L,Ls,Intersect,[L|Disjunct]):-
	ord_split_lists_from_list1(Ls,NewVars,Intersect,Disjunct).
	
has_intersection_compared(=,X,Xs,_Y,_Ys,yes,[X|Xs]).
has_intersection_compared(<,_X,[],_Y,_Ys,Flag,_NewVars):- !,
	Flag = end.
has_intersection_compared(<,_,[X|Xs],Y,Ys,Flag,NewVars):-
	compare(D,X,Y),
	has_intersection_compared(D,X,Xs,Y,Ys,Flag,NewVars).
has_intersection_compared(>,X,Xs,_Y,[],Flag,NewVars):- !,
	NewVars = [X|Xs],
	Flag = no.
has_intersection_compared(>,X,Xs,_,[Y|Ys],Flag,NewVars):- 
	NewVars = [X|Xs],
	compare(D,X,Y),
	has_intersection_next(D,X,Xs,Y,Ys,Flag).

has_intersection_next(=,_X,_Xs,_Y,_Ys,yes).
has_intersection_next(<,_X,[],_Y,_Ys,Flag):- !,
	Flag = no.
has_intersection_next(<,_,[X|Xs],Y,Ys,Flag):-
	compare(D,X,Y),
	has_intersection_next(D,X,Xs,Y,Ys,Flag).
has_intersection_next(>,_X,_Xs,_Y,[],Flag):- !,
	Flag = no.
has_intersection_next(>,X,Xs,_,[Y|Ys],Flag):-
	compare(D,X,Y),
	has_intersection_next(D,X,Xs,Y,Ys,Flag).

%-------------------------------------------------------------------------
% split_lists_from_list(+,+,-,-)                                         |
% split_lists_from_list(Xs,Xss,Int,NotInt)                               |
% Split the list of lists Xss into two lists of lists: Int contains the  |
% elements in Xss which intersect with Xs, NotInt those with empty       |
% intersection.                                                          |
%-------------------------------------------------------------------------

split_lists_from_list([],L,[],L).
split_lists_from_list([X|Xs],Xss,Int,NotInt):-
	split_lists_from_list1(Xss,[X|Xs],Int,NotInt).

split_lists_from_list1([],_,[],[]).
split_lists_from_list1([Xs|Xss],Ys,Int,NotInt):-
	has_intersection(Xs,Ys,Flag),
	construct_sets(Flag, Xs, Int, NotInt, Int1, NotInt1),
	split_lists_from_list1(Xss, Ys, Int1, NotInt1).

construct_sets(yes, L, [L|Intersect], D, Intersect, D).
construct_sets(no, L, Intersect, [L|D], Intersect, D).

has_intersection(Xs,Ys,Flag):-
	ord_intersect(Xs,Ys),!,
	Flag = yes.
has_intersection(_,_,no).

%-------------------------------------------------------------------------
% powerset_of_set_of_sets(+,-)                                           |
% powerset_of_set_of_sets(Xss,Powerset)                                  | 
% Xss is a list of lists of variables. It computes:                      | 
%  {S | exists Ys in Xss, S subseteq Ys}                                 | 
%-------------------------------------------------------------------------

powerset_of_set_of_sets(Xss,Pw):- 
	powerset_of_set_of_sets_dl(Xss,[],Pw).

powerset_of_set_of_sets_dl([],Pw,Pw).
powerset_of_set_of_sets_dl([Xs|Xss],Temp,Pw):- 
	powerset(Xs,Ys),
	sort(Ys,Ys_s),
	merge(Ys_s,Temp,Temp1),
	powerset_of_set_of_sets_dl(Xss,Temp1,Pw).
