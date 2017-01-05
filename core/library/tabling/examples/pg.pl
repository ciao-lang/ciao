:- module(pg, 
	[
	    test/1, 
	    result/1,
	    spend_time/2
	]).

:- include(tabling_type).

:- dynamic results/1.
:- set_prolog_flag(multi_arity_warnings, off).

 %% :- bridge /(pds1,4).
 %% :- bridge /(mergedelete,3).
 %% :- bridge /(rev,3).
 %% :- bridge /(dif,5).
 %% :- bridge /(iota1,3).
 %% :- bridge /(my_member,2).

:- table set_unify/2.
:- table pdsbm__1/2.
:- table pds__1/2.
:- table pds1__1/4.
:- table check__1/5.
:- table mergedelete__1/3.
:- table rev__1/3.
:- table dif__1/5.
:- table iota1__1/3.
:- table iota__1/2.
:- table member__1/2.

spend_time(NT,T) :-
        statistics(runtime,[_,_]),
	tp, abolish_all_tables,
        statistics(runtime,[_,Tt]),
	N is NT / Tt,
        statistics(runtime,[_,_]),
        (
            between(1,N,_),
            tp, abolish_all_tables,
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin]),
        (
            between(1,N,_),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin2]),
        T is (Tfin - Tfin2) / N.

test(_) :- abolish_all_tables, tp, fail.
test(L) :- show_facts, get_list(L).

get_list([R|L]) :-
	retract(results(R)), !,
	get_list(L).

get_list([]).

unify_sets([],[]).
unify_sets([A|As],[B|Bs]) :- 
       set_unify(A,B), unify_sets(As,Bs).


%---------------- Transformed program ----------------------

member__1(_141,_143) :- 
        [_119] = _145, [list,_106] = _147, 
        normalize_result([_145,_147],
                          [_141,_143]).
member__1(_215,_217) :- 
        [_191] = _219, [list,_195] = _221, 
        my_member([_191],[_195]),
        normalize_result([_219,_221],
                         [_215,_217]).
iota__1(_226,_228) :- 
        [_201] = _230, [_205] = _232, 
        iota1([num],[_201],[_205]),
        normalize_result([_230,_232],
                         [_226,_228]).
iota1__1(_149,_151,_153) :- 
        [_129] = _155, [_129] = _157, [list] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
iota1__1(_362,_364,_366) :- 
        [_313] = _368, [_332] = _370, [list,_336] = _372, 
        'my is'([_328],[[_313],[num]]),
        iota1([_328],[_332],[_336]),
        normalize_result([_368,_370,_372],
                         [_362,_364,_366]).
dif__1(_197,_199,_201,_203,_205) :- 
        [list] = _207, [_163] = _209, [_167] = _211, [list] = _213, [list] = _215, 
        normalize_result([_207,_209,_211,_213,_215],
                          [_197,_199,_201,_203,_205]).
dif__1(_593,_595,_597,_599,_601) :- 
        [list,_542] = _603, [_546] = _605, [_550] = _607, [list,_554] = _609, [list,_558] = _611, 
        'my is'([_531],[[_546],[_507]]),
        'my is'([_518],[[_550],[_531]]),
        dif([_542],[_546],[_550],[_554],[_558]),
        normalize_result([_603,_605,_607,_609,_611],
                         [_593,_595,_597,_599,_601]).
rev__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
rev__1(_279,_281,_283) :- 
        [list,_246] = _285, [_227] = _287, [_256] = _289, 
        rev([_246],[list,_227],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
mergedelete__1(_149,_151,_153) :- 
        [list] = _155, [_133] = _157, [_133] = _159, 
        normalize_result([_155,_157,_159],
                          [_149,_151,_153]).
mergedelete__1(_279,_281,_283) :- 
        [list,_248] = _285, [list,_252] = _287, [_256] = _289, 
        mergedelete([_248],[_252],[_256]),
        normalize_result([_285,_287,_289],
                         [_279,_281,_283]).
mergedelete__1(_364,_366,_368) :- 
        [list,_84] = _370, [list,_334] = _372, [list,_338] = _374, 
        'my >'([_310],[_319]),
        mergedelete([list,_84],[_334],[_338]),
        normalize_result([_370,_372,_374],
                         [_364,_366,_368]).
check__1(_207,_209,_211,_213,_215) :- 
        [list] = _217, [_173] = _219, [_181] = _221, [_181] = _223, [_185] = _225, 
        normalize_result([_217,_219,_221,_223,_225],
                          [_207,_209,_211,_213,_215]).
check__1(_717,_719,_721,_723,_725) :- 
        [_612] = _727, [_616] = _729, [_642] = _731, [_678] = _733, [_620] = _735, 
        'my ='([_612],[list,_218]),
        dif([_612],[_616],[_620],[_638],[_654]),
        mergedelete([_638],[_642],[_674]),
        rev([_654],[list],[_670]),
        mergedelete([_670],[_674],[_678]),
        normalize_result([_727,_729,_731,_733,_735],
                         [_717,_719,_721,_723,_725]).
pds1__1(_174,_176,_178,_180) :- 
        [list] = _182, [_147] = _184, [list] = _186, [_155] = _188, 
        normalize_result([_182,_184,_186,_188],
                          [_174,_176,_178,_180]).
pds1__1(_544,_546,_548,_550) :- 
        [_480] = _552, [_472] = _554, [list,_508] = _556, [_512] = _558, 
        my_member([_476],[_480]),
        check([_472],[_476],[_480],[_498],[_512]),
        pds1([_498],[list,_472],[_508],[_512]),
        normalize_result([_552,_554,_556,_558],
                         [_544,_546,_548,_550]).
pds__1(_506,_508) :- 
        [_428] = _510, [list,_474] = _512, 
        'my is'([_478],[[[_428],[[_428],[num]]],[num]]),
        iota([_478],[list,_464]),
        pds1([_464],[list,list],[_474],[_478]),
        normalize_result([_510,_512],
                         [_506,_508]).
pdsbm__1(_225,_227) :- 
        [_197] = _229, [_183] = _231, 
        pds([_197],[list,list,_183]),
        normalize_result([_229,_231],
                         [_225,_227]).

%---------------- Definitions of tabled preds --------------
pdsbm(_63,_65) :- 
        pdsbm__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
pds(_63,_65) :- 
        pds__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
pds1(_63,_65,_67,_69) :- 
        pds1__1(_71,_73,_75,_77),
        unify_sets([_63,_65,_67,_69], [_71,_73,_75,_77]).
check(_63,_65,_67,_69,_71) :- 
        check__1(_73,_75,_77,_79,_81),
        unify_sets([_63,_65,_67,_69,_71], [_73,_75,_77,_79,_81]).
mergedelete(_63,_65,_67) :- 
        mergedelete__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
rev(_63,_65,_67) :- 
        rev__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
dif(_63,_65,_67,_69,_71) :- 
        dif__1(_73,_75,_77,_79,_81),
        unify_sets([_63,_65,_67,_69,_71], [_73,_75,_77,_79,_81]).
iota1(_63,_65,_67) :- 
        iota1__1(_69,_71,_73),
        unify_sets([_63,_65,_67], [_69,_71,_73]).
iota(_63,_65) :- 
        iota__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).
my_member(_63,_65) :- 
        member__1(_67,_69),
        unify_sets([_63,_65], [_67,_69]).

%---------------- Tp ---------------------------------------

tp :- pdsbm__1(_64,_66), fail.
tp :- pds__1(_64,_66), fail.
tp :- pds1__1(_64,_66,_68,_70), fail.
tp :- check__1(_64,_66,_68,_70,_72), fail.
tp :- mergedelete__1(_64,_66,_68), fail.
tp :- rev__1(_64,_66,_68), fail.
tp :- dif__1(_64,_66,_68,_70,_72), fail.
tp :- iota1__1(_64,_66,_68), fail.
tp :- iota__1(_64,_66), fail.
tp :- member__1(_64,_66), fail.
tp.


%---------------- Builtin Preds ----------------------------

'my ='(X1,X2) :- 'my =__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my ==='(X1,X2) :- 'my ===__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my =='([num],[num]).
'my is'(X1,X2) :- 'my is__1'(Y1,Y2), unify_sets([X1,X2],[Y1,Y2]).
'my <'([num],[num]).
'my >'([num],[num]).
'my >='([num],[num]).
'my =<'([num],[num]).
'my =:='([num],[num]).

'my =__1'(X,X).
'my ===__1'(_,_).
'my is__1'(num,num).


%---------------- Show Result ------------------------------

show_facts :- pdsbm__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(pdsbm(_63,_65))), fail.
show_facts :- pds__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(pds(_63,_65))), fail.
show_facts :- pds1__1(_63,_65,_67,_69),
              numbervars([_63,_65,_67,_69]),
              assert(results(pds1(_63,_65,_67,_69))), fail.
show_facts :- check__1(_63,_65,_67,_69,_71),
              numbervars([_63,_65,_67,_69,_71]),
              assert(results(check(_63,_65,_67,_69,_71))), fail.
show_facts :- mergedelete__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(mergedelete(_63,_65,_67))), fail.
show_facts :- rev__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(rev(_63,_65,_67))), fail.
show_facts :- dif__1(_63,_65,_67,_69,_71),
              numbervars([_63,_65,_67,_69,_71]),
              assert(results(dif(_63,_65,_67,_69,_71))), fail.
show_facts :- iota1__1(_63,_65,_67),
              numbervars([_63,_65,_67]),
              assert(results(iota1(_63,_65,_67))), fail.
show_facts :- iota__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(iota(_63,_65))), fail.
show_facts :- member__1(_63,_65),
              numbervars([_63,_65]),
              assert(results(member(_63,_65))), fail.
show_facts.
%----------------------------------------------------------

set_unify(A,B) :-
	flatten(A,AF), flatten(B,BF),
        (my_ground(AF), my_ground(BF) -> AF==BF
          ;
         ord_union(AF,BF,ToCover),
         ord_setproduct(AF,BF,AxB),
         drop_nonuni(AxB,UTable),
         covering_u_table(UTable,ToCover,CovUTable),
         keysort(CovUTable,CovUTableS),
         table2graph(CovUTableS,CovUGraph),
         u_graph_proceed(CovUGraph)
        ).

drop_nonuni([],[]).
drop_nonuni([X-Y|AxB],Out) :-
        (\+ \+ X=Y -> Out = [X-Y|NewAxB] ; Out = NewAxB),
        drop_nonuni(AxB,NewAxB).

covering_u_table([],[],[]).
covering_u_table([X-Y|XsYs],ToCover,[X-L,Y-L|UTRest]) :-
        (X @< Y ->
         ord_subtract(ToCover,[X,Y],ToCoverRest)
          ;
         ord_subtract(ToCover,[Y,X],ToCoverRest)),
        covering_u_table(XsYs,ToCoverRest,UTRest).
covering_u_table([_|XsYs],ToCover,UTable) :-
        covering_u_table(XsYs,ToCover,UTable).

u_graph_proceed([]).
u_graph_proceed([V-E|G]) :- match_single(V,E), u_graph_proceed(G).

match_single(S,Y) :-
	var(S) -> S=Y ; match_list(Y,S).	% simple atom

match_list([],_).
match_list([S|Xs],S) :- match_list(Xs,S).

normalize_result(Args,SortedArgsC) :-!,
        flatten_sets(Args,ArgsF),
        occur_graph(ArgsF,OG),
        swap_keys_values(OG,GO),
        keysort(GO,GOS),
        drop_equiv(GOS,NewGO),
        graph2table(NewGO,OGTable),
	keysort(OGTable,OGTableS),	% was merge_keysort
        table2graph(OGTableS,KeysArgs),
        keys_and_values(KeysArgs,_,ArgsC),
	sortall(ArgsC,SortedArgsC).

sortall([], []).
sortall([H|T], [NH|NT]) :- sort(H,NH), sortall(T,NT).

drop_equiv([], []).
drop_equiv([One|Rest], Out) :- drop_equiv(One, Rest, Out).

%:- index drop_equiv/3-2.

drop_equiv(X, [], [X]).
drop_equiv(E1-V1, [E2-V2|Vs],Out) :-
        (E1==E2, my_variant(V1,V2) ->
         V1=V2, Out=OutRest
          ;
         Out=[E1-V1|OutRest]),
        drop_equiv(E2-V2, Vs, OutRest).

og_table([],_,[]).
og_table([One|Args],I,OG_Table) :- og_table(One,Args,I,OG_Table).

og_table([],Args,I,OG_Table) :- !, J is I+1, og_table(Args,J,OG_Table).
og_table([V|Vs],Args,I,[V-I|OG_Table]) :- og_table(Vs,Args,I,OG_Table).

table2graph([],[]).
table2graph([One|Rest],Out) :- table2graph(One,Rest,Out).

%:- index table2graph/3-2.

table2graph(V-I,[],[V-[I]]).
table2graph(V-I,[W-J|Vs],Out) :-
        ( V==W ->
          Out = [V-[I|Is]|VsClps], Cont = [V-Is|VsClps]
          ;
          Out = [V-[I]|VsClps], Cont = VsClps),
        table2graph(W-J,Vs,Cont).

graph2table([],[]).
graph2table([Elem-V|Vs],Edges) :- graph2table(Elem,V,Vs,Edges).

graph2table([],_,Vs,Edges) :- graph2table(Vs,Edges).
graph2table([E|Es],V,Vs,[E-V|Edges]) :- graph2table(Es,V,Vs,Edges).

swap_keys_values([],[]).
swap_keys_values([Key-Val|KVs],[Val-Key|VKs]) :- swap_keys_values(KVs,VKs).

/*------------------ Bypasses a bug of SWI -----------------------------

merge_keysort([],[]) :- !.
merge_keysort([X],[X]) :- !.
merge_keysort(VL,VS) :-
        split(VL,VL1,VL2),
        merge_keysort(VL1,VL1S), merge_keysort(VL2,VL2S),
        merge(VL1S,VL2S,VS).

split([],[],[]) :- !.
split([X],[X],[]) :- !.
split([X1,X2|Xs],[X1|X1s],[X2|X2s]) :- split(Xs,X1s,X2s).

merge([],[],[]) :- !.
merge(L,[],L) :- !.
merge([],L,L) :- !.
merge([K1-V1|X1s],[K2-V2|X2s],[K1-V1|Xs]) :- K1@<K2, !,
        merge(X1s,[K2-V2|X2s],Xs).
merge([X1|X1s],[X2|X2s],[X2|Xs]) :- merge([X1|X1s],X2s,Xs).

  ----------------------------------------------------------------------*/

%%-----------------------------------------------------------------------------

keys_and_values([], [], []).
keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
   keys_and_values(Pairs, Keys, Values).

%%-----------------------------------------------------------------------------
flatten(Set,SetF) :- colterms(Set,[],SetMemb), sort(SetMemb,SetF).

colterms(V,I,O) :- var(V), !, O = [V|I].
colterms([],I,O) :- !, O = I.
colterms(A,I,O) :- atom(A), !, O=[A|I].
colterms([T|Ts],I,O) :- colterms(T,I,IT), colterms(Ts,IT,O).

flatten_sets([],[]).
flatten_sets([T|Ts],[FT|FTs]) :- flatten(T,FT), flatten_sets(Ts,FTs).

%%-----------------------------------------------------------------------------
%% Operations on graphs

insert_vertice([],N,[N]).
insert_vertice([V-E|G],V1-E1,[V-EE1|G]) :- V==V1, ord_union(E,E1,EE1), !.
insert_vertice([N|G],N1,[N|NewG]) :- insert_vertice(G,N1,NewG).

%%-----------------------------------------------------------------------------
occur_graph(ArgsVs,OG) :-
        og_table(ArgsVs,0,OG_Table),
        sort(OG_Table,OG_TableSorted),
        table2graph(OG_TableSorted,OG).

occur_graph(ArgsV,OG) :- occur_graph(ArgsV,0,OG).
occur_graph([],_,[]).
occur_graph([L|A],I,OG) :- J is I+1, occur_graph(A,J,O1), ogl(L,I,O1,OG) .

ogl([],_,O,O).
ogl([V|R],I,In,Out) :- ogl(R,I,In,O1), insert_vertice(O1,V-[I],Out).

%%-----------------------------------------------------------------------------

numbervars(X):-my_number_vars(X,0,_).

my_number_vars(T, N0, N) :- 
    var(T),!, 
    T='$VAR'(N0),
    N is N0+1.
my_number_vars(T, N0, N) :- 
    atomic(T),!,
     N0=N.
my_number_vars([X|Xs], N0, N) :- !,
    my_number_vars(X, N0, N1),
    my_number_vars(Xs, N1, N).
my_number_vars(X, N0, N) :-
    functor(X, _, A),
    my_number_vars(0, A, X, N0, N).

my_number_vars(A, A, _, N0, N) :- !,
    N0=N.
my_number_vars(A0, A, X, N0, N) :-
    A1 is A0+1,
    arg(A1, X, X1),
    my_number_vars(X1, N0, N1),
    my_number_vars(A1, A, X, N1, N).


%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  The only reason for giving it
%   a name at all is that you may not have realised that sort/2 could be
%   used this way.

list_to_ord_set(List, Set) :-
	sort(List, Set).


%   merge(+List1, +List2, -Merged)
%   is true when Merged is the stable merge of the two given lists.
%   If the two lists are not ordered, the merge doesn't mean a great
%   deal.  Merging is perfectly well defined when the inputs contain
%   duplicates, and all copies of an element are preserved in the
%   output, e.g. merge("122357", "34568", "12233455678").  Study this
%   routine carefully, as it is the basis for all the rest.

merge([Head1|Tail1], [Head2|Tail2], [Head2|Merged]) :-
	Head1 @> Head2, !,
	merge([Head1|Tail1], Tail2, Merged).
merge([Head1|Tail1], List2, [Head1|Merged]) :-
	List2 \== [], !,
	merge(Tail1, List2, Merged).
merge([], List2, List2) :- !.
merge(List1, [], List1).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  If the
%   arguments are not ordered, I have no idea what happens.

ord_disjoint([], _) :- !.
ord_disjoint(_, []) :- !.
ord_disjoint([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint(Order, Head1, Tail1, Head2, Tail2).

ord_disjoint(<, _, Tail1, Head2, Tail2) :-
	ord_disjoint(Tail1, [Head2|Tail2]).
ord_disjoint(>, Head1, Tail1, _, Tail2) :-
	ord_disjoint([Head1|Tail1], Tail2).



%   ord_insert(+Set1, +Element, ?Set2)
%   is the equivalent of add_element for ordered sets.  It should give
%   exactly the same result as merge(Set1, [Element], Set2), but a bit
%   faster, and certainly more clearly.

ord_insert([], Element, [Element]).
ord_insert([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_insert(Order, Head, Tail, Element, Set).


ord_insert(<, Head, Tail, Element, [Head|Set]) :-
	ord_insert(Tail, Element, Set).
ord_insert(=, Head, Tail, _, [Head|Tail]).
ord_insert(>, Head, Tail, Element, [Element,Head|Tail]).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(=, _, _, _, _).
ord_intersect(<, _, Tail1, Head2, Tail2) :-
	ord_intersect(Tail1, [Head2|Tail2]).
ord_intersect(>, Head1, Tail1, _, Tail2) :-
	ord_intersect([Head1|Tail1], Tail2).



%   ord_intersect(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersect(_, [], []) :- !.
ord_intersect([], _, []) :- !.
ord_intersect([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersect(=, Head,  Tail1, _,     Tail2, [Head|Intersection]) :-
	ord_intersect(Tail1, Tail2, Intersection).
ord_intersect(<, _,     Tail1, Head2, Tail2, Intersection) :-
	ord_intersect(Tail1, [Head2|Tail2], Intersection).
ord_intersect(>, Head1, Tail1, _,     Tail2, Intersection) :-
	ord_intersect([Head1|Tail1], Tail2, Intersection).



%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.


ord_seteq(Set1, Set2) :-
	Set1 == Set2.


%   ord_setproduct(+Set1, +Set2, ?Product)
%   is in fact identical to setproduct(Set1, Set2, Product).
%   If Set1 and Set2 are ordered sets, Product will be an ordered
%   set of x1-x2 pairs.  Note that we cannot solve for Set1 and
%   Set2, because there are infinitely many solutions when
%   Product is empty, and may be a large number in other cases.

ord_setproduct([], _, []).
ord_setproduct([H|T], L, Product) :-
	ord_setproduct(L, H, Product, Rest),
	ord_setproduct(T, L, Rest).

ord_setproduct([], _, L, L).
ord_setproduct([H|T], X, [X-H|TX], TL) :-
	ord_setproduct(T, X, TX, TL).


%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

ord_subset([], _) :- !.
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Head2, Tail2).

ord_subset(=, _, Tail1, _, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, _, Tail2) :-
	ord_subset([Head1|Tail1], Tail2).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2.


ord_subtract([], _, []).
ord_subtract([Head1|Tail1], Set2, Difference) :-
	ord_subtract(Set2, Head1, Tail1, Difference).

ord_subtract([], Head1, Tail1, [Head1|Tail1]).
ord_subtract([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
        'ord subtract'(Tail1, Head2, Tail2, Difference).
ord_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
        ord_subtract(Tail2, Head1, Tail1, Difference).
ord_subtract(=, _,     Tail1, _,     Tail2, Difference) :-
        ord_subtract(Tail1, Tail2, Difference).

'ord subtract'([], _, _, []).
'ord subtract'([Head1|Tail1], Head2, Tail2, Difference) :-
        compare(Order, Head1, Head2),
        ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).


%   ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

ord_symdiff(Set1, [], Set1) :- !.
ord_symdiff([], Set2, Set2) :- !.
ord_symdiff([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_symdiff(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_symdiff(Tail1, Tail2, Difference).
ord_symdiff(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_symdiff(Tail1, [Head2|Tail2], Difference).
ord_symdiff(>, Head1, Tail1, Head2, Tail2, [Head2|Difference]) :-
	ord_symdiff([Head1|Tail1], Tail2, Difference).



%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union([], Set2, Set2).
ord_union([Head1|Tail1], Set2, Union) :-
	ord_union(Set2, Head1, Tail1, Union).

ord_union([], Head1, Tail1, [Head1|Tail1]).
ord_union([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union(=, Head,  Tail1, _,     Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_union(Tail1, [Head2|Tail2], Union).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_union([Head1|Tail1], Tail2, Union).


%   ord_union(+ListOfSets, ?Union)
%   is true when ListOfSets is given as a proper list of ordered sets
%   and Union is their union.  Letting K be the length of ListOfSets,
%   and N the sum of the sizes of its elements, the cost is of order
%   N.lg(K).  The auxiliary routine
%   ord_union_3(N, L, U, R)
%   is true when the union of the first N sets in L is U and
%   R is the remaining elements of L.

ord_union(ListOfSets, Union) :-
	length(ListOfSets, NumberOfSets),
	ord_union_3(NumberOfSets, ListOfSets, Union, []).

ord_union_3(0, R, [], R) :- !.
ord_union_3(1, [U|R], U, R) :- !.
ord_union_3(2, [A,B|R], U, R) :- !,
	ord_union(A, B, U).
ord_union_3(N, R0, U, R) :-
	P is N>>1,      % |first  half of list|
	Q is N- P,      % |second half of list|
	ord_union_3(P, R0, A, R1),
	ord_union_3(Q, R1, B, R),
	ord_union(A, B, U).


%   ord_del_element(+Set1, +Element, ?Set2)
%   is the equivalent of del_element for ordered sets.  Because it uses
%   ordering, it typically builds less structure, but is slower than
%   del_element.  I am beginning to wonder whether a predicate
%       set_plus(SmallSet, Element, LargeSet)
%   would be a better way of doing this, the idea being that
%   LargeSet = SmallSet U {Element} and Element is not in SmallSet.
%   There is already a predicate with this effect called select/3.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Element, Head, Tail, Set).

ord_del_element(<, Element, Head, Tail, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, _, Set, Set).
ord_del_element(>, _, Head, Tail, [Head|Tail]).


my_ground(X):-var(X),!,fail.
my_ground(X):-atomic(X),!,true.
my_ground([X|Xs]):-!,my_ground(X),my_ground(Xs).
my_ground(X):-functor(X,_,N),my_ground(X,N).

my_ground(_,N):-N=:=0,!,true.
my_ground(X,N):-N>0,arg(N,X,A),my_ground(A),N1 is N-1,my_ground(X,N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   subsumes_chk(General, Specific)
%   is true when Specific is an instance of General. However, this
%   predicate performs the test without binding any variables neither
%   in General nor in Specific.

subsumes_chk(General, Specific) :-
    \+((numbervars(Specific),
       \+ (General = Specific)
       )).

%   subsumes(General, Specific)
%   is true when Specific is an instance of General. Unlike subsumes_chk/2
%   this predicate will bind variables in General (but not those in Specific)
%   so that General becomes identical to Specific.

subsumes(General, Specific) :-
    subsumes_chk(General, Specific),
    General = Specific.

/*** SB-Prolog one
subsumes(X,Y) :- \+ (X=Y),!,fail.
subsumes(X,Y) :- numbervars(Y,0,_),\+ (X=Y),!,fail.
subsumes(_,_).
***/

%   my_variant(Term1, Term2)
%   is true when Term1 and Term2 are alphabetic variants. The definition 
%   here is not quite right;  if Term1 and Term2 share any variables it 
%   may give the wrong answer. It is only meant to be used when the two 
%   terms have no variables in common. 

/*** A naive first version */
my_variant(Term1, Term2) :-
    subsumes_chk(Term1, Term2),
    subsumes_chk(Term2, Term1).

result([pdsbm([num],[list]), pds([num],[list]),
	pds1([list],[A],[list],[B]), pds1([list],[list],[list],[A]),
	pds1([list],[list],[list],[num]),
	check([list],[A],[B],[B],[C]),
	check([list],[A],[B,list],[B],[C]),
	check([list],[A],[B,list],[B,list],[C]),
	check([list],[A],[list],[list],[B]),
	check([list],[num],[A],[A],[num]),
	check([list],[num],[A,list],[A],[num]),
	check([list],[num],[A,list],[A,list],[num]),
	check([list],[num],[list],[list],[num]),
	mergedelete([list],[A],[A]), mergedelete([list],[A,list],[A]),
	mergedelete([list],[A,list],[A,list]), rev([list],[A],[A]),
	rev([list],[A],[A,list]), dif([list],[A],[B],[list],[list]),
	dif([list],[num],[num],[list],[list]), iota1([A],[A],[list]),
	iota1([num],[num],[list]), iota([num],[list]),
	member([A],[B,list])]).
