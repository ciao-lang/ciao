%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ml.pl -- main file fot the WITT clustering system. To start, simply
%% load this file and call witt/0. It will take some seconds to complete.
%% Universe to be clustered in examples.pl.
%% AFSID           : $__Header$
%% Author          : Manuel Carro Li~nares
%% Created On      : At some point in the year 92/93
%% Last Modified By: MCL
%% Last Modified On: Wed Feb 26 18:32:32 2003
%% Update Count    : 89
%% Status          : Correct and working


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% READ THIS
%% This file implements a conceptual clustering algorithm (it was one
%% of my assignements...).
%% References: Stephen Jos\'{e} Hanson, Malcolm Bauer.
%%             Conceptual Clustering, Categorization, and Polymorphy.
%%             Machine Learning 3:343-372, 1989.
%%
%% The universe to be clustered is to be specified through the
%% predicates example/2 and attribute/2. There are also threshold
%% parameters which can be different from one domain to other.
%% NOTE: this program uses float point numbers (I was to lazy to
%% implement metrics with integers...) and needs of the natural
%% algorithm function. If your system complains, look for the
%% definition of the predicate mylog/2 and adapt it to your system.
%% As a hint (sob...) a polynomial approximation does not work very
%% well, because natural logarithms are needed for numbers not really
%% near to zero, so you'll need to work out several approximating
%% polynomials. 
%%
%% The entry point is witt/0. As it is, it takes about 6.5 seconds in a Sun
%% Sparc II with SICStus 2.1 #5, compiled with cc. The prolog code
%% was compiled with compactcode option.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This what witt should print. Check if this is correct, because the
%% program behavior and results can be affected by the precission of
%% the nat_log/2 predicate. Different levels correspond to successive
%% clustering step, until a fix point is reached. They can be seen as
%% a taxonomy (draw them as a tree, being each level the set of nodes
%% in that level of the tree). The ordering inside each level is not
%% important. 
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% level(1) [nicomaco], [godel], [lolita], [neuromante], [fundacion],
%% [maldoror], [flores_mal], [dragon], [jazmin], [perfume], [quijote],
%% [biblia], [sodoma], [jazaro]
%% 
%% level(2) [nicomaco], [godel], [dragon], [jazmin], [perfume],
%% [quijote], [biblia], [sodoma], [jazaro], [neuromante,fundacion],
%% [lolita,maldoror,flores_mal]
%% 
%% level(3) [neuromante,fundacion], [lolita,maldoror,flores_mal],
%% [godel,dragon], [nicomaco], [jazmin], [perfume], [quijote], [biblia],
%% [sodoma], [jazaro]
%% 
%% level(4) [neuromante,fundacion], [godel,dragon],
%% [sodoma,lolita,maldoror,flores_mal], [nicomaco], [jazmin], [perfume],
%% [quijote], [biblia], [jazaro]
%% 
%% level(5) [neuromante,fundacion], [godel,dragon],
%% [sodoma,lolita,maldoror,flores_mal], [perfume,jazaro], [nicomaco],
%% [jazmin], [quijote], [biblia]
%% 
%% level(6) [neuromante,fundacion], [godel,dragon],
%% [sodoma,lolita,maldoror,flores_mal], [perfume,jazaro],
%% [nicomaco,biblia,quijote], [jazmin]

:- module(witt, [main/1, do_witt/0], [profiler, expander]).
%:- module(witt, [main/1,do_witt/0], []).
:- use_module(library(prolog_sys)).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(sort)).

:- cost_center main/1, do_witt/0, dij/5.

main(Args) :-
	my_own_time(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(Tx),
	my_own_time(T),
	(
	    member('--write', Args) ->
	    write_taxo(Tx, 1),
	    nl
	;
	    true
	),
	(
	    member('--stats', Args) ->
	    statistics,
	    nl
	;
	    true
	),
	write('Executed in '),
	write(T),
	write(' ms.'),
	nl.

my_own_time(T) :- statistics(runtime, [_, T]).


twitt(T) :-
	statistics(runtime, _),
	witt_nw(_),
	statistics(runtime, T).

do_witt :-
	my_own_time(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_),
	witt_nw(_Tx),
	my_own_time(T),
	display('Witt: '),
	display(T),
	display(' milliseconds'),
	nl.


witt_nw(Taxo) :-
	Taxo = [Initial, First_Partition|Classes],
	universe(World),
	my_own_select_names(World, Initial),
	precluster(World, First_step),
	my_own_select_names(First_step, First_Partition),
	split(First_step, Instances, Categories),
	refinement(Instances, Categories, Classes).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This definition can be changed to work in particular systems.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nat_log(X, -12) :- X =< 0.00001, !. % This may happen in the program
nat_log(X, Y) :- mylog(X, Y). % Adapt mylog for your system.

%% mylog(X,Y):- Y is log(X).           % SICStus 2.1
%% mylog(X,Y):- Y is ln(X).           % ECLiPSe

mylog(Number, Log) :-
	N is Number - 1,
	N2 is N * N,
	N3 is N2 * N,
	N4 is N3 * N,
	N5 is N4 * N,
	Log is N - N2/2 + N3/3 - N4/4 + N5/5.




%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some stuff for tracing in &-Prolog.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% witt_trc:-
%% 	start_event_trace,
%% 	Taxo = [Initial, First_Partition|Classes],
%% 	universe(World),
%% 	my_own_select_names(World, Initial),
%% 	precluster(World, First_step),
%% 	my_own_select_names(First_step, First_Partition),
%% 	split(First_step, Instances, Categories),
%% 	refinement(Instances, Categories, Classes),
%% 	end_event_trace,
%% 	write_taxo(Taxo, 1),
%% 	save_trace('EventFile_witt').
%% 
%% save_trace(X) :-





%% 	write('Saving trace in file '), write(X), write('... '), 
%% 	open(X,write,Y),
%% 	save_event_trace(Y),
%% 	close(X),
%% 	write('done.'), nl.
%% 	


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The actual algorithm follows. I'm not going to explain it
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

precluster(World, NewWorld) :-
	smallest_dist(World, O1, O2, D),
	factor(F),
	T1 is F*D,
	compare(C, D, T1),
	precluster(C, World, O1, O2, T1, NewWorld).

:- push_prolog_flag(multi_arity_warnings, off).
precluster(>, W, _,  _,  _,  W).
precluster(=, W, _,  _,  _,  W).
precluster(<, W, O1, O2, T1, Nw) :-
	my_own_select_(O1, W,  W1),
	my_own_select_(O2, W1, W2),
	precluster(W2, O1, O2, T1, Nw).
precluster([],     O1, O2, _,  [O1, O2]).
precluster([W|Ws], O1, O2, T1, Nw) :-
	combine(O1, O2, Category),
	UpW = [Category, W|Ws],
	smallest_dist(UpW, Ob1, Ob2, D),
	compare(C, D, T1),
	precluster(C, UpW, Ob1, Ob2, T1, Nw).

smallest_dist(World, O1, O2, Best) :-
	World = [Ob1, Ob2|_],
	distance(Ob1, Ob2, D),
	smallest_dist(World, Ob1, Ob2, D, O1, O2, Best).

smallest_dist([],     O1,  O2,  D, O1, O2, D).
smallest_dist([W|Ws], Ob1, Ob2, D, O1, O2, Best) :-
	smallest_dist(Ws, W, Ob1, Ob2, D, Oi1, Oi2, Di),
	smallest_dist(Ws, Oi1, Oi2, Di, O1, O2, Best).
smallest_dist([],       _,      O1,  O2,  Best, O1, O2, Best).
smallest_dist([Ob|Obs], Object, Ob1, Ob2, D,    O1, O2, Best) :-
	distance(Ob, Object, Di),
	compare(C, D, Di),
	my_own_select_tri(C, Ob, Object, Di, Ob1, Ob2, D, NOb1, NOb2, Nd),
	smallest_dist(Obs, Object, NOb1, NOb2, Nd, O1, O2, Best).

refinement(Ins, Cats, Classes) :-
	threshold2(T2),
	threshold3(T3),
	refinement(Ins, Cats, T2, T3, Classes).

refinement([], Cats, T2, T3, Classes) :-
	compute_within(Cats, [], T2, T3, Classes).
refinement([I|Is], Cats, T2, T3, Classes) :-
	classify_pairs([I|Is], Cats, T2, T3, Classes).
:- pop_prolog_flag(multi_arity_warnings).

classify_pairs(Ins, Cats, T2, T3, Classes) :-
	get_best_pair(Ins, Cats, In, Cat, Score),
	compare(C, Score, T2),
	check_add(C, Ins, Cats, In, Cat, T2, T3, Classes).

check_add(>, Ins, Cats, I, Cat, T2, T3, [Partition|Classes]) :-
	NewWorld = [NewCat|NewCats],
	combine(I, Cat, NewCat),
	my_own_select_(I,   Ins,  NewIns),
	my_own_select_(Cat, Cats, NewCats),
	my_own_select_names(pair(NewIns, NewWorld), Partition),
	refinement(NewIns, NewWorld, T2, T3, Classes).
check_add(=, Ins, Cats, _, _, T2, T3, Classes) :-
	new_categories(Ins, Cats, T2, T3, Classes).
check_add(<, Ins, Cats, _, _, T2, T3, Classes) :-
	new_categories(Ins, Cats, T2, T3, Classes).


new_categories([], Cats, T2, T3, Classes) :- %% No examples
	compute_within(Cats, [], T2, T3, Classes).
new_categories([I|Ins], Cats, T2, T3, Classes) :- %% Extract one
	new_categories(Ins, I, Cats, T2, T3, Classes).

:- push_prolog_flag(multi_arity_warnings, off).
new_categories([], I, Cats, T2, T3, Classes) :- %% Only one example
	compute_within(Cats, [I], T2, T3, Classes).
new_categories([I1|Is], I, Cats, T2, T3, Classes) :- %% Two or more
	Ins = [I, I1|Is],
	precluster(Ins, NewIns),
	split(NewIns, _, NewCats),
	find_addable(NewCats, Cats, T3, Addable),
	extract_instances(Ins, Addable, TrueIns),
	add_cats(Addable, TrueIns, Cats, T2, T3, Classes).
:- pop_prolog_flag(multi_arity_warnings).

add_cats([], Ins, Cats, T2, T3, Classes) :-
	compute_within(Ins, Cats, T2, T3, Classes).
add_cats([C|Cs], Ins, Cats, T2, T3, [Partition|Classes]) :-
	my_own_append(Cs,  [C|Cats], NewCats),
	my_own_append(Ins, NewCats,  Part),
	my_own_select_names(Part, Partition),
	refinement(Ins, NewCats, T2, T3, Classes).

compute_within([C|Cs], Ins, T2, T3, Classes) :-
	compute_within(Cs, C, Ins, T2, T3, Classes).

:- push_prolog_flag(multi_arity_warnings, off).

compute_within([], C, Ins, _, _, [Classes]) :- %% Only one cathegory
	my_own_select_names([C|Ins], Classes).
compute_within([Ca1|Cats], Ca2, Ins, T2, T3, Classes) :-
	best_cohesion(Ca1, Ca2, Cats, C1, C2, Score),
	compare(C, Score, T3),
	check_end(C, Ins, [Ca1, Ca2|Cats], C1, C2, T2, T3, Classes).
:- pop_prolog_flag(multi_arity_warnings).

check_end(>, Ins, Cats, C1, C2, T2, T3, [Partition|Classes]) :-
	NewWorld = [C3|Cats2],
	combine(C1, C2, C3),
	my_own_select_(C1, Cats,  Cats1),
	my_own_select_(C2, Cats1, Cats2),
	my_own_select_names(pair(Ins, NewWorld), Partition),
	refinement(Ins, NewWorld, T2, T3, Classes).
check_end(<, Ins, Cats, _, _, _, _, [Classes]) :-
	my_own_select_names(pair(Ins, Cats), Classes).
check_end(=, Ins, Cats, _, _, _, _, [Classes]) :-
	my_own_select_names(pair(Ins, Cats), Classes).



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The universe to be clustered.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attribute(epoca, [antigua, media, actual, futura, nula]).
attribute(tema, [ciencia, vida, aventuras, amor, guerra, poesia, religion,
		muerte, ficcion, sexo]).
attribute(autor,   [anonimo, antiguo, moderno, medio]).
attribute(publico, [joven,   adulto,  culto,   no_culto]).

example(jazaro,     [epoca -[antigua], tema -[ficcion],
		autor -[moderno], publico -[culto]]).
example(sodoma,     [epoca -[actual], tema -[sexo],
		autor -[moderno], publico -[adulto]]).
example(biblia,     [epoca -[antigua], tema -[religion],
		autor -[anonimo, antiguo], publico -[joven, adulto]]).
example(quijote,    [epoca -[media], tema -[aventuras, vida, ficcion],
		autor -[medio], publico -[joven, adulto]]).
example(perfume,    [epoca -[actual], tema -[ficcion],
		autor -[moderno], publico -[adulto, culto]]).
example(jazmin,     [epoca -[actual, media], tema -[amor],
		autor -[moderno], publico -[no_culto]]).
example(dragon,     [epoca -[nula], tema -[ciencia],
		autor -[moderno], publico -[culto]]).
example(flores_mal, [epoca -[actual], tema -[amor, muerte, vida],
		autor -[moderno], publico -[adulto, culto]]).
example(maldoror,   [epoca -[actual], tema -[vida, muerte],
		autor -[moderno], publico -[adulto, culto]]).
example(fundacion,  [epoca -[futura], tema -[ciencia, ficcion],
		autor -[moderno], publico -[joven, adulto]]).
example(neuromante, [epoca -[futura], tema -[ciencia, ficcion],
		autor -[moderno], publico -[joven, adulto]]).
example(lolita,     [epoca -[actual], tema -[sexo, amor],
		autor -[moderno], publico -[adulto, culto]]).
example(godel,      [epoca -[nula], tema -[ciencia],
		autor -[moderno], publico -[joven, culto]]).
example(nicomaco,   [epoca -[nula], tema -[ciencia],
		autor -[antiguo], publico -[adulto, culto]]).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Clustering parameters. They change the behavior of the algorithm,
%% and are domain-dependent. This is the bad point of all the work...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


factor(1.2).
threshold2(0.05).
threshold3(1.0).




dij(M, Dij) :-
	dij(M, 0, Num, 0, D),
	nat_log(D, L),
	Den is L*D,
	divide(Num, Den, Dij).

divide(X, X, 1).
divide(X, Y, R) :-
	X =\= Y,
	R is X / Y.

:- push_prolog_flag(multi_arity_warnings, off).
dij([],     X,  X,  Y,  Y) :- !. %% Green
dij([M|Ms], Xi, Xo, Yi, Yo) :- !, %% Green
	dij(M,  Xi, Xm, Yi, Ym),
	dij(Ms, Xm, Xo, Ym, Yo).
dij(A, Xi, Xo, Yi, Yo) :-
	number(A), !, %% Green
	Yo is Yi + A,
	nat_log(A, L),
	Xo is Xi + A * L.
:- pop_prolog_flag(multi_arity_warnings).

wc(ml(_, _, Z, Conts), WC) :-
	wc(Conts, 0, Out),
	WC is Out / Z.
:- push_prolog_flag(multi_arity_warnings, off).
wc([],                   A, A).
wc([mat(D, _, _, _)|Ms], I, O) :-
	I1 is I + D,
	wc(Ms, I1, O).
:- pop_prolog_flag(multi_arity_warnings).

distance(ml(_, Ats1, _, _), ml(_, Ats2, _, _), D) :-
	distance(Ats1, Ats2, 1, D1),
	D is 1 / D1.

:- push_prolog_flag(multi_arity_warnings, off).
distance([],          [],          D,  D).
distance([A-V1s|A1s], [A-V2s|A2s], In, Out) :-
	my_own_ordintersection(V1s, V2s, Vs),
	length(Vs, L),
	Mid is In + L,
	distance(A1s, A2s, Mid, Out).
:- pop_prolog_flag(multi_arity_warnings).

bck(C1, C2, B) :-
	combine(C1, C2, C12),
	wc(C1,  W1),
	wc(C2,  W2),
	wc(C12, W12),
	B is 1 / (W1 + W2 - 2*W12).


oc([], _, N, In, Out) :-
	Out is In / N.
oc([C|Cs], Cat, N, In, Out) :-
	N1 is N + 1,
	bck(C, Cat, Bck),
	Mid is In + Bck,
	oc(Cs, Cat, N1, Mid, Out).


cc(Cat, Cats, C) :-
	wc(Cat, Wc),
	oc(Cats, Cat, 0, 0, Oc),
	C is Wc / Oc.




write_taxo([],  _).
write_taxo([T], N) :- !,
	write(level(N)), nl,
	write_clas(T).
write_taxo([T, T1|Ts], N) :-
	write(level(N)), nl,
	write_clas(T),
	N1 is N + 1,
	write_taxo([T1|Ts], N1).
write_clas([T]) :- write(T), nl, nl.
write_clas([T, T1|Ts]) :-
	write(T),
	write(', '),
	write_clas([T1|Ts]).



my_own_select_names(O, N) :-
	my_own_select_names(O, [], N).

:- push_prolog_flag(multi_arity_warnings, off).
my_own_select_names(ml(N, _, _, _), I, [N|I]).
my_own_select_names(pair(O1, O2),   I, N) :-
	my_own_select_names(O1, I, M),
	my_own_select_names(O2, M, N).
my_own_select_names([],                  N, N).
my_own_select_names([ml(N, _, _, _)|Os], A, Ns) :-
	my_own_select_names(Os, [N|A], Ns).
:- pop_prolog_flag(multi_arity_warnings).

combine(ml(Nm1, Cars1, Z, M1), ml(Nm2, Cars2, Z, M2), ml(Nm3, Cars3, Z, M3)) :-
	my_own_append(Nm1, Nm2, Nm3),
	combine_cars(Cars1, Cars2, Cars3),
	combine_conts(M1, M2, M3).

combine_cars([],          [],          []).
combine_cars([A-V1s|C1s], [A-V2s|C2s], [A-V3s|C3s]) :-
	my_own_ordunion(V1s, V2s, V3s),
	combine_cars(C1s, C2s, C3s).

combine_conts([], [], []).
combine_conts([mat(_, Ai, Aj, M1)|M1s], [mat(_, Ai, Aj, M2)|M2s],
	    [mat(D, Ai, Aj, M3)|M3s]) :-
	add_matrices(M1, M2, M3),
	dij(M3, D),
	combine_conts(M1s, M2s, M3s).

add_matrices([],       [],       []).
add_matrices([M1|M1s], [M2|M2s], [M3|M3s]) :-
	add_matrices(M1,  M2,  M3),
	add_matrices(M1s, M2s, M3s).
add_matrices(E1, E2, E3) :-
	number(E1),
	E3 is E1 + E2.

extract_instances(Ins, Addable, True) :-
	my_own_select_names(Addable, AdNames),
	plain(AdNames, [], Plain),
	extract_instances_1(Ins, Plain, True).
extract_instances_1([],      _,     []) :- !.
extract_instances_1([I|Ins], NCats, TIns) :-
	I = ml([N], _, _, _),
	my_own_member(N, NCats), !,
	extract_instances_1(Ins, NCats, TIns).
extract_instances_1([I|Ins], NCats, [I|TIns]) :-
	I = ml([N], _, _, _),
	non_my_own_member(N, NCats), !,
	extract_instances_1(Ins, NCats, TIns).

plain([],     P, P) :- !.
plain([A|As], I, O) :- !,
	plain(A,  I, M),
	plain(As, M, O).
plain(A, I, [A|I]) :-
	atomic(A), !.

split([],     [],     []).
split([X|Xs], [X|Ys], Zs) :-
	instance(X),
	split(Xs, Ys, Zs).
split([X|Xs], Ys, [X|Zs]) :-
	category(X),
	split(Xs, Ys, Zs).

instance(ml([_], _, _, _)).
category(ml([_, _|_], _, _, _)).

get_best_pair([In|Ins], [Cat|Cts], BestI, BestC, BestSc) :-
	combine(In, Cat, Prv),
	cc(Prv, Cts, Sco),
	get_best_pair([Cat|Cts], [In|Ins], [], Cat, In, Sco, BestC, BestI,
	    BestSc).

:- push_prolog_flag(multi_arity_warnings, off).
get_best_pair([],     _,   _,    C,     I,     S,     C,    I,    S).
get_best_pair([C|Cs], Ins, PCts, PrCat, PrIns, PrSco, BCat, BIns, BSco) :-
	my_own_append(Cs, PCts, NCts),
	get_best_pair_ins(Ins, C, NCts, PrCat, PrIns, PrSco, MdCat, MdIns,
	    MdSco),
	get_best_pair(Cs, Ins, [C|PCts], MdCat, MdIns, MdSco, BCat, BIns,
	    BSco).
:- pop_prolog_flag(multi_arity_warnings).

get_best_pair_ins([], _, _, C, I, S, C, I, S).
get_best_pair_ins([In|Ins], CCat, Cts, PrCat, PrIns, PrSco, BCat, BIns,
	    BSco) :-
	combine(In, CCat, NCat),
	cc(NCat, Cts, Sco),
	compare(C, Sco, PrSco),
	my_own_select_tri(C, CCat, In, Sco, PrCat, PrIns, PrSco, MC, MI, MS),
	get_best_pair_ins(Ins, CCat, Cts, MC, MI, MS, BCat, BIns, BSco).


find_addable(NewCats, Cats, T3, Addable) :-
	find_addable(NewCats, Cats, T3, [], Addable).

:- push_prolog_flag(multi_arity_warnings, off).
find_addable([],       _,    _,  A,   A).
find_addable([Nc|Ncs], Cats, T3, InA, OutA) :-
	is_addable(Cats, Nc, T3, Verdict),
	verdict(Verdict, Nc, InA, MidA),
	find_addable(Ncs, Cats, T3, MidA, OutA).
:- pop_prolog_flag(multi_arity_warnings).

verdict(yes, Nc, InA, [Nc|InA]).
verdict(no,  _,  InA, InA).

is_addable([],      _,  _,  yes).
is_addable([C1|Cs], Nc, T3, V) :-
	combine(C1, Nc, C2),
	wc(C2, W),
	compare(C, W, T3),
	is_addable(C, Cs, Nc, T3, V).

:- push_prolog_flag(multi_arity_warnings, off).
is_addable(<, Cats, Cat, T3, V) :-
	is_addable(Cats, Cat, T3, V).
is_addable(>, _, _, _, no).
is_addable(=, _, _, _, no).
:- pop_prolog_flag(multi_arity_warnings).

best_cohesion(Cat1, Cat2, Cats, C1, C2, Score) :-
	combine(Cat1, Cat2, Cat3),
	wc(Cat3, W3),
	best_cohesion([Cat2|Cats], Cat1, Cat1, Cat2, W3, C1, C2, Score).

:- push_prolog_flag(multi_arity_warnings, off).
best_cohesion([],         _, C1,    C2,    S,    C1,    C2,    S).
best_cohesion([Cat|Cats], C, PCat1, PCat2, PSco, BCat1, BCat2, BSco) :-
	combine(Cat, C, NCat),
	wc(NCat, NSco),
	compare(Comp, NSco, PSco),
	my_own_select_tri(Comp, Cat, C, NSco, PCat1, PCat2, PSco, MCat1, MCat2,
	    MSco),
	best_cohesion(Cats, C,   MCat1, MCat2, MSco, QCat1, QCat2, QSco),
	best_cohesion(Cats, Cat, QCat1, QCat2, QSco, BCat1, BCat2, BSco).
:- pop_prolog_flag(multi_arity_warnings).

my_own_select_tri(<, _, _, _, A, B, C, A, B, C).
my_own_select_tri(=, _, _, _, A, B, C, A, B, C).
my_own_select_tri(>, A, B, C, _, _, _, A, B, C).

universe(U) :-
	findall(A, attribute(A, _), Ats),
	length(Ats, N),
	Z is N*(N - 1) / 2,
	findall(Instance, instance(Instance, Ats, Z), U).

:- push_prolog_flag(multi_arity_warnings, off).
instance(ml([Name], OrdCars, Z, Mats), Ats, Z) :-
	example(Name, Cars),
	intra_order(Cars, IntraCars),
	list_to_my_own_ordset(IntraCars, OrdCars),
	findall(mat(D, Ai, Aj, M), cont_table(D, Ai, Aj, M, Ats, Cars), Mats).
:- pop_prolog_flag(multi_arity_warnings).

intra_order([],        []).
intra_order([A-Vs|Cs], [A-Ovs|Os]) :-
	list_to_my_own_ordset(Vs, Ovs),
	intra_order(Cs, Os).

cont_table(D, Ai, Aj, M, Ats, Cars) :-
	my_own_append(_, [Ai|Ats1], Ats), attribute(Ai, Vali),
	my_own_member(Aj,    Ats1), attribute(Aj, Valj),
	my_own_member(Ai-Vi, Cars),
	my_own_member(Aj-Vj, Cars),
	fill_matrix(Vali, Valj, Vi, Vj, M),
	dij(M, D).

fill_matrix([],           _,     _,     _,     []).
fill_matrix([Vali|Valis], Valjs, Presi, Presj, [V|Vs]) :-
	fill_vector(Valjs, Vali, Presi, Presj, V),
	fill_matrix(Valis, Valjs, Presi, Presj, Vs).

fill_vector([],           _,    _,     _,     []).
fill_vector([Valj|Valjs], Vali, Presi, Presj, [E|Es]) :-
	double_my_own_member_check(Vali, Valj, Presi, Presj, E),
	fill_vector(Valjs, Vali, Presi, Presj, Es).

double_my_own_member_check(E1, E2, L1, L2, 1) :-
	my_own_memberchk(E1, L1),
	my_own_memberchk(E2, L2), !.
double_my_own_member_check(_, _, _, _, 0).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Several public library predicates, from O'Keefe's shared code.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%   my_own_ordunion(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

my_own_ordunion([],            Set2,          Set2) :- !.
my_own_ordunion(Set1,          [],            Set1) :- !.
my_own_ordunion([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	my_own_ordunion(Order, Head1, Tail1, Head2, Tail2, Union).

:- push_prolog_flag(multi_arity_warnings, off).
my_own_ordunion(<, Head0, [], Head2, Tail2, [Head0, Head2|Tail2]) :-
	!.
my_own_ordunion(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	my_own_ordunion(Order, Head1, Tail1, Head2, Tail2, Union).
my_own_ordunion(=, Head, Tail1, _, Tail2, [Head|Union]) :-
	my_own_ordunion(Tail1, Tail2, Union).
my_own_ordunion(>, Head1, Tail1, Head0, [], [Head0, Head1|Tail1]) :-
	!.
my_own_ordunion(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	my_own_ordunion(Order, Head1, Tail1, Head2, Tail2, Union).
:- pop_prolog_flag(multi_arity_warnings).

%   my_own_ordintersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

my_own_ordintersection([],            _,             []) :- !.
my_own_ordintersection(_,             [],            []) :- !.
my_own_ordintersection([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	my_own_ordintersection(Order, Head1, Tail1, Head2, Tail2,
	    Intersection).

:- push_prolog_flag(multi_arity_warnings, off).
my_own_ordintersection(<, _, [],            _,     _,     []) :- !.
my_own_ordintersection(<, _, [Head1|Tail1], Head2, Tail2, Intersection) :-
	compare(Order, Head1, Head2),
	my_own_ordintersection(Order, Head1, Tail1, Head2, Tail2,
	    Intersection).
my_own_ordintersection(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	my_own_ordintersection(Tail1, Tail2, Intersection).
my_own_ordintersection(>, _,     _,     _, [],            []) :- !.
my_own_ordintersection(>, Head1, Tail1, _, [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	my_own_ordintersection(Order, Head1, Tail1, Head2, Tail2,
	    Intersection).
:- pop_prolog_flag(multi_arity_warnings).

%   list_to_my_own_ordset(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  

list_to_my_own_ordset(List, Set) :-
	sort(List, Set).


%:- use_module(library(lists)).



%   my_own_select_(?Element, ?List, ?List2)
%   is true when the result of removing an occurrence of Element in List
%   is List2.

my_own_select_(Element, [Element|Tail], Tail).
my_own_select_(Element, [Head|Tail1],   [Head|Tail2]) :-
	my_own_select_(Element, Tail1, Tail2).


%   my_own_member(?Element, +List)
%   is true when Element is a my_own_member of List.  It may be used to test 
%   for my_own_membership in a list, but it can also be used to enumerate all 
%   the elements in List.

my_own_member(Element, [Head|Tail]) :-
	my_own_member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
my_own_member_(_,           Element, Element).
my_own_member_([Head|Tail], _,       Element) :-
	my_own_member_(Tail, Head, Element).


%   my_own_memberchk(+Element, +List)
%   is true when Element is a my_own_member of List, but my_own_memberchk/2 only succeeds
%   once and can therefore not be used to enumerate the elements in List.

my_own_memberchk(Element, [Element|_]) :- !.
my_own_memberchk(Element, [_|Rest]) :-
	my_own_memberchk(Element, Rest).



%   my_own_append(?Prefix, ?Suffix, ?Combined)
%   is true when Combined is the combined list of the elements in Prefix 
%   followed by the elements in Suffix. It can be used to form Combined or
%   it can be used to find Prefix and/or Suffix from a given Combined  

my_own_append([],          List, List).
my_own_append([Head|Tail], List, [Head|Rest]) :-
	my_own_append(Tail, List, Rest).


%   non_my_own_member(+Element, +List)
%   non_my_own_member is true when Element does not exist in List.

non_my_own_member(Element, List) :-
	non_my_own_member_(List, Element).


non_my_own_member_([],          _).
non_my_own_member_([Head|Tail], Element) :-
%	dif(Head, Element),
	Head \== Element,
	non_my_own_member_(Tail, Element).


%% natural logarithm, which treats the special case of a zero as
%% argument. For our convenience, 0 will be returned.
