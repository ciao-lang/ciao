/*
PROLOG Digest            Friday, 29 Aug 1986       Volume 4 : Issue 45

Today's Topics:
           Performance - Benchmarking Systems (part 3 of 3)
----------------------------------------------------------------------

Date: Mon, 25 Aug 86 12:15:23 -0100
From: Jean Claude Syre <unido!ecrcvax!jclaude@seismo>
Subject: Benchmarking Prolog Systems (part 3 of 3)


                           A Proposal for
           ***********************************************
           *** BENCHMARK PROGRAMS FOR PROLOG SYSTEMS   ***
           ***            (FINAL VERSION)              ***
           ***********************************************
                           Part 3 (of 3)

2. Small Prolog programs.

Here we deal with prolog programs that do something, while
being still small but representative of some well-known Prolog
computations. This set should be augmented by other programs,
some of them might come from your ideas.

Some of the following programs were taken from the Berkeley
paper by Peter Van Roy "A Prolog Compiler for the PLM".  Other
programs were kindly donated by the following ECRC members:
Helmut Simonis, Mehmet Dincbas, Micha Meier and Pascal
Vanhentenryck.

The  programs  have  been  statically analysed and they
represent fairly standard programs as far as the statistical
averages are concerned. That is  the  arity  of  most  clauses
is 2 or 3 and there are usually 2 or 3 clauses per predicate.
The programs range from fairly  trivial  programs like fibonacci
series to problems such as Hamiltonian graph traversal.

Also, some more programs have been added since the last release
and some corrections have been made. Most of the writes were
removed in order to reduce i/o activity.

The programs added were symbolic differentiation (from Warrens
paper) and a quick sort algorithm using difference lists. The
last addition is a bit of a rogue: its a naive reverse, where
one can enter the list length. The list gets constructed and
then gets reversed.

We are grateful to Saumya Debray from Stony Brook and others
for comments, suggestions, feedback and useful inputs.

These benchmarks were run on a VAX 785 with 8 Meg of memory,
under 4.2 BSD Unix. The interpreter was C-Prolog version 1.5.

This entire file (without mail/net headers) contains 584 lines.

Name      |      Call by      |  # of Inferences  | KLips
          |                   |  (one iteration)  | (C-Prolog)
----------+-------------------+-------------------+-----------
fib       | fibonacci(1).     |        4932       |   2.0
----------+-------------------+-------------------+-----------
map       | map(200).         |          68       |   1.3
----------+-------------------+-------------------+-----------
mham      | mham(1).          |      493824       |   1.7
----------+-------------------+-------------------+-----------
mutest    | mutest(1).        |        1366       |   2.3
----------+-------------------+-------------------+-----------
quicksort | qs(10).           |         601       |   1.9
----------+-------------------+-------------------+-----------
queens    | qu(10).           |         684       |   1.7
----------+-------------------+-------------------+-----------
query     | query(1).         |        2294       |   0.9
----------+-------------------+-------------------+-----------
sym_diff  | differen(150).    |          71       |   1.5
----------+-------------------+-------------------+-----------
diff_lists| diff(50).         |         608       |   2.1
----------+-------------------+-------------------+-----------
nrev  10  | nrev.             |          66       |   2.0
----------+-------------------+-------------------+-----------
nrev  30  | nrev.             |         496       |   2.5
----------+-------------------+-------------------+-----------
nrev  50  | nrev.             |        1326       |   2.5
----------+-------------------+-------------------+-----------
nrev 100  | nrev.             |        5151       |   2.5
----------+-------------------+-------------------+-----------
nrev 150  | nrev.             |       11476       |   2.5
----------+-------------------+-------------------+-----------
nrev 200  | nrev.             |       20301       |   2.5
----------+-------------------+-------------------+-----------
-----------------------------CUT HERE------------------------- */
:- module(small_programs, [
        fibonacci/1,
	map/1,
	mham/1,
	mutest/1,
	qs/1,
	qu/1,
	query/1,
	differen/1,
	diff/1,
	nrev/1,
        qsort/3,
        list50/1], []).

:- use_module(benchmark_utilities).

% :- op(700,xfx,'<>').
% A <> B :- \+ A = B.

:- meta_predicate not(goal). % jfmc: added for ciao
not(X) :- \+ X.

/* Common functions...       */

el(X,[X|_L]).
el(X,[_Y|L]):- el(X,L).

list50([27,74,17,33,94,18,46,83,65,2,
        32,53,28,85,99,47,28,82,6,11,
        55,29,39,81,90,37,10,0,66,51,
        7,21,85,27,31,63,75,4,95,99,
        11,28,61,74,18,92,40,53,59,8]).

/* Fibonacci Series the slow way            */
/* fibonacci(1) will do...                  */

fibonacci(N) :- common_loop(3, N, top_fib(15, _), 4932, fibonacci).

top_fib(0,1).
top_fib(1,1).
top_fib(X,Y):- 
        X > 1,
        X1 is X-1,
        X2 is X-2,
        top_fib(X1,Y1),
        top_fib(X2,Y2),
        Y is Y1+Y2.

/* ------------------------------------ */
/* Map colouring problem                */
/*  map(200) is advised.                */

map(N) :-  common_loop(3, N, map_top, 68, map).

map_top:-
        el(_X1,[b]),
        el(X2,[r]),
        el(X7,[g]),
        el(X13,[w]),
        el(X3,[b,r,g,w]),
        not(X2=X3),
        not(X3=X13),
        el(X4,[b,r,g,w]),
        not(X2=X4),
        not(X7=X4),
        not(X3=X4),
        el(X5,[b,r,g,w]),
        not(X13=X5),
        not(X3=X5),
        not(X4=X5),
        el(X6,[b,r,g,w]),
        not(X13=X6),
        not(X5=X6),
        el(X8,[b,r,g,w]),
        not(X7=X8),
        not(X13=X8),
        el(X9,[b,r,g,w]),
        not(X13=X9),
        not(X4=X9),
        not(X8=X9),
        el(X10,[b,r,g,w]),
        not(X4=X10),
        not(X5=X10),
        not(X6=X10),
        not(X9=X10),
        el(X11,[b,r,g,w]),
        not(X11=X13),
        not(X11=X10),
        not(X11=X6),
        el(X12,[b,r,g,w]),
        not(X12=X13),
        not(X12=X11),
        not(X12=X9).


/* ---------------------------------------------- */
/*  Hamiltonian Graphs...                         */
/*  Extremely long (nearly half a million LI's !) */
/*  Only 1 advised !                              */

mham(N) :- common_loop(3, N, mham_top, 493824, mham).

mham_top:-
        cycle_ham([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t],_X), 
        fail.

cycle_ham([X|Y],[X,T|L]):-
        chain_ham([X|Y],[],[T|L]),
        edge(T,X).

chain_ham([X],L,[X|L]).
chain_ham([X|Y],K,L):-
        delete(Z,Y,T),
        edge(X,Z),
        chain_ham([Z|T],[X|K],L).

delete(X,[X|Y],Y).
delete(X,[U|Y],[U|Z]):-
        delete(X,Y,Z).

edge(X,Y):-
        connect(X,L),
        el(Y,L).

connect(0,[1,2,3,4,5,6,7,8,9]).
connect(1,[0,2,3,4,5,6,7,8,9]).
connect(2,[0,1,3,4,5,6,7,8,9]).
connect(3,[0,1,2,4,5,6,7,8,9]).
connect(4,[0,1,2,3,5,6,7,8,9]).
connect(5,[0,1,2,3,4,6,7,8,9]).
connect(6,[0,1,2,3,4,5,7,8,9]).
connect(7,[0,1,2,3,4,5,6,8,9]).
connect(8,[0,1,2,3,4,5,6,7,9]).
connect(9,[0,1,2,3,4,5,6,7,8]).

connect(a,[b,j,k]).
connect(b,[a,c,p]).
connect(c,[b,d,l]).
connect(d,[c,e,q]).
connect(e,[d,f,m]).
connect(f,[e,g,r]).
connect(g,[f,h,n]).
connect(h,[i,g,s]).
connect(i,[j,h,o]).
connect(j,[a,i,t]).
connect(k,[o,l,a]).
connect(l,[k,m,c]).
connect(m,[l,n,e]).
connect(n,[m,o,g]).
connect(o,[n,k,i]).
connect(p,[b,q,t]).
connect(q,[p,r,d]).
connect(r,[q,s,f]).
connect(s,[r,t,h]).
connect(t,[p,s,j]).

/* -------------------------------------------- */
/*  Hofstader's mu math (mutest) proving muiiu  */
/*  from Godel Escher Bach                      */
mutest(N) :- common_loop(3, N, mu_top, 1366, mutest).

mu_top:- theorem(5,[m,u,i,i,u]).

rules(S, R) :- rule3(S,R).
rules(S, R) :- rule4(S,R).
rules(S, R) :- rule1(S,R).
rules(S, R) :- rule2(S,R).

rule1(S,R) :-
        append(X, [i], S),
        append(X, [i,u], R).

rule2([m|T], [m|R]) :- append(T, T, R).

rule3([], -) :- fail.
rule3(R, T) :-
        append([i,i,i], S, R),
        append([u], S, T).
rule3([H|T], [H|R]) :- rule3(T, R).

rule4([], -) :- fail.
rule4(R, T) :- append([u, u], T, R).
rule4([H|T], [H|R]) :- rule4(T, R).

theorem(_Depth, [m, i]).
theorem(_Depth, []) :- fail.

theorem(Depth, R) :-
        Depth > 0,
        D is Depth - 1,
        theorem(D, S),
        rules(S, R).

append([], X, X).
append([A|B], X, [A|B1]) :-
        append(B, X, B1).
/* ------------------------------------  */
/*  Quicksort of 50 element list         */
/*                                       */

qs(N) :-  
        list50(L),
        common_loop(3, N, qsort(L, _, []), 601, qs).

qsort([X|L],R,R0) :-
        partition(L,X,L1,L2),
        qsort(L2,R1,R0),
        qsort(L1,R,[X|R1]).
qsort([],R,R).

partition([X|L],Y,[X|L1],L2) :- X =< Y,!,
        partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
        partition(L,Y,L1,L2).
partition([],_,[],[]).

/* ------------------------------------- */
/*  Queens on a chess board problem...   */
/*  Only twos solution on a 4x4 board... */
/*  about 5 - 10 is advised for N.       */

qu(N):- common_loop(3, N, qu_top, 684, qu).

qu_top :-  run(4,_X), fail.

size(4).
snint(1).
snint(2).
snint(3).
snint(4).

run(Size, Soln) :- get_solutions(Size, Soln).

get_solutions(Board_size, Soln) :- solve(Board_size, [], Soln).

/*  newsquare generates legal positions for next queen     */

newsquare([], square(1, X)) :- snint(X).
newsquare([square(I, J)|Rest], square(X, Y)) :-
        X is I + 1,
        snint(Y),
        not(threatened(I, J, X, Y)),
        safe(X, Y, Rest).

/*   safe checks whether square(X, Y) is threatened by any */
/*   existing queens                                       */

safe(_X, _Y, []).
safe(X, Y, [square(I, J)|L]) :-
        not(threatened(I, J, X, Y)),
        safe(X, Y, L).

/*    threatened checks whether squares (I, J) and (X, Y) */
/*    threaten each other                                 */

threatened(I, _J, X, _Y) :- 
        I = X, !.
threatened(_I, J, _X, Y) :-
        J = Y, !.
threatened(I, J, X, Y) :-
        U is I - J,
        V is X - Y,
        U = V, !.
threatened(I, J, X, Y) :-
        U is I + J,
        V is X + Y,
        U = V, !.

/* solve accumulates the positions of occupied squares */

solve(Bs, [square(Bs, Y)|L], [square(Bs, Y)|L]):- size(Bs).

solve(Board_size, Initial, Final) :-
        newsquare(Initial, Next),
        solve(Board_size, [Next|Initial], Final).

/* ------------------------------------ */
/* Query does simple database queries.  */
/*                                      */

query(N) :- common_loop(3, N, que_top, 2294, query).

que_top:-
        que(_X),
        fail.

que([C1,D1,C2,D2]) :-
        density(C1,D1),
        density(C2,D2),
        D1>D2,
        20*D1<21*D2.

density(C,D) :-
        pop(C,P),
        area(C,A),
        D is (P*100)/A.

pop(china,8250).
pop(india,5863).
pop(ussr,2521).
pop(usa,2119).
pop(indonesia,1276).
pop(japan,1097).
pop(brazil,1042).
pop(bangladesh,750).
pop(pakistan,682).
pop(w_germany,620).
pop(nigeria,613).
pop(mexico,581).
pop(uk,559).
pop(italy,554).
pop(france,525).
pop(philippines,415).
pop(thailand,410).
pop(turkey,383).
pop(egypt,364).
pop(spain,352).
pop(poland,337).
pop(s_korea,335).
pop(iran,320).
pop(ethiopia,272).
pop(argentina,251).

area(china,3380).
area(india,1139).
area(ussr,8708).
area(usa,3609).
area(indonesia,570).
area(japan,148).
area(brazil,3288).
area(bangladesh,55).
area(pakistan,311).
area(w_germany,96).
area(nigeria,373).
area(mexico,764).
area(uk,86).
area(italy,116).
area(france,213).
area(philippines,90).
area(thailand,200).
area(turkey,296).
area(egypt,386).
area(spain,190).
area(poland,121).
area(s_korea,37).
area(iran,628).
area(ethiopia,350).
area(argentina,1080).

/* --------------------------------------------------*/
/*       differen (times10,divide10,log10,ops8)      */
/*       These 4 examples are from Warren's thesis   */
/*       differen(150) will do.                      */

differen(N) :- common_loop(3, N, differen_top, 71, differen).

differen_top:-
        times10(I1),
        d(I1,x,_D1),
        divide10(I2),
        d(I2,x,_D2),
        log10(I3),
        d(I3,x,_D3),
        ops8(I4),
        d(I4,x,_D4).

d(U+V,X,DU+DV) :- !, d(U,X,DU), d(V,X,DV).
d(U-V,X,DU-DV) :- !, d(U,X,DU), d(V,X,DV).
d(U*V,X,DU*V+U*DV) :- !, d(U,X,DU), d(V,X,DV).
d(U/V,X,(DU*V-U*DV)/(^(V,2))) :- !, d(U,X,DU), d(V,X,DV).
d(^(U,N),X,DU*N*(^(U,N1))) :- !, integer(N), N1 is N - 1,d(U,X,DU).

d(-U,X,-DU) :- !, d(U,X,DU).
d(exp(U),X,exp(U)*DU) :- !, d(U,X,DU).
d(log(U),X,DU/U) :- !, d(U,X,DU).
d(X,X,1).
d(_C,_X,0).

times10( ((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x ).
divide10( ((((((((x/x)/x)/x)/x)/x)/x)/x)/x)/x ).
log10( log(log(log(log(log(log(log(log(log(log(x)))))))))) ).
ops8( (x+1)*((^(x,2)+2)*(^(x,3)+3)) ).

/* --------------------------------------------------- */
/*  Difference Lists                                   */
/*       quicksort on 50 items (difference lists)      */

diff(N) :- 
        list50(L),
        common_loop(3, N, qdsort(L, _), 608, diff).

qdsort([X|L],R-R0) :-
        dpartition(L,X,L1,L2),
        qdsort(L1,R-[X|R1]),
        qdsort(L2,R1-R0).
qdsort([],R0-R0).

dpartition([X|L],Y,[X|L1],L2) :-
        X<Y, !,
        dpartition(L,Y,L1,L2).
dpartition([X|L],Y,L1,[X|L2]) :-
        dpartition(L,Y,L1,L2).
dpartition([],_,[],[]).

/* -------------------------------------------------- */
/*  Naive reverse for a 30-length lists...        */


nrev(N) :- 
        Len = 30,
        LIs = (Len * (Len + 3))/2 + 1,
        conslist(Len, List),
        common_loop(3, N, nreverse(List, _), LIs, nrev).


nreverse([], []).
nreverse([X|L0],L) :- 
        nreverse(L0, L1),
        concatenate(L1, [X], L).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :- concatenate(L1, L2, L3).

conslist(0, []).
conslist(N, [N|L]) :-
        N > 0,
        N1 is N-1,
        conslist(N1, L).
/*
        3. Real Prolog programs.

This section is empty for now. We would like to have your advice
and/or your programs, with comments on how significant they are to be
considered candidates for benchmarking, and figures on how long they
take, which kinds of queries are advisable, etc... . Thank you for
your collaboration.

ALTHOUGH EMPTY, THIS SECTION SHOULD BE FILLED IN BY AS MANY REAL
PROGRAMS AS POSSIBLE, COMING FROM AS DIFFERENT SOURCES AS
POSSIBLE. ONLY A JOINT EFFORT MAY LEAD TO A SET OF COMPREHENSIVE
PROGRAMS, WHOSE EVALUATION MAY HELP EVERYBODY IN THE (NARROW BUT
GROWING) PROLOG FANS COMMUNITY.

------------------------------*/
