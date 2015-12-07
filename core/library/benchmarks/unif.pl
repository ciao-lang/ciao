
/*
1.5. Programs to test unification.

We have 6 programs to evaluate the process of unification in
the Prolog system. As usual, we will use the "compens_loop"
predicate as a compensation for the extra work done for looping
a little, and the "print_times" predicate to print results.

----------cut here - beginning of program listing-----------------
*/
:- module(unif, [
        construct_list/1,
	match_list/1,
	construct_structure/1,
	match_structure/1,
	match_nested_structure/1,
	general_unification/1
                 ], []).

:- use_module(benchmark_utilities).

/* The predicates are called with:                           */
/*             benchmark_name(X).                            */
/* where benchmark_name is the name of the predicate         */
/*        and X is the number of (external) loop iterations  */
/* The benchmarks on this file are:   construct_list         */
/*                                    match_list             */
/*                                    construct_structure    */
/*                                    match_structure        */
/*                                    match_nested_structure */
/*                                    general_unification    */

/*  Test of list construction via unification */
/* suggested value for N: 100 (interp), 500(comp) */
/* results for Cprolog: N=100  */
/* Tloop=2.49 Tcomp=0.08 Tnet=2.41 Klips=4.2  */
construct_list(X):- common_loop(3, X, cl1(_,_,_), 100, construct_list).


/*  Test of list matching unification */
/* suggested value for N: 100 (interp), 1000(comp) */
/* results for Cprolog: N=100  */
/* Tloop=4.56 Tcomp=0.1 Tnet=4.46 Klips=2.2  */
match_list(X) :-
        list100(Z),
        common_loop(3, X, cl1(Z,Z,Z), 100, match_list).


/*  Test of structure construction via unification      */

/* this program is equivalent to construct_list, except */
/* that it uses the standard structure representation   */
/*instead of the simplified list notation               */
/* suggested value for N: 100 (interp), 500(comp)       */
/* results for Cprolog: N=100                           */
/* Tloop=2.56 Tcomp=0.08 Tnet=2.48 Klips=4              */
construct_structure(X):- 
        common_loop(3, X, cs1(_,_,_), 100, construct_structure).


/*  Test of structure matching via unification    */
/* this predicate matches a list of 100 elements  */
/* in structure notation                          */
/* suggested value for N: 100 (interp), 100(comp) */
/* results for Cprolog: N=100                     */
/* Tloop=4.66 Tcomp=0.1 Tnet=4.56 Klips=2.2       */
match_structure(X):- 
        structure100(Z),
        common_loop(3, X, cs1(Z,Z,Z), 100, match_structure).

/*  Test to match a nested structure               */
/* this predicate tests the (compiled) unification */
/* of a complex structure                          */
/* suggested value for N: 200 (interp), 200(comp)  */
/* results for Cprolog: N=200                      */
/* Tloop=1.34 Tcomp=0.17 Tnet=1.18 Klips=0.17      */
match_nested_structure(X):-
        nested_structure1(Z),
        common_loop(3, X, nested_structure1(Z), 1, match_nested_structure).


/*     Test of general unification of 2 complex structures     */

/* This predicate tests general unification.                   */
/* We call it general unification, because it cannot           */
/* be analysed at compile time. Therefore this kind of         */
/* unification cannot be compiled and, even in                 */
/* a compiled system, it must be handled at                    */
/* run time, exactly as by an interpreter.                     */
/* This is done by a general procedure for unification.        */
/* The name of the benchmark therefore does not                */
/* reflect that the unification is general, i.e. including     */
/* all Prolog types (e.g. it does not contain variables),      */
/* but it reflects the use of the procedure for general        */
/* unification as opposed to specific, compiled unification.   */


/* suggested value for N: 200 (interp), 500(comp) */
/* results for Cprolog: N=200  */
/* Tloop=1.38 Tcomp=0.18 Tnet=1.20 Klips=0.17  */
general_unification(X) :-
        nested_structure1(A),
        nested_structure2(B),
        common_loop(3, X, unify(A, B), 1, general_unification).

/* general unification */
unify(X,X).

/* complex structure as example for unification tests      */

/* the same structure is given twice, in order to make     */
/* sure that even implementations using structure sharing  */
/* execute the unification and do not just pass pointers   */

nested_structure1(
[a(    [a1([1,2,3],a),a2([4,5,6],b),a3([7,8,9],c)],
       [a4([0,1,2],d),a5([3,4,5],e),a6([6,7,8],f)],
       [a7([9,0,1],g),a8([2,3,4],h),a9([5,6,7],i)]),
 b(    [b1([1,2,3],a),b2([4,5,6],b),b3([7,8,9],c)],
       [b4([0,1,2],d),b5([3,4,5],e),b6([6,7,8],f)],
       [b7([9,0,1],g),b8([2,3,4],h),b9([5,6,7],i)]),
 c(    [c1([1,2,3],a),c2([4,5,6],b),c3([7,8,9],c)],
       [c4([0,1,2],d),c5([3,4,5],e),c6([6,7,8],f)],
       [c7([9,0,1],g),c8([2,3,4],h),c9([5,6,7],i)])]).


nested_structure2(
[a(    [a1([1,2,3],a),a2([4,5,6],b),a3([7,8,9],c)],
       [a4([0,1,2],d),a5([3,4,5],e),a6([6,7,8],f)],
       [a7([9,0,1],g),a8([2,3,4],h),a9([5,6,7],i)]),
 b(    [b1([1,2,3],a),b2([4,5,6],b),b3([7,8,9],c)],
       [b4([0,1,2],d),b5([3,4,5],e),b6([6,7,8],f)],
       [b7([9,0,1],g),b8([2,3,4],h),b9([5,6,7],i)]),
 c(    [c1([1,2,3],a),c2([4,5,6],b),c3([7,8,9],c)],
       [c4([0,1,2],d),c5([3,4,5],e),c6([6,7,8],f)],
       [c7([9,0,1],g),c8([2,3,4],h),c9([5,6,7],i)])]).

/* list of 100 elements used for match_list */
list100([a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
           a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
           a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
           a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]).

/* structure of 100 elements used for match_structure   */
structure100(st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,st(a,
             st(a,nil)))))))))))))))))))))))))))))))))
             ))))))))))))))))))))))))))))
             )))))))))))))))))))))))))))))))))))))))).

/* predicates to test unification of lists */
cl1([a|X],[a|Y],[a|Z]) :- cl2(X,Y,Z).
cl2([a|X],[a|Y],[a|Z]) :- cl3(X,Y,Z).
cl3([a|X],[a|Y],[a|Z]) :- cl4(X,Y,Z).
cl4([a|X],[a|Y],[a|Z]) :- cl5(X,Y,Z).
cl5([a|X],[a|Y],[a|Z]) :- cl6(X,Y,Z).
cl6([a|X],[a|Y],[a|Z]) :- cl7(X,Y,Z).
cl7([a|X],[a|Y],[a|Z]) :- cl8(X,Y,Z).
cl8([a|X],[a|Y],[a|Z]) :- cl9(X,Y,Z).
cl9([a|X],[a|Y],[a|Z]) :- cl10(X,Y,Z).
cl10([a|X],[a|Y],[a|Z]) :- cl11(X,Y,Z).
cl11([a|X],[a|Y],[a|Z]) :- cl12(X,Y,Z).
cl12([a|X],[a|Y],[a|Z]) :- cl13(X,Y,Z).
cl13([a|X],[a|Y],[a|Z]) :- cl14(X,Y,Z).
cl14([a|X],[a|Y],[a|Z]) :- cl15(X,Y,Z).
cl15([a|X],[a|Y],[a|Z]) :- cl16(X,Y,Z).
cl16([a|X],[a|Y],[a|Z]) :- cl17(X,Y,Z).
cl17([a|X],[a|Y],[a|Z]) :- cl18(X,Y,Z).
cl18([a|X],[a|Y],[a|Z]) :- cl19(X,Y,Z).
cl19([a|X],[a|Y],[a|Z]) :- cl20(X,Y,Z).
cl20([a|X],[a|Y],[a|Z]) :- cl21(X,Y,Z).
cl21([a|X],[a|Y],[a|Z]) :- cl22(X,Y,Z).
cl22([a|X],[a|Y],[a|Z]) :- cl23(X,Y,Z).
cl23([a|X],[a|Y],[a|Z]) :- cl24(X,Y,Z).
cl24([a|X],[a|Y],[a|Z]) :- cl25(X,Y,Z).
cl25([a|X],[a|Y],[a|Z]) :- cl26(X,Y,Z).
cl26([a|X],[a|Y],[a|Z]) :- cl27(X,Y,Z).
cl27([a|X],[a|Y],[a|Z]) :- cl28(X,Y,Z).
cl28([a|X],[a|Y],[a|Z]) :- cl29(X,Y,Z).
cl29([a|X],[a|Y],[a|Z]) :- cl30(X,Y,Z).
cl30([a|X],[a|Y],[a|Z]) :- cl31(X,Y,Z).
cl31([a|X],[a|Y],[a|Z]) :- cl32(X,Y,Z).
cl32([a|X],[a|Y],[a|Z]) :- cl33(X,Y,Z).
cl33([a|X],[a|Y],[a|Z]) :- cl34(X,Y,Z).
cl34([a|X],[a|Y],[a|Z]) :- cl35(X,Y,Z).
cl35([a|X],[a|Y],[a|Z]) :- cl36(X,Y,Z).
cl36([a|X],[a|Y],[a|Z]) :- cl37(X,Y,Z).
cl37([a|X],[a|Y],[a|Z]) :- cl38(X,Y,Z).
cl38([a|X],[a|Y],[a|Z]) :- cl39(X,Y,Z).
cl39([a|X],[a|Y],[a|Z]) :- cl40(X,Y,Z).
cl40([a|X],[a|Y],[a|Z]) :- cl41(X,Y,Z).
cl41([a|X],[a|Y],[a|Z]) :- cl42(X,Y,Z).
cl42([a|X],[a|Y],[a|Z]) :- cl43(X,Y,Z).
cl43([a|X],[a|Y],[a|Z]) :- cl44(X,Y,Z).
cl44([a|X],[a|Y],[a|Z]) :- cl45(X,Y,Z).
cl45([a|X],[a|Y],[a|Z]) :- cl46(X,Y,Z).
cl46([a|X],[a|Y],[a|Z]) :- cl47(X,Y,Z).
cl47([a|X],[a|Y],[a|Z]) :- cl48(X,Y,Z).
cl48([a|X],[a|Y],[a|Z]) :- cl49(X,Y,Z).
cl49([a|X],[a|Y],[a|Z]) :- cl50(X,Y,Z).
cl50([a|X],[a|Y],[a|Z]) :- cl51(X,Y,Z).
cl51([a|X],[a|Y],[a|Z]) :- cl52(X,Y,Z).
cl52([a|X],[a|Y],[a|Z]) :- cl53(X,Y,Z).
cl53([a|X],[a|Y],[a|Z]) :- cl54(X,Y,Z).
cl54([a|X],[a|Y],[a|Z]) :- cl55(X,Y,Z).
cl55([a|X],[a|Y],[a|Z]) :- cl56(X,Y,Z).
cl56([a|X],[a|Y],[a|Z]) :- cl57(X,Y,Z).
cl57([a|X],[a|Y],[a|Z]) :- cl58(X,Y,Z).
cl58([a|X],[a|Y],[a|Z]) :- cl59(X,Y,Z).
cl59([a|X],[a|Y],[a|Z]) :- cl60(X,Y,Z).
cl60([a|X],[a|Y],[a|Z]) :- cl61(X,Y,Z).
cl61([a|X],[a|Y],[a|Z]) :- cl62(X,Y,Z).
cl62([a|X],[a|Y],[a|Z]) :- cl63(X,Y,Z).
cl63([a|X],[a|Y],[a|Z]) :- cl64(X,Y,Z).
cl64([a|X],[a|Y],[a|Z]) :- cl65(X,Y,Z).
cl65([a|X],[a|Y],[a|Z]) :- cl66(X,Y,Z).
cl66([a|X],[a|Y],[a|Z]) :- cl67(X,Y,Z).
cl67([a|X],[a|Y],[a|Z]) :- cl68(X,Y,Z).
cl68([a|X],[a|Y],[a|Z]) :- cl69(X,Y,Z).
cl69([a|X],[a|Y],[a|Z]) :- cl70(X,Y,Z).
cl70([a|X],[a|Y],[a|Z]) :- cl71(X,Y,Z).
cl71([a|X],[a|Y],[a|Z]) :- cl72(X,Y,Z).
cl72([a|X],[a|Y],[a|Z]) :- cl73(X,Y,Z).
cl73([a|X],[a|Y],[a|Z]) :- cl74(X,Y,Z).
cl74([a|X],[a|Y],[a|Z]) :- cl75(X,Y,Z).
cl75([a|X],[a|Y],[a|Z]) :- cl76(X,Y,Z).
cl76([a|X],[a|Y],[a|Z]) :- cl77(X,Y,Z).
cl77([a|X],[a|Y],[a|Z]) :- cl78(X,Y,Z).
cl78([a|X],[a|Y],[a|Z]) :- cl79(X,Y,Z).
cl79([a|X],[a|Y],[a|Z]) :- cl80(X,Y,Z).
cl80([a|X],[a|Y],[a|Z]) :- cl81(X,Y,Z).
cl81([a|X],[a|Y],[a|Z]) :- cl82(X,Y,Z).
cl82([a|X],[a|Y],[a|Z]) :- cl83(X,Y,Z).
cl83([a|X],[a|Y],[a|Z]) :- cl84(X,Y,Z).
cl84([a|X],[a|Y],[a|Z]) :- cl85(X,Y,Z).
cl85([a|X],[a|Y],[a|Z]) :- cl86(X,Y,Z).
cl86([a|X],[a|Y],[a|Z]) :- cl87(X,Y,Z).
cl87([a|X],[a|Y],[a|Z]) :- cl88(X,Y,Z).
cl88([a|X],[a|Y],[a|Z]) :- cl89(X,Y,Z).
cl89([a|X],[a|Y],[a|Z]) :- cl90(X,Y,Z).
cl90([a|X],[a|Y],[a|Z]) :- cl91(X,Y,Z).
cl91([a|X],[a|Y],[a|Z]) :- cl92(X,Y,Z).
cl92([a|X],[a|Y],[a|Z]) :- cl93(X,Y,Z).
cl93([a|X],[a|Y],[a|Z]) :- cl94(X,Y,Z).
cl94([a|X],[a|Y],[a|Z]) :- cl95(X,Y,Z).
cl95([a|X],[a|Y],[a|Z]) :- cl96(X,Y,Z).
cl96([a|X],[a|Y],[a|Z]) :- cl97(X,Y,Z).
cl97([a|X],[a|Y],[a|Z]) :- cl98(X,Y,Z).
cl98([a|X],[a|Y],[a|Z]) :- cl99(X,Y,Z).
cl99([a|X],[a|Y],[a|Z]) :- cl100(X,Y,Z).
cl100([a],[a],[a]).


/* predicates to test unification of structures */

cs1(st(a,X),st(a,Y),st(a,Z)) :- cs2(X,Y,Z).
cs2(st(a,X),st(a,Y),st(a,Z)) :- cs3(X,Y,Z).
cs3(st(a,X),st(a,Y),st(a,Z)) :- cs4(X,Y,Z).
cs4(st(a,X),st(a,Y),st(a,Z)) :- cs5(X,Y,Z).
cs5(st(a,X),st(a,Y),st(a,Z)) :- cs6(X,Y,Z).
cs6(st(a,X),st(a,Y),st(a,Z)) :- cs7(X,Y,Z).
cs7(st(a,X),st(a,Y),st(a,Z)) :- cs8(X,Y,Z).
cs8(st(a,X),st(a,Y),st(a,Z)) :- cs9(X,Y,Z).
cs9(st(a,X),st(a,Y),st(a,Z)) :- cs10(X,Y,Z).
cs10(st(a,X),st(a,Y),st(a,Z)) :- cs11(X,Y,Z).
cs11(st(a,X),st(a,Y),st(a,Z)) :- cs12(X,Y,Z).
cs12(st(a,X),st(a,Y),st(a,Z)) :- cs13(X,Y,Z).
cs13(st(a,X),st(a,Y),st(a,Z)) :- cs14(X,Y,Z).
cs14(st(a,X),st(a,Y),st(a,Z)) :- cs15(X,Y,Z).
cs15(st(a,X),st(a,Y),st(a,Z)) :- cs16(X,Y,Z).
cs16(st(a,X),st(a,Y),st(a,Z)) :- cs17(X,Y,Z).
cs17(st(a,X),st(a,Y),st(a,Z)) :- cs18(X,Y,Z).
cs18(st(a,X),st(a,Y),st(a,Z)) :- cs19(X,Y,Z).
cs19(st(a,X),st(a,Y),st(a,Z)) :- cs20(X,Y,Z).
cs20(st(a,X),st(a,Y),st(a,Z)) :- cs21(X,Y,Z).
cs21(st(a,X),st(a,Y),st(a,Z)) :- cs22(X,Y,Z).
cs22(st(a,X),st(a,Y),st(a,Z)) :- cs23(X,Y,Z).
cs23(st(a,X),st(a,Y),st(a,Z)) :- cs24(X,Y,Z).
cs24(st(a,X),st(a,Y),st(a,Z)) :- cs25(X,Y,Z).
cs25(st(a,X),st(a,Y),st(a,Z)) :- cs26(X,Y,Z).
cs26(st(a,X),st(a,Y),st(a,Z)) :- cs27(X,Y,Z).
cs27(st(a,X),st(a,Y),st(a,Z)) :- cs28(X,Y,Z).
cs28(st(a,X),st(a,Y),st(a,Z)) :- cs29(X,Y,Z).
cs29(st(a,X),st(a,Y),st(a,Z)) :- cs30(X,Y,Z).
cs30(st(a,X),st(a,Y),st(a,Z)) :- cs31(X,Y,Z).
cs31(st(a,X),st(a,Y),st(a,Z)) :- cs32(X,Y,Z).
cs32(st(a,X),st(a,Y),st(a,Z)) :- cs33(X,Y,Z).
cs33(st(a,X),st(a,Y),st(a,Z)) :- cs34(X,Y,Z).
cs34(st(a,X),st(a,Y),st(a,Z)) :- cs35(X,Y,Z).
cs35(st(a,X),st(a,Y),st(a,Z)) :- cs36(X,Y,Z).
cs36(st(a,X),st(a,Y),st(a,Z)) :- cs37(X,Y,Z).
cs37(st(a,X),st(a,Y),st(a,Z)) :- cs38(X,Y,Z).
cs38(st(a,X),st(a,Y),st(a,Z)) :- cs39(X,Y,Z).
cs39(st(a,X),st(a,Y),st(a,Z)) :- cs40(X,Y,Z).
cs40(st(a,X),st(a,Y),st(a,Z)) :- cs41(X,Y,Z).
cs41(st(a,X),st(a,Y),st(a,Z)) :- cs42(X,Y,Z).
cs42(st(a,X),st(a,Y),st(a,Z)) :- cs43(X,Y,Z).
cs43(st(a,X),st(a,Y),st(a,Z)) :- cs44(X,Y,Z).
cs44(st(a,X),st(a,Y),st(a,Z)) :- cs45(X,Y,Z).
cs45(st(a,X),st(a,Y),st(a,Z)) :- cs46(X,Y,Z).
cs46(st(a,X),st(a,Y),st(a,Z)) :- cs47(X,Y,Z).
cs47(st(a,X),st(a,Y),st(a,Z)) :- cs48(X,Y,Z).
cs48(st(a,X),st(a,Y),st(a,Z)) :- cs49(X,Y,Z).
cs49(st(a,X),st(a,Y),st(a,Z)) :- cs50(X,Y,Z).
cs50(st(a,X),st(a,Y),st(a,Z)) :- cs51(X,Y,Z).
cs51(st(a,X),st(a,Y),st(a,Z)) :- cs52(X,Y,Z).
cs52(st(a,X),st(a,Y),st(a,Z)) :- cs53(X,Y,Z).
cs53(st(a,X),st(a,Y),st(a,Z)) :- cs54(X,Y,Z).
cs54(st(a,X),st(a,Y),st(a,Z)) :- cs55(X,Y,Z).
cs55(st(a,X),st(a,Y),st(a,Z)) :- cs56(X,Y,Z).
cs56(st(a,X),st(a,Y),st(a,Z)) :- cs57(X,Y,Z).
cs57(st(a,X),st(a,Y),st(a,Z)) :- cs58(X,Y,Z).
cs58(st(a,X),st(a,Y),st(a,Z)) :- cs59(X,Y,Z).
cs59(st(a,X),st(a,Y),st(a,Z)) :- cs60(X,Y,Z).
cs60(st(a,X),st(a,Y),st(a,Z)) :- cs61(X,Y,Z).
cs61(st(a,X),st(a,Y),st(a,Z)) :- cs62(X,Y,Z).
cs62(st(a,X),st(a,Y),st(a,Z)) :- cs63(X,Y,Z).
cs63(st(a,X),st(a,Y),st(a,Z)) :- cs64(X,Y,Z).
cs64(st(a,X),st(a,Y),st(a,Z)) :- cs65(X,Y,Z).
cs65(st(a,X),st(a,Y),st(a,Z)) :- cs66(X,Y,Z).
cs66(st(a,X),st(a,Y),st(a,Z)) :- cs67(X,Y,Z).
cs67(st(a,X),st(a,Y),st(a,Z)) :- cs68(X,Y,Z).
cs68(st(a,X),st(a,Y),st(a,Z)) :- cs69(X,Y,Z).
cs69(st(a,X),st(a,Y),st(a,Z)) :- cs70(X,Y,Z).
cs70(st(a,X),st(a,Y),st(a,Z)) :- cs71(X,Y,Z).
cs71(st(a,X),st(a,Y),st(a,Z)) :- cs72(X,Y,Z).
cs72(st(a,X),st(a,Y),st(a,Z)) :- cs73(X,Y,Z).
cs73(st(a,X),st(a,Y),st(a,Z)) :- cs74(X,Y,Z).
cs74(st(a,X),st(a,Y),st(a,Z)) :- cs75(X,Y,Z).
cs75(st(a,X),st(a,Y),st(a,Z)) :- cs76(X,Y,Z).
cs76(st(a,X),st(a,Y),st(a,Z)) :- cs77(X,Y,Z).
cs77(st(a,X),st(a,Y),st(a,Z)) :- cs78(X,Y,Z).
cs78(st(a,X),st(a,Y),st(a,Z)) :- cs79(X,Y,Z).
cs79(st(a,X),st(a,Y),st(a,Z)) :- cs80(X,Y,Z).
cs80(st(a,X),st(a,Y),st(a,Z)) :- cs81(X,Y,Z).
cs81(st(a,X),st(a,Y),st(a,Z)) :- cs82(X,Y,Z).
cs82(st(a,X),st(a,Y),st(a,Z)) :- cs83(X,Y,Z).
cs83(st(a,X),st(a,Y),st(a,Z)) :- cs84(X,Y,Z).
cs84(st(a,X),st(a,Y),st(a,Z)) :- cs85(X,Y,Z).
cs85(st(a,X),st(a,Y),st(a,Z)) :- cs86(X,Y,Z).
cs86(st(a,X),st(a,Y),st(a,Z)) :- cs87(X,Y,Z).
cs87(st(a,X),st(a,Y),st(a,Z)) :- cs88(X,Y,Z).
cs88(st(a,X),st(a,Y),st(a,Z)) :- cs89(X,Y,Z).
cs89(st(a,X),st(a,Y),st(a,Z)) :- cs90(X,Y,Z).
cs90(st(a,X),st(a,Y),st(a,Z)) :- cs91(X,Y,Z).
cs91(st(a,X),st(a,Y),st(a,Z)) :- cs92(X,Y,Z).
cs92(st(a,X),st(a,Y),st(a,Z)) :- cs93(X,Y,Z).
cs93(st(a,X),st(a,Y),st(a,Z)) :- cs94(X,Y,Z).
cs94(st(a,X),st(a,Y),st(a,Z)) :- cs95(X,Y,Z).
cs95(st(a,X),st(a,Y),st(a,Z)) :- cs96(X,Y,Z).
cs96(st(a,X),st(a,Y),st(a,Z)) :- cs97(X,Y,Z).
cs97(st(a,X),st(a,Y),st(a,Z)) :- cs98(X,Y,Z).
cs98(st(a,X),st(a,Y),st(a,Z)) :- cs99(X,Y,Z).
cs99(st(a,X),st(a,Y),st(a,Z)) :- cs100(X,Y,Z).
cs100(st(a,nil),st(a,nil),st(a,nil)).
/*-----------------------CUT HERE (END OF PROGRAM)-----*/
