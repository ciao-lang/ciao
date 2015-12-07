
/*
1.2. Program to test non deterministic behaviour

This program contains a series of 3 different benchmark predicates.

The predicate "choice_point(N)" tests calls invoking the creation
of a choice point, i.e. a branch point where the execution
will possibly come back to in case of backtracking. It
does NOT backtrack. Two versions are proposed, one with and the
other without arguments.

We then present two predicates to evaluate the mechanism of
backtracking during execution. Both predicates create one
choice_point and then backtrack 20 times on every loop iteration
step. "baktrak1(N)" exhibits a kind of backtracking called "deep",
while "baktrak2(N)" deals with "shallow" backtracking.
Both are worth being tried, whatever your particular Prolog
System is.

------------cut here - beginning of program listing----------------
*/

:- module(choice,
	[choice_point/1,
	 choice_point0ar/1,
	 baktrak1/1,
	 baktrak2/1],
         []).

:- use_module(benchmark_utilities).

/* The predicates are called:                             */

/* o  "choice_point(N)"    - creation of choice points    */
/* o  "choice_point0ar(N)  - same, with 0 arg             */
/* o  "baktrak1(N)"        - deep backtracking            */
/* o  "baktrak2(N)"        - shallow backtracking         */

/*  N is the number of loop iterations executed  */


/* predicate to test creation of choice points without baktracking */

/* suggested value for N: 1000 */
/* results for  Cprolog N=1000 */
/* Tloop=5.95 Tcompens=0.98 Tnet=4.97 Klips=4.02 */

choice_point(N):- common_loop(3, N, ccp1(0,0,0), 20, choice_point).

/* predicate choice_point, but with zero argument  */
/* suggested value for N: 1000 */
/* results for Cprolog: N=1000 */
/* Tloop=3.55 Tcompens=0.98 Tnet=2.57 Klips=7.7  */

choice_point0ar(N):- common_loop(3, N, ccp1_0, 20, choice_point0ar).

/* Predicate to test the (deep) backtracking mechanism. */
/* suggested value for N: 1000 (interp), 2000(comp) */
/* results for Cprolog: N=1000  */
/* Tloop=9.63 Tcomp=1 Tnet=8.63 Klips=2.32  */

baktrak1(N):- common_loop(3, N, pd(_,_,_), 20, baktrak1).

/* Predicate to test the (shallow) backtracking mechanism */
/* suggested value for N: 1000 (interp), 2000 (comp) */
/* results for Cprolog: N=1000  */
/* Tloop=3.63  Tcomp=0.95 Tnet=2.68 Klips=7.45 */

baktrak2(N):- common_loop(3, N, ps(_,a,d), 20, baktrak2).

/* ccp1 creates 20 choice points */
/* ccp1 is the beginning of a set of predicates  */
/* composed of 2 clauses each. Every invokation of nd0 will create */
/* a sequence of 20 choice points. The body of the clauses are     */
/* limited to one goal, thus avoiding a creation of environment    */
/* when the clause is activated. nd0, and its successors, have     */
/*   three arguments to comply with our average static analysis    */
/*   results made on more than 30 real Prolog programs.            */
/* ccpXX exists with 3 arguments, and 0 args. */

ccp1(X,Y,Z):-ccp2(X,Y,Z).
ccp1(_X,_Y,_Z).
ccp2(X,Y,Z):-ccp3(X,Y,Z).
ccp2(_X,_Y,_Z).
ccp3(X,Y,Z):-ccp4(X,Y,Z).
ccp3(_X,_Y,_Z).
ccp4(X,Y,Z):-ccp5(X,Y,Z).
ccp4(_X,_Y,_Z).
ccp5(X,Y,Z):-ccp6(X,Y,Z).
ccp5(_X,_Y,_Z).
ccp6(X,Y,Z):-ccp7(X,Y,Z).
ccp6(_X,_Y,_Z).
ccp7(X,Y,Z):-ccp8(X,Y,Z).
ccp7(_X,_Y,_Z).
ccp8(X,Y,Z):-ccp9(X,Y,Z).
ccp8(_X,_Y,_Z).
ccp9(X,Y,Z):-ccp10(X,Y,Z).
ccp9(_X,_Y,_Z).
ccp10(X,Y,Z):-ccp11(X,Y,Z).
ccp10(_X,_Y,_Z).
ccp11(X,Y,Z):-ccp12(X,Y,Z).
ccp11(_X,_Y,_Z).
ccp12(X,Y,Z):-ccp13(X,Y,Z).
ccp12(_X,_Y,_Z).
ccp13(X,Y,Z):-ccp14(X,Y,Z).
ccp13(_X,_Y,_Z).
ccp14(X,Y,Z):-ccp15(X,Y,Z).
ccp14(_X,_Y,_Z).
ccp15(X,Y,Z):-ccp16(X,Y,Z).
ccp15(_X,_Y,_Z).
ccp16(X,Y,Z):-ccp17(X,Y,Z).
ccp16(_X,_Y,_Z).
ccp17(X,Y,Z):-ccp18(X,Y,Z).
ccp17(_X,_Y,_Z).
ccp18(X,Y,Z):-ccp19(X,Y,Z).
ccp18(_X,_Y,_Z).
ccp19(X,Y,Z):-ccp20(X,Y,Z).
ccp19(_X,_Y,_Z).

ccp20(_X,_Y,_Z).
ccp20(_X,_Y,_Z).

ccp1_0:-ccp2_0.
ccp1_0.
ccp2_0:-ccp3_0.
ccp2_0.
ccp3_0:-ccp4_0.
ccp3_0.
ccp4_0:-ccp5_0.
ccp4_0.
ccp5_0:-ccp6_0.
ccp5_0.
ccp6_0:-ccp7_0.
ccp6_0.
ccp7_0:-ccp8_0.
ccp7_0.
ccp8_0:-ccp9_0.
ccp8_0.
ccp9_0:-ccp10_0.
ccp9_0.
ccp10_0:-ccp11_0.
ccp10_0.
ccp11_0:-ccp12_0.
ccp11_0.
ccp12_0:-ccp13_0.
ccp12_0.
ccp13_0:-ccp14_0.
ccp13_0.
ccp14_0:-ccp15_0.
ccp14_0.
ccp15_0:-ccp16_0.
ccp15_0.
ccp16_0:-ccp17_0.
ccp16_0.
ccp17_0:-ccp18_0.
ccp17_0.
ccp18_0:-ccp19_0.
ccp18_0.
ccp19_0:-ccp20_0.
ccp19_0.

ccp20_0.
ccp20_0.


/*  deep backtracking */
/*  The call to pd creates a choice point, and invokes a      */
/*  call to q. It will fail and there will be a backtracking  */
/*  step  to try the next clause defining pd. pd has 21       */
/*  clauses,thus failure                                      */
/*  occurs 20 times                                           */

pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(X1,X2,_) :- q(X1,X2,a).
pd(_X1,_X2,_).

q(_X1,_X2,b).


/* shallow backtracking */
/* The ps predicate fails 20 times. The shallow backtracking   */
/* will not restore all current state registers in Prolog      */
/* systems which perform this optimisation, while others will. */

ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,X,X).
ps(_,_,_).

/* ---------------------cut here - end of program listing------------- */
