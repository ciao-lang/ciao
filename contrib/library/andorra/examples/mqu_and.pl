:- module(mqu_and,[queens/1],[andorra]).

:- use_module(library(write), [write/1]).
:- use_module(library(prolog_sys), [statistics/2]).

queens(A) :-
        queens(A,A,A,B),
        nl,
        displ(B).

%:- determinate(zero(A),nonvar(A)).

zero([]).
zero([0|A]) :-
        zero(A).


adddig([A|B],C,[[A]|D]) :-
        add(B,C,D).

%:- determinate(place(A,B), (nonvar(A), ( A==0 ; instantiated(B,[1]) ) ) ).
%:- determinate(place(A,B), (nonvar(A), ( A==0 ; nonvar(B),B=[X|_],nonvar(X) ))).
%:- determinate(place(A,B), ( nonvar(A),A=0 ; 
%	                     nonvar(A),A=s(_),nonvar(B),B=[X|_],nonvar(X) ) ).

place(0,A) :-
        zero(A).
place(s(A),[q|B]) :-
        place(A,B).
place(s(A),[0|B]) :-
        place(s(A),B).

%% :- determinate(app(A,B,C), ( nonvar(A) ; B?\=C ; C?\=[_|_] ) ).
%:- determinate(app(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ; 
%	                     nonvar(C),\+C=[_|_] ) ).

app([],A,A).
app([A|B],C,[A|D]) :-
        app(B,C,D).

%:- determinate(seed(A,B), ( nonvar(A) ; nonvar(B) ) ).

seed(0,[]).
seed(s(A),[[]|B]) :-
        seed(A,B).

%:- determinate(displ(A),nonvar(A)).

displ([]).
displ([A|B]) :-
        write(A),
        nl,
        displ(B).

%% :- determinate(board(A,B,C,D,E,F,G), 
%% 	( nonvar(A) ; B?\=s(_); C?\=[] ; nonvar(D) ; F?\=G ) ).
%:- determinate(board(A,B,C,D,E,F,G), 
%	( nonvar(A) ; nonvar(B),\+B=s(_); nonvar(C),\+C=[]; nonvar(D) ; 
%	  nonvar(F),nonvar(G),\+F=G ) ).

board(0,s(A),[],[],B,C,C) :-
        seed(s(A),B),
        seed(A,C).
board(s(A),B,C,[D|E],F,G,H) :-
        board(A,B,I,E,J,K,L),
        new(B,D),
        app(D,I,C),
        add(D,J,F),
        adddig(D,K,G),
        rev(D,[],M),
        adddig(M,L,H).

%:- determinate(atmost1(A), ( atomic(A) ; instantiated(A,[1]) ) ).
%:- determinate(atmost1(A), ( nonvar(A), ( A==[] ; A=[X|_],nonvar(X) ) ) ).
%:- determinate(atmost1(A), ( nonvar(A),A=[] ; nonvar(A),A=[X|_],nonvar(X) ) ).

atmost1([]).
atmost1([q|A]) :-
        zero(A).
atmost1([0|A]) :-
        atmost1(A).

%:- determinate(add(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ;
%	nonvar(B),\+B=[_|_] ; nonvar(C),\+C=[_|_] ) ).

add([],A,A).
add([A|B],[C|D],[[A|C]|E]) :-
        add(B,D,E).

%:- determinate(check(A),nonvar(A)).

check([]).
check([A|B]) :-
        atmost1(A),
        check(B).

%:- determinate(rev(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ) ).

rev([],A,A).
rev([A|B],C,D) :-
        rev(B,[A|C],D).

%:- determinate(new(A,B), ( nonvar(A) ; nonvar(B) ) ).

new(0,[]).
new(s(A),[B|C]) :-
        new(A,C).


queens(A,B,C,D) :-
        board(B,C,E,F,G,H,I),
        place(A,E),
        check(F),
        check(G),
        check(H),
        check(I),
        D=F.

%%%%%%%%%%%%%%%%%%%%%

ourmain:-
	statistics(runtime,_),
	ourdo,
	statistics(runtime,[_,T1]),
        write(T1).

%:- determinate(ourdo,true).

ourdo:- 
	queens(s(s(s(s(s(s(s(s(0))))))))),
	fail.
ourdo.
