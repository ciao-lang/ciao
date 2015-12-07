pdef([generator(Right,Cur:integer,Max:integer),[24,1,24,42]],
	[pref([out(Right,Cur),[25,2,25,12]],[pref([(Cur1 is (Cur + 1)),[26,2,26,14]],[if([(Cur =< Max),[27,5,27,5]],[generator(Right,Cur1,Max),[28,8,28,34]],[end,[29,8,29,10]]),[27,2,29,10]]),[26,2,29,10]]),[25,2,29,10]],
	[(Cur1,'Cur1'), (Max,'Max'), (Cur,'Cur'), (Right,'Right')]).

pdef([unit(Left,Right),[35,1,35,17]],
	[pref([in(Left,P),[36,2,36,9]],[filter(Left,Right,P),[37,2,37,23]]),[36,2,37,23]],
	[(P,'P'), (Right,'Right'), (Left,'Left')]).

pdef([filter(Left,Right,P),[39,1,39,22]],
	[pref([in(Left,N),[40,2,40,9]],[pref([(Rem is (N mod P)),[41,2,41,15]],[pref([if([(Rem == 0),[42,7,42,7]],[true,[43,11,43,14]],[out(Right,N),[44,11,44,19]]),[42,2,45,2]],[filter(Left,Right,P),[46,2,46,23]]),[42,2,46,23]]),[41,2,46,23]]),[40,2,46,23]],
	[(Rem,'Rem'), (N,'N'), (P,'P'), (Right,'Right'), (Left,'Left')]).

pdef([consumer(Left,N:integer,MAX:integer),[51,1,51,38]],
	[pref([in(Left,X),[52,2,52,9]],[pref([(N1 is (N - 1)),[53,2,53,10]],[if([(N1 > 0),[54,5,54,5]],[consumer(Left,N1,MAX),[55,9,55,31]],[pref([if([(X == MAX),[56,14,56,14]],[action(finish),[57,12,57,25]],[action(error),[58,12,58,24]]),[56,11,58,24]],[end,[59,6,59,8]]),[56,9,60,3]]),[54,2,60,3]]),[53,2,60,3]]),[52,2,60,3]],
	[(N1,'N1'), (X,'X'), (MAX,'MAX'), (N,'N'), (Left,'Left')]).

pdef([medium(Left,Right),[66,1,66,19]],
	[pref([in(Left,X),[67,2,67,9]],[pref([out(Right,X),[68,2,68,10]],[medium(Left,Right),[69,2,69,20]]),[68,2,69,20]]),[67,2,69,20]],
	[(X,'X'), (Right,'Right'), (Left,'Left')]).

pdef([buf_filter(Left,Right),[74,1,74,23]],
	[par([medium(Left,Temp),[75,4,75,21]],[unit(Temp,Right),[76,4,76,20]]),[75,4,76,20]],
	[(Temp,'Temp'), (Right,'Right'), (Left,'Left')]).

pdef([units(Left,Right,NFILTER),[95,1,95,27]],
	[if([(NFILTER > 1),[96,5,96,5]],[pref([(N is (NFILTER - 1)),[97,11,97,26]],[par([unit(Left,T1),[97,31,97,44]],[units(T1,Right,N),[97,48,97,66]]),[97,30,97,67]]),[97,9,97,69]],[unit(Left,Right),[98,9,98,25]]),[96,2,98,25]],
	[(T1,'T1'), (N,'N'), (NFILTER,'NFILTER'), (Right,'Right'), (Left,'Left')]).

pdef([sieve(NFILTER,NOVER,MAX),[100,1,100,26]],
	[par([generator(Gen,2,MAX),[101,4,101,25]],[par([units(Gen,Cons,NFILTER),[102,4,102,28]],[consumer(Cons,NOVER,MAX),[103,4,103,29]]),[102,4,103,29]]),[101,4,103,29]],
	[(Cons,'Cons'), (Gen,'Gen'), (MAX,'MAX'), (NOVER,'NOVER'), (NFILTER,'NFILTER')]).

pdef([s1,[105,1,105,2]],
	[par([generator(Gen,2,9),[106,4,106,23]],[par([buf_filter(Gen,T1),[107,4,107,22]],[par([medium(T1,Cons),[108,4,108,19]],[consumer(Cons,4,9),[109,4,109,23]]),[108,4,109,23]]),[107,4,109,23]]),[106,2,110,2]],
	[(Cons,'Cons'), (T1,'T1'), (Gen,'Gen')]).

pdef([s2,[112,1,112,2]],
	[par([generator(Gen,2,13),[113,4,113,24]],[par([buf_filter(Gen,T1),[114,4,114,22]],[par([buf_filter(T1,T2),[115,4,115,21]],[par([medium(T2,Cons),[116,4,116,19]],[consumer(Cons,4,13),[117,4,117,24]]),[116,4,117,24]]),[115,4,117,24]]),[114,4,117,24]]),[113,2,118,2]],
	[(Cons,'Cons'), (T2,'T2'), (T1,'T1'), (Gen,'Gen')]).

pdef([s3,[120,1,120,2]],
	[par([generator(Gen,2,17),[121,4,121,24]],[par([buf_filter(Gen,T1),[122,4,122,22]],[par([buf_filter(T1,T2),[123,4,123,21]],[par([buf_filter(T2,T3),[124,4,124,21]],[par([medium(T3,Cons),[125,4,125,19]],[consumer(Cons,4,17),[126,4,126,24]]),[125,4,126,24]]),[124,4,126,24]]),[123,4,126,24]]),[122,4,126,24]]),[121,2,127,2]],
	[(Cons,'Cons'), (T3,'T3'), (T2,'T2'), (T1,'T1'), (Gen,'Gen')]).

pdef([s4,[129,1,129,2]],
	[par([generator(Gen,2,19),[130,4,130,24]],[par([buf_filter(Gen,T1),[131,4,131,22]],[par([buf_filter(T1,T2),[132,4,132,21]],[par([buf_filter(T2,T3),[133,4,133,21]],[par([buf_filter(T3,T4),[134,4,134,21]],[par([medium(T4,Cons),[135,4,135,19]],[consumer(Cons,4,19),[136,4,136,24]]),[135,4,136,24]]),[134,4,136,24]]),[133,4,136,24]]),[132,4,136,24]]),[131,4,136,24]]),[130,2,137,2]],
	[(Cons,'Cons'), (T4,'T4'), (T3,'T3'), (T2,'T2'), (T1,'T1'), (Gen,'Gen')]).

pdef([s5,[139,1,139,2]],
	[par([generator(Gen,2,23),[140,4,140,24]],[par([buf_filter(Gen,T1),[141,4,141,22]],[par([buf_filter(T1,T2),[142,4,142,21]],[par([buf_filter(T2,T3),[143,4,143,21]],[par([buf_filter(T3,T4),[144,4,144,21]],[par([buf_filter(T4,T5),[145,4,145,21]],[par([medium(T5,Cons),[146,4,146,19]],[consumer(Cons,4,23),[147,4,147,24]]),[146,4,147,24]]),[145,4,147,24]]),[144,4,147,24]]),[143,4,147,24]]),[142,4,147,24]]),[141,4,147,24]]),[140,2,148,2]],
	[(Cons,'Cons'), (T5,'T5'), (T4,'T4'), (T3,'T3'), (T2,'T2'), (T1,'T1'), (Gen,'Gen')]).

fdef(ae_finish, and(diamAll(tt),boxMinus(action(finish),form(ae_finish)))).
fneg(ae_finish, neg_form(ae_finish)).
fdef(ee_error, or(diam(action(error),tt),diamAll(form(ee_error)))).
fneg(ee_error, neg_form(ee_error)).

% ----------------------------------------------------------------

trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(generator_0(A,B,C,D),out(A,B),D) :-
    E  is  B + 1,
    not B =< C.
trans(generator_0(A,B,C,D),out(A,B),generator_0(A,E,C,D)) :-
    E  is  B + 1,
    B =< C.
trans(unit_0(A,B,C),in(A,D),filter_0(A,B,D,C)).
trans(filter_0(A,B,C,D),in(A,E),filter_5(B,A,E,C,D)) :-
    F  is  E  mod  C,
    not F == 0.
trans(filter_0(A,B,C,D),in(A,E),filter_0(A,B,C,D)) :-
    F  is  E  mod  C,
    F == 0.
trans(filter_5(A,B,C,D,E),out(A,C),filter_0(B,A,D,E)).
trans(consumer_0(A,B,C,D),in(A,E),consumer_6(D)) :-
    F  is  B - 1,
    not F > 0,
    E == C.
trans(consumer_0(A,B,C,D),in(A,E),consumer_7(D)) :-
    F  is  B - 1,
    not F > 0,
    not E == C.
trans(consumer_6(A),action(finish),A).
trans(consumer_7(A),action(error),A).
trans(consumer_0(A,B,C,D),in(A,E),consumer_0(A,F,C,D)) :-
    F  is  B - 1,
    F > 0.
trans(medium_0(A,B,C),in(A,D),medium_1(B,A,D,C)).
trans(medium_1(A,B,C,D),out(A,C),medium_0(B,A,D)).
trans(buf_filter_3_0(medium_0(A,B,C),D,E),in(A,F),buf_filter_3_0(medium_1(B,A,F,C),D,E)).
trans(buf_filter_3_0(A,filter_5(B,C,D,E,F),G),out(B,D),buf_filter_3_0(A,filter_0(C,B,E,F),G)).
trans(buf_filter_3_0(medium_1(A,B,C,D),filter_0(A,E,F,G),H),tau,buf_filter_3_0(medium_0(B,A,D),filter_5(E,A,C,F,G),H)) :-
    I  is  C  mod  F,
    not I == 0.
trans(buf_filter_3_0(medium_1(A,B,C,D),filter_0(A,E,F,G),H),tau,buf_filter_3_0(medium_0(B,A,D),filter_0(A,E,F,G),H)) :-
    I  is  C  mod  F,
    I == 0.
trans(buf_filter_3_0(medium_1(A,B,C,D),unit_0(A,E,F),G),tau,buf_filter_3_0(medium_0(B,A,D),filter_0(A,E,C,F),G)).
trans(buf_filter_0(A,B,C),nop,buf_filter_3_0(medium_0(A,D,end),unit_0(D,B,end),C)) :-
    handlechan([A,B],[D],E).
trans(buf_filter_3_0(end,end,A),nop,A).
trans(units_6(A,B,C),nop,unit_0(A,B,C)).
trans(units_7(A,B,C,D),nop,units_0(C,A,B,D)).
trans(units_0(A,B,C,D),nop,par(E,unit_0(A,F,end),units_0(F,B,G,end),D)) :-
    C > 1,
    G  is  C - 1,
    handlechan([A,B],[F],E).
trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(units_0(A,B,C,D),nop,unit_0(A,B,D)) :-
    not C > 1.
trans(sieve_5(A,B,C),nop,generator_0(B,2,A,C)).
trans(sieve_9(A,B,C,D),nop,units_0(B,C,A,D)).
trans(sieve_10(A,B,C,D),nop,consumer_0(C,A,B,D)).
trans(sieve_6(A,B,C,D,E),nop,par(F,units_0(D,G,A,end),consumer_0(G,B,C,end),E)) :-
    handlechan([D],[G],F).
trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(sieve_0(A,B,C,D),nop,par(E,generator_0(F,2,C,end),par(G,units_0(F,H,A,end),consumer_0(H,B,C,end),end),D)) :-
    handlechan([],[F],E),
    handlechan([F],[H],G).
trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(s1_7_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L),tau,s1_7_0(A,buf_filter_3_0(medium_0(C,B,E),filter_5(F,B,D,G,H),I),J,K,L)) :-
    M  is  D  mod  G,
    not M == 0.
trans(s1_7_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L),tau,s1_7_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,G,H),I),J,K,L)) :-
    M  is  D  mod  G,
    M == 0.
trans(s1_7_0(A,buf_filter_3_0(medium_1(B,C,D,E),unit_0(B,F,G),H),I,J,K),tau,s1_7_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,D,G),H),I,J,K)).
trans(s1_7_0(A,buf_filter_0(B,C,D),E,F,G),nop,s1_7_0(A,buf_filter_3_0(medium_0(B,H,end),unit_0(H,C,end),D),E,F,G)) :-
    handlechan([B,C],[H],I).
trans(s1_7_0(A,buf_filter_3_0(end,end,B),C,D,E),nop,s1_7_0(A,B,C,D,E)).
trans(s1_7_0(A,B,C,consumer_6(D),E),action(finish),s1_7_0(A,B,C,D,E)).
trans(s1_7_0(A,B,C,consumer_7(D),E),action(error),s1_7_0(A,B,C,D,E)).
trans(s1_7_0(A,B,C,consumer_6(D),E),action(finish),s1_7_0(A,B,C,D,E)).
trans(s1_7_0(A,B,C,consumer_7(D),E),action(error),s1_7_0(A,B,C,D,E)).
trans(s1_7_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K),tau,s1_7_0(D,buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K)) :-
    L  is  B + 1,
    not B =< C.
trans(s1_7_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K),tau,s1_7_0(generator_0(A,L,C,D),buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K)) :-
    L  is  B + 1,
    B =< C.
trans(s1_7_0(generator_0(A,2,9,B),buf_filter_3_0(medium_0(A,C,D),E,F),G,H,I),tau,s1_7_0(generator_0(A,3,9,B),buf_filter_3_0(medium_1(C,A,2,D),E,F),G,H,I)).
trans(s1_7_0(A,buf_filter_3_0(B,filter_5(C,D,E,F,G),H),medium_0(C,I,J),K,L),tau,s1_7_0(A,buf_filter_3_0(B,filter_0(D,C,F,G),H),medium_1(I,C,E,J),K,L)).
trans(s1_7_0(A,B,medium_1(C,D,E,F),consumer_0(C,G,H,I),J),tau,s1_7_0(A,B,medium_0(D,C,F),consumer_6(I),J)) :-
    K  is  G - 1,
    not K > 0,
    E == H.
trans(s1_7_0(A,B,medium_1(C,D,E,F),consumer_0(C,G,H,I),J),tau,s1_7_0(A,B,medium_0(D,C,F),consumer_7(I),J)) :-
    K  is  G - 1,
    not K > 0,
    not E == H.
trans(s1_7_0(A,B,medium_1(C,D,E,F),consumer_0(C,G,H,I),J),tau,s1_7_0(A,B,medium_0(D,C,F),consumer_0(C,K,H,I),J)) :-
    K  is  G - 1,
    K > 0.
trans(s1_7_0(A,B,medium_1(C,D,E,F),consumer_0(C,4,9,G),H),tau,s1_7_0(A,B,medium_0(D,C,F),consumer_0(C,3,9,G),H)).
trans(s1_0(A),nop,s1_7_0(generator_0(B,2,9,end),buf_filter_3_0(medium_0(B,C,end),unit_0(C,D,end),end),medium_0(D,E,end),consumer_0(E,4,9,end),A)) :-
    handlechan([],[B],F),
    handlechan([B],[D],G),
    handlechan([D],[E],H),
    handlechan([B,D],[C],I).
trans(s1_7_0(end,end,end,end,A),nop,A).
trans(s2_9_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M),tau,s2_9_0(A,buf_filter_3_0(medium_0(C,B,E),filter_5(F,B,D,G,H),I),J,K,L,M)) :-
    N  is  D  mod  G,
    not N == 0.
trans(s2_9_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M),tau,s2_9_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,G,H),I),J,K,L,M)) :-
    N  is  D  mod  G,
    N == 0.
trans(s2_9_0(A,buf_filter_3_0(medium_1(B,C,D,E),unit_0(B,F,G),H),I,J,K,L),tau,s2_9_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,D,G),H),I,J,K,L)).
trans(s2_9_0(A,buf_filter_0(B,C,D),E,F,G,H),nop,s2_9_0(A,buf_filter_3_0(medium_0(B,I,end),unit_0(I,C,end),D),E,F,G,H)) :-
    handlechan([B,C],[I],J).
trans(s2_9_0(A,buf_filter_3_0(end,end,B),C,D,E,F),nop,s2_9_0(A,B,C,D,E,F)).
trans(s2_9_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M),tau,s2_9_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_5(G,C,E,H,I),J),K,L,M)) :-
    N  is  E  mod  H,
    not N == 0.
trans(s2_9_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M),tau,s2_9_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,H,I),J),K,L,M)) :-
    N  is  E  mod  H,
    N == 0.
trans(s2_9_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),unit_0(C,G,H),I),J,K,L),tau,s2_9_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,E,H),I),J,K,L)).
trans(s2_9_0(A,B,buf_filter_0(C,D,E),F,G,H),nop,s2_9_0(A,B,buf_filter_3_0(medium_0(C,I,end),unit_0(I,D,end),E),F,G,H)) :-
    handlechan([C,D],[I],J).
trans(s2_9_0(A,B,buf_filter_3_0(end,end,C),D,E,F),nop,s2_9_0(A,B,C,D,E,F)).
trans(s2_9_0(A,B,C,D,consumer_6(E),F),action(finish),s2_9_0(A,B,C,D,E,F)).
trans(s2_9_0(A,B,C,D,consumer_7(E),F),action(error),s2_9_0(A,B,C,D,E,F)).
trans(s2_9_0(A,B,C,D,consumer_6(E),F),action(finish),s2_9_0(A,B,C,D,E,F)).
trans(s2_9_0(A,B,C,D,consumer_7(E),F),action(error),s2_9_0(A,B,C,D,E,F)).
trans(s2_9_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L),tau,s2_9_0(D,buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L)) :-
    M  is  B + 1,
    not B =< C.
trans(s2_9_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L),tau,s2_9_0(generator_0(A,M,C,D),buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L)) :-
    M  is  B + 1,
    B =< C.
trans(s2_9_0(generator_0(A,2,13,B),buf_filter_3_0(medium_0(A,C,D),E,F),G,H,I,J),tau,s2_9_0(generator_0(A,3,13,B),buf_filter_3_0(medium_1(C,A,2,D),E,F),G,H,I,J)).
trans(s2_9_0(A,buf_filter_3_0(B,filter_5(C,D,E,F,G),H),buf_filter_3_0(medium_0(C,I,J),K,L),M,N,O),tau,s2_9_0(A,buf_filter_3_0(B,filter_0(D,C,F,G),H),buf_filter_3_0(medium_1(I,C,E,J),K,L),M,N,O)).
trans(s2_9_0(A,B,buf_filter_3_0(C,filter_5(D,E,F,G,H),I),medium_0(D,J,K),L,M),tau,s2_9_0(A,B,buf_filter_3_0(C,filter_0(E,D,G,H),I),medium_1(J,D,F,K),L,M)).
trans(s2_9_0(A,B,C,medium_1(D,E,F,G),consumer_0(D,H,I,J),K),tau,s2_9_0(A,B,C,medium_0(E,D,G),consumer_6(J),K)) :-
    L  is  H - 1,
    not L > 0,
    F == I.
trans(s2_9_0(A,B,C,medium_1(D,E,F,G),consumer_0(D,H,I,J),K),tau,s2_9_0(A,B,C,medium_0(E,D,G),consumer_7(J),K)) :-
    L  is  H - 1,
    not L > 0,
    not F == I.
trans(s2_9_0(A,B,C,medium_1(D,E,F,G),consumer_0(D,H,I,J),K),tau,s2_9_0(A,B,C,medium_0(E,D,G),consumer_0(D,L,I,J),K)) :-
    L  is  H - 1,
    L > 0.
trans(s2_9_0(A,B,C,medium_1(D,E,F,G),consumer_0(D,4,13,H),I),tau,s2_9_0(A,B,C,medium_0(E,D,G),consumer_0(D,3,13,H),I)).
trans(s2_0(A),nop,s2_9_0(generator_0(B,2,13,end),buf_filter_3_0(medium_0(B,C,end),unit_0(C,D,end),end),buf_filter_3_0(medium_0(D,E,end),unit_0(E,F,end),end),medium_0(F,G,end),consumer_0(G,4,13,end),A)) :-
    handlechan([],[B],H),
    handlechan([B],[D],I),
    handlechan([D],[F],J),
    handlechan([F],[G],K),
    handlechan([B,D],[C],L),
    handlechan([D,F],[E],M).
trans(s2_9_0(end,end,end,end,end,A),nop,A).
trans(s3_11_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M,N),tau,s3_11_0(A,buf_filter_3_0(medium_0(C,B,E),filter_5(F,B,D,G,H),I),J,K,L,M,N)) :-
    O  is  D  mod  G,
    not O == 0.
trans(s3_11_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M,N),tau,s3_11_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,G,H),I),J,K,L,M,N)) :-
    O  is  D  mod  G,
    O == 0.
trans(s3_11_0(A,buf_filter_3_0(medium_1(B,C,D,E),unit_0(B,F,G),H),I,J,K,L,M),tau,s3_11_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,D,G),H),I,J,K,L,M)).
trans(s3_11_0(A,buf_filter_0(B,C,D),E,F,G,H,I),nop,s3_11_0(A,buf_filter_3_0(medium_0(B,J,end),unit_0(J,C,end),D),E,F,G,H,I)) :-
    handlechan([B,C],[J],K).
trans(s3_11_0(A,buf_filter_3_0(end,end,B),C,D,E,F,G),nop,s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M,N),tau,s3_11_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_5(G,C,E,H,I),J),K,L,M,N)) :-
    O  is  E  mod  H,
    not O == 0.
trans(s3_11_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M,N),tau,s3_11_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,H,I),J),K,L,M,N)) :-
    O  is  E  mod  H,
    O == 0.
trans(s3_11_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),unit_0(C,G,H),I),J,K,L,M),tau,s3_11_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,E,H),I),J,K,L,M)).
trans(s3_11_0(A,B,buf_filter_0(C,D,E),F,G,H,I),nop,s3_11_0(A,B,buf_filter_3_0(medium_0(C,J,end),unit_0(J,D,end),E),F,G,H,I)) :-
    handlechan([C,D],[J],K).
trans(s3_11_0(A,B,buf_filter_3_0(end,end,C),D,E,F,G),nop,s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),filter_0(D,H,I,J),K),L,M,N),tau,s3_11_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_5(H,D,F,I,J),K),L,M,N)) :-
    O  is  F  mod  I,
    not O == 0.
trans(s3_11_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),filter_0(D,H,I,J),K),L,M,N),tau,s3_11_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_0(D,H,I,J),K),L,M,N)) :-
    O  is  F  mod  I,
    O == 0.
trans(s3_11_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),unit_0(D,H,I),J),K,L,M),tau,s3_11_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_0(D,H,F,I),J),K,L,M)).
trans(s3_11_0(A,B,C,buf_filter_0(D,E,F),G,H,I),nop,s3_11_0(A,B,C,buf_filter_3_0(medium_0(D,J,end),unit_0(J,E,end),F),G,H,I)) :-
    handlechan([D,E],[J],K).
trans(s3_11_0(A,B,C,buf_filter_3_0(end,end,D),E,F,G),nop,s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(A,B,C,D,E,consumer_6(F),G),action(finish),s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(A,B,C,D,E,consumer_7(F),G),action(error),s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(A,B,C,D,E,consumer_6(F),G),action(finish),s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(A,B,C,D,E,consumer_7(F),G),action(error),s3_11_0(A,B,C,D,E,F,G)).
trans(s3_11_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L,M),tau,s3_11_0(D,buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L,M)) :-
    N  is  B + 1,
    not B =< C.
trans(s3_11_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L,M),tau,s3_11_0(generator_0(A,N,C,D),buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L,M)) :-
    N  is  B + 1,
    B =< C.
trans(s3_11_0(generator_0(A,2,17,B),buf_filter_3_0(medium_0(A,C,D),E,F),G,H,I,J,K),tau,s3_11_0(generator_0(A,3,17,B),buf_filter_3_0(medium_1(C,A,2,D),E,F),G,H,I,J,K)).
trans(s3_11_0(A,buf_filter_3_0(B,filter_5(C,D,E,F,G),H),buf_filter_3_0(medium_0(C,I,J),K,L),M,N,O,P),tau,s3_11_0(A,buf_filter_3_0(B,filter_0(D,C,F,G),H),buf_filter_3_0(medium_1(I,C,E,J),K,L),M,N,O,P)).
trans(s3_11_0(A,B,buf_filter_3_0(C,filter_5(D,E,F,G,H),I),buf_filter_3_0(medium_0(D,J,K),L,M),N,O,P),tau,s3_11_0(A,B,buf_filter_3_0(C,filter_0(E,D,G,H),I),buf_filter_3_0(medium_1(J,D,F,K),L,M),N,O,P)).
trans(s3_11_0(A,B,C,buf_filter_3_0(D,filter_5(E,F,G,H,I),J),medium_0(E,K,L),M,N),tau,s3_11_0(A,B,C,buf_filter_3_0(D,filter_0(F,E,H,I),J),medium_1(K,E,G,L),M,N)).
trans(s3_11_0(A,B,C,D,medium_1(E,F,G,H),consumer_0(E,I,J,K),L),tau,s3_11_0(A,B,C,D,medium_0(F,E,H),consumer_6(K),L)) :-
    M  is  I - 1,
    not M > 0,
    G == J.
trans(s3_11_0(A,B,C,D,medium_1(E,F,G,H),consumer_0(E,I,J,K),L),tau,s3_11_0(A,B,C,D,medium_0(F,E,H),consumer_7(K),L)) :-
    M  is  I - 1,
    not M > 0,
    not G == J.
trans(s3_11_0(A,B,C,D,medium_1(E,F,G,H),consumer_0(E,I,J,K),L),tau,s3_11_0(A,B,C,D,medium_0(F,E,H),consumer_0(E,M,J,K),L)) :-
    M  is  I - 1,
    M > 0.
trans(s3_11_0(A,B,C,D,medium_1(E,F,G,H),consumer_0(E,4,17,I),J),tau,s3_11_0(A,B,C,D,medium_0(F,E,H),consumer_0(E,3,17,I),J)).
trans(s3_0(A),nop,s3_11_0(generator_0(B,2,17,end),buf_filter_3_0(medium_0(B,C,end),unit_0(C,D,end),end),buf_filter_3_0(medium_0(D,E,end),unit_0(E,F,end),end),buf_filter_3_0(medium_0(F,G,end),unit_0(G,H,end),end),medium_0(H,I,end),consumer_0(I,4,17,end),A)) :-
    handlechan([],[B],J),
    handlechan([B],[D],K),
    handlechan([D],[F],L),
    handlechan([F],[H],M),
    handlechan([H],[I],N),
    handlechan([B,D],[C],O),
    handlechan([D,F],[E],P),
    handlechan([F,H],[G],Q).
trans(s3_11_0(end,end,end,end,end,end,A),nop,A).
trans(s4_13_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M,N,O),tau,s4_13_0(A,buf_filter_3_0(medium_0(C,B,E),filter_5(F,B,D,G,H),I),J,K,L,M,N,O)) :-
    P  is  D  mod  G,
    not P == 0.
trans(s4_13_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M,N,O),tau,s4_13_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,G,H),I),J,K,L,M,N,O)) :-
    P  is  D  mod  G,
    P == 0.
trans(s4_13_0(A,buf_filter_3_0(medium_1(B,C,D,E),unit_0(B,F,G),H),I,J,K,L,M,N),tau,s4_13_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,D,G),H),I,J,K,L,M,N)).
trans(s4_13_0(A,buf_filter_0(B,C,D),E,F,G,H,I,J),nop,s4_13_0(A,buf_filter_3_0(medium_0(B,K,end),unit_0(K,C,end),D),E,F,G,H,I,J)) :-
    handlechan([B,C],[K],L).
trans(s4_13_0(A,buf_filter_3_0(end,end,B),C,D,E,F,G,H),nop,s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M,N,O),tau,s4_13_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_5(G,C,E,H,I),J),K,L,M,N,O)) :-
    P  is  E  mod  H,
    not P == 0.
trans(s4_13_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M,N,O),tau,s4_13_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,H,I),J),K,L,M,N,O)) :-
    P  is  E  mod  H,
    P == 0.
trans(s4_13_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),unit_0(C,G,H),I),J,K,L,M,N),tau,s4_13_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,E,H),I),J,K,L,M,N)).
trans(s4_13_0(A,B,buf_filter_0(C,D,E),F,G,H,I,J),nop,s4_13_0(A,B,buf_filter_3_0(medium_0(C,K,end),unit_0(K,D,end),E),F,G,H,I,J)) :-
    handlechan([C,D],[K],L).
trans(s4_13_0(A,B,buf_filter_3_0(end,end,C),D,E,F,G,H),nop,s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),filter_0(D,H,I,J),K),L,M,N,O),tau,s4_13_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_5(H,D,F,I,J),K),L,M,N,O)) :-
    P  is  F  mod  I,
    not P == 0.
trans(s4_13_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),filter_0(D,H,I,J),K),L,M,N,O),tau,s4_13_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_0(D,H,I,J),K),L,M,N,O)) :-
    P  is  F  mod  I,
    P == 0.
trans(s4_13_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),unit_0(D,H,I),J),K,L,M,N),tau,s4_13_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_0(D,H,F,I),J),K,L,M,N)).
trans(s4_13_0(A,B,C,buf_filter_0(D,E,F),G,H,I,J),nop,s4_13_0(A,B,C,buf_filter_3_0(medium_0(D,K,end),unit_0(K,E,end),F),G,H,I,J)) :-
    handlechan([D,E],[K],L).
trans(s4_13_0(A,B,C,buf_filter_3_0(end,end,D),E,F,G,H),nop,s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,C,D,buf_filter_3_0(medium_1(E,F,G,H),filter_0(E,I,J,K),L),M,N,O),tau,s4_13_0(A,B,C,D,buf_filter_3_0(medium_0(F,E,H),filter_5(I,E,G,J,K),L),M,N,O)) :-
    P  is  G  mod  J,
    not P == 0.
trans(s4_13_0(A,B,C,D,buf_filter_3_0(medium_1(E,F,G,H),filter_0(E,I,J,K),L),M,N,O),tau,s4_13_0(A,B,C,D,buf_filter_3_0(medium_0(F,E,H),filter_0(E,I,J,K),L),M,N,O)) :-
    P  is  G  mod  J,
    P == 0.
trans(s4_13_0(A,B,C,D,buf_filter_3_0(medium_1(E,F,G,H),unit_0(E,I,J),K),L,M,N),tau,s4_13_0(A,B,C,D,buf_filter_3_0(medium_0(F,E,H),filter_0(E,I,G,J),K),L,M,N)).
trans(s4_13_0(A,B,C,D,buf_filter_0(E,F,G),H,I,J),nop,s4_13_0(A,B,C,D,buf_filter_3_0(medium_0(E,K,end),unit_0(K,F,end),G),H,I,J)) :-
    handlechan([E,F],[K],L).
trans(s4_13_0(A,B,C,D,buf_filter_3_0(end,end,E),F,G,H),nop,s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,C,D,E,F,consumer_6(G),H),action(finish),s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,C,D,E,F,consumer_7(G),H),action(error),s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,C,D,E,F,consumer_6(G),H),action(finish),s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(A,B,C,D,E,F,consumer_7(G),H),action(error),s4_13_0(A,B,C,D,E,F,G,H)).
trans(s4_13_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L,M,N),tau,s4_13_0(D,buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L,M,N)) :-
    O  is  B + 1,
    not B =< C.
trans(s4_13_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L,M,N),tau,s4_13_0(generator_0(A,O,C,D),buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L,M,N)) :-
    O  is  B + 1,
    B =< C.
trans(s4_13_0(generator_0(A,2,19,B),buf_filter_3_0(medium_0(A,C,D),E,F),G,H,I,J,K,L),tau,s4_13_0(generator_0(A,3,19,B),buf_filter_3_0(medium_1(C,A,2,D),E,F),G,H,I,J,K,L)).
trans(s4_13_0(A,buf_filter_3_0(B,filter_5(C,D,E,F,G),H),buf_filter_3_0(medium_0(C,I,J),K,L),M,N,O,P,Q),tau,s4_13_0(A,buf_filter_3_0(B,filter_0(D,C,F,G),H),buf_filter_3_0(medium_1(I,C,E,J),K,L),M,N,O,P,Q)).
trans(s4_13_0(A,B,buf_filter_3_0(C,filter_5(D,E,F,G,H),I),buf_filter_3_0(medium_0(D,J,K),L,M),N,O,P,Q),tau,s4_13_0(A,B,buf_filter_3_0(C,filter_0(E,D,G,H),I),buf_filter_3_0(medium_1(J,D,F,K),L,M),N,O,P,Q)).
trans(s4_13_0(A,B,C,buf_filter_3_0(D,filter_5(E,F,G,H,I),J),buf_filter_3_0(medium_0(E,K,L),M,N),O,P,Q),tau,s4_13_0(A,B,C,buf_filter_3_0(D,filter_0(F,E,H,I),J),buf_filter_3_0(medium_1(K,E,G,L),M,N),O,P,Q)).
trans(s4_13_0(A,B,C,D,buf_filter_3_0(E,filter_5(F,G,H,I,J),K),medium_0(F,L,M),N,O),tau,s4_13_0(A,B,C,D,buf_filter_3_0(E,filter_0(G,F,I,J),K),medium_1(L,F,H,M),N,O)).
trans(s4_13_0(A,B,C,D,E,medium_1(F,G,H,I),consumer_0(F,J,K,L),M),tau,s4_13_0(A,B,C,D,E,medium_0(G,F,I),consumer_6(L),M)) :-
    N  is  J - 1,
    not N > 0,
    H == K.
trans(s4_13_0(A,B,C,D,E,medium_1(F,G,H,I),consumer_0(F,J,K,L),M),tau,s4_13_0(A,B,C,D,E,medium_0(G,F,I),consumer_7(L),M)) :-
    N  is  J - 1,
    not N > 0,
    not H == K.
trans(s4_13_0(A,B,C,D,E,medium_1(F,G,H,I),consumer_0(F,J,K,L),M),tau,s4_13_0(A,B,C,D,E,medium_0(G,F,I),consumer_0(F,N,K,L),M)) :-
    N  is  J - 1,
    N > 0.
trans(s4_13_0(A,B,C,D,E,medium_1(F,G,H,I),consumer_0(F,4,19,J),K),tau,s4_13_0(A,B,C,D,E,medium_0(G,F,I),consumer_0(F,3,19,J),K)).
trans(s4_0(A),nop,s4_13_0(generator_0(B,2,19,end),buf_filter_3_0(medium_0(B,C,end),unit_0(C,D,end),end),buf_filter_3_0(medium_0(D,E,end),unit_0(E,F,end),end),buf_filter_3_0(medium_0(F,G,end),unit_0(G,H,end),end),buf_filter_3_0(medium_0(H,I,end),unit_0(I,J,end),end),medium_0(J,K,end),consumer_0(K,4,19,end),A)) :-
    handlechan([],[B],L),
    handlechan([B],[D],M),
    handlechan([D],[F],N),
    handlechan([F],[H],O),
    handlechan([H],[J],P),
    handlechan([J],[K],Q),
    handlechan([B,D],[C],R),
    handlechan([D,F],[E],S),
    handlechan([F,H],[G],T),
    handlechan([H,J],[I],U).
trans(s4_13_0(end,end,end,end,end,end,end,A),nop,A).
trans(s5_15_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M,N,O,P),tau,s5_15_0(A,buf_filter_3_0(medium_0(C,B,E),filter_5(F,B,D,G,H),I),J,K,L,M,N,O,P)) :-
    Q  is  D  mod  G,
    not Q == 0.
trans(s5_15_0(A,buf_filter_3_0(medium_1(B,C,D,E),filter_0(B,F,G,H),I),J,K,L,M,N,O,P),tau,s5_15_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,G,H),I),J,K,L,M,N,O,P)) :-
    Q  is  D  mod  G,
    Q == 0.
trans(s5_15_0(A,buf_filter_3_0(medium_1(B,C,D,E),unit_0(B,F,G),H),I,J,K,L,M,N,O),tau,s5_15_0(A,buf_filter_3_0(medium_0(C,B,E),filter_0(B,F,D,G),H),I,J,K,L,M,N,O)).
trans(s5_15_0(A,buf_filter_0(B,C,D),E,F,G,H,I,J,K),nop,s5_15_0(A,buf_filter_3_0(medium_0(B,L,end),unit_0(L,C,end),D),E,F,G,H,I,J,K)) :-
    handlechan([B,C],[L],M).
trans(s5_15_0(A,buf_filter_3_0(end,end,B),C,D,E,F,G,H,I),nop,s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M,N,O,P),tau,s5_15_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_5(G,C,E,H,I),J),K,L,M,N,O,P)) :-
    Q  is  E  mod  H,
    not Q == 0.
trans(s5_15_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),filter_0(C,G,H,I),J),K,L,M,N,O,P),tau,s5_15_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,H,I),J),K,L,M,N,O,P)) :-
    Q  is  E  mod  H,
    Q == 0.
trans(s5_15_0(A,B,buf_filter_3_0(medium_1(C,D,E,F),unit_0(C,G,H),I),J,K,L,M,N,O),tau,s5_15_0(A,B,buf_filter_3_0(medium_0(D,C,F),filter_0(C,G,E,H),I),J,K,L,M,N,O)).
trans(s5_15_0(A,B,buf_filter_0(C,D,E),F,G,H,I,J,K),nop,s5_15_0(A,B,buf_filter_3_0(medium_0(C,L,end),unit_0(L,D,end),E),F,G,H,I,J,K)) :-
    handlechan([C,D],[L],M).
trans(s5_15_0(A,B,buf_filter_3_0(end,end,C),D,E,F,G,H,I),nop,s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),filter_0(D,H,I,J),K),L,M,N,O,P),tau,s5_15_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_5(H,D,F,I,J),K),L,M,N,O,P)) :-
    Q  is  F  mod  I,
    not Q == 0.
trans(s5_15_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),filter_0(D,H,I,J),K),L,M,N,O,P),tau,s5_15_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_0(D,H,I,J),K),L,M,N,O,P)) :-
    Q  is  F  mod  I,
    Q == 0.
trans(s5_15_0(A,B,C,buf_filter_3_0(medium_1(D,E,F,G),unit_0(D,H,I),J),K,L,M,N,O),tau,s5_15_0(A,B,C,buf_filter_3_0(medium_0(E,D,G),filter_0(D,H,F,I),J),K,L,M,N,O)).
trans(s5_15_0(A,B,C,buf_filter_0(D,E,F),G,H,I,J,K),nop,s5_15_0(A,B,C,buf_filter_3_0(medium_0(D,L,end),unit_0(L,E,end),F),G,H,I,J,K)) :-
    handlechan([D,E],[L],M).
trans(s5_15_0(A,B,C,buf_filter_3_0(end,end,D),E,F,G,H,I),nop,s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,D,buf_filter_3_0(medium_1(E,F,G,H),filter_0(E,I,J,K),L),M,N,O,P),tau,s5_15_0(A,B,C,D,buf_filter_3_0(medium_0(F,E,H),filter_5(I,E,G,J,K),L),M,N,O,P)) :-
    Q  is  G  mod  J,
    not Q == 0.
trans(s5_15_0(A,B,C,D,buf_filter_3_0(medium_1(E,F,G,H),filter_0(E,I,J,K),L),M,N,O,P),tau,s5_15_0(A,B,C,D,buf_filter_3_0(medium_0(F,E,H),filter_0(E,I,J,K),L),M,N,O,P)) :-
    Q  is  G  mod  J,
    Q == 0.
trans(s5_15_0(A,B,C,D,buf_filter_3_0(medium_1(E,F,G,H),unit_0(E,I,J),K),L,M,N,O),tau,s5_15_0(A,B,C,D,buf_filter_3_0(medium_0(F,E,H),filter_0(E,I,G,J),K),L,M,N,O)).
trans(s5_15_0(A,B,C,D,buf_filter_0(E,F,G),H,I,J,K),nop,s5_15_0(A,B,C,D,buf_filter_3_0(medium_0(E,L,end),unit_0(L,F,end),G),H,I,J,K)) :-
    handlechan([E,F],[L],M).
trans(s5_15_0(A,B,C,D,buf_filter_3_0(end,end,E),F,G,H,I),nop,s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_1(F,G,H,I),filter_0(F,J,K,L),M),N,O,P),tau,s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_0(G,F,I),filter_5(J,F,H,K,L),M),N,O,P)) :-
    Q  is  H  mod  K,
    not Q == 0.
trans(s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_1(F,G,H,I),filter_0(F,J,K,L),M),N,O,P),tau,s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_0(G,F,I),filter_0(F,J,K,L),M),N,O,P)) :-
    Q  is  H  mod  K,
    Q == 0.
trans(s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_1(F,G,H,I),unit_0(F,J,K),L),M,N,O),tau,s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_0(G,F,I),filter_0(F,J,H,K),L),M,N,O)).
trans(s5_15_0(A,B,C,D,E,buf_filter_0(F,G,H),I,J,K),nop,s5_15_0(A,B,C,D,E,buf_filter_3_0(medium_0(F,L,end),unit_0(L,G,end),H),I,J,K)) :-
    handlechan([F,G],[L],M).
trans(s5_15_0(A,B,C,D,E,buf_filter_3_0(end,end,F),G,H,I),nop,s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,D,E,F,G,consumer_6(H),I),action(finish),s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,D,E,F,G,consumer_7(H),I),action(error),s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,D,E,F,G,consumer_6(H),I),action(finish),s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(A,B,C,D,E,F,G,consumer_7(H),I),action(error),s5_15_0(A,B,C,D,E,F,G,H,I)).
trans(s5_15_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L,M,N,O),tau,s5_15_0(D,buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L,M,N,O)) :-
    P  is  B + 1,
    not B =< C.
trans(s5_15_0(generator_0(A,B,C,D),buf_filter_3_0(medium_0(A,E,F),G,H),I,J,K,L,M,N,O),tau,s5_15_0(generator_0(A,P,C,D),buf_filter_3_0(medium_1(E,A,B,F),G,H),I,J,K,L,M,N,O)) :-
    P  is  B + 1,
    B =< C.
trans(s5_15_0(generator_0(A,2,23,B),buf_filter_3_0(medium_0(A,C,D),E,F),G,H,I,J,K,L,M),tau,s5_15_0(generator_0(A,3,23,B),buf_filter_3_0(medium_1(C,A,2,D),E,F),G,H,I,J,K,L,M)).
trans(s5_15_0(A,buf_filter_3_0(B,filter_5(C,D,E,F,G),H),buf_filter_3_0(medium_0(C,I,J),K,L),M,N,O,P,Q,R),tau,s5_15_0(A,buf_filter_3_0(B,filter_0(D,C,F,G),H),buf_filter_3_0(medium_1(I,C,E,J),K,L),M,N,O,P,Q,R)).
trans(s5_15_0(A,B,buf_filter_3_0(C,filter_5(D,E,F,G,H),I),buf_filter_3_0(medium_0(D,J,K),L,M),N,O,P,Q,R),tau,s5_15_0(A,B,buf_filter_3_0(C,filter_0(E,D,G,H),I),buf_filter_3_0(medium_1(J,D,F,K),L,M),N,O,P,Q,R)).
trans(s5_15_0(A,B,C,buf_filter_3_0(D,filter_5(E,F,G,H,I),J),buf_filter_3_0(medium_0(E,K,L),M,N),O,P,Q,R),tau,s5_15_0(A,B,C,buf_filter_3_0(D,filter_0(F,E,H,I),J),buf_filter_3_0(medium_1(K,E,G,L),M,N),O,P,Q,R)).
trans(s5_15_0(A,B,C,D,buf_filter_3_0(E,filter_5(F,G,H,I,J),K),buf_filter_3_0(medium_0(F,L,M),N,O),P,Q,R),tau,s5_15_0(A,B,C,D,buf_filter_3_0(E,filter_0(G,F,I,J),K),buf_filter_3_0(medium_1(L,F,H,M),N,O),P,Q,R)).
trans(s5_15_0(A,B,C,D,E,buf_filter_3_0(F,filter_5(G,H,I,J,K),L),medium_0(G,M,N),O,P),tau,s5_15_0(A,B,C,D,E,buf_filter_3_0(F,filter_0(H,G,J,K),L),medium_1(M,G,I,N),O,P)).
trans(s5_15_0(A,B,C,D,E,F,medium_1(G,H,I,J),consumer_0(G,K,L,M),N),tau,s5_15_0(A,B,C,D,E,F,medium_0(H,G,J),consumer_6(M),N)) :-
    O  is  K - 1,
    not O > 0,
    I == L.
trans(s5_15_0(A,B,C,D,E,F,medium_1(G,H,I,J),consumer_0(G,K,L,M),N),tau,s5_15_0(A,B,C,D,E,F,medium_0(H,G,J),consumer_7(M),N)) :-
    O  is  K - 1,
    not O > 0,
    not I == L.
trans(s5_15_0(A,B,C,D,E,F,medium_1(G,H,I,J),consumer_0(G,K,L,M),N),tau,s5_15_0(A,B,C,D,E,F,medium_0(H,G,J),consumer_0(G,O,L,M),N)) :-
    O  is  K - 1,
    O > 0.
trans(s5_15_0(A,B,C,D,E,F,medium_1(G,H,I,J),consumer_0(G,4,23,K),L),tau,s5_15_0(A,B,C,D,E,F,medium_0(H,G,J),consumer_0(G,3,23,K),L)).
trans(s5_0(A),nop,s5_15_0(generator_0(B,2,23,end),buf_filter_3_0(medium_0(B,C,end),unit_0(C,D,end),end),buf_filter_3_0(medium_0(D,E,end),unit_0(E,F,end),end),buf_filter_3_0(medium_0(F,G,end),unit_0(G,H,end),end),buf_filter_3_0(medium_0(H,I,end),unit_0(I,J,end),end),buf_filter_3_0(medium_0(J,K,end),unit_0(K,L,end),end),medium_0(L,M,end),consumer_0(M,4,23,end),A)) :-
    handlechan([],[B],N),
    handlechan([B],[D],O),
    handlechan([D],[F],P),
    handlechan([F],[H],Q),
    handlechan([H],[J],R),
    handlechan([J],[L],S),
    handlechan([L],[M],T),
    handlechan([B,D],[C],U),
    handlechan([D,F],[E],V),
    handlechan([F,H],[G],W),
    handlechan([H,J],[I],X),
    handlechan([J,L],[K],Y).
trans(s5_15_0(end,end,end,end,end,end,end,end,A),nop,A).

partrans(A,B,C,D,E,F) :-
    ((var(B)
      ;
      B == tau
     )
     -> (B = G,
         F = D,
         (G = action(H)
          ;
          (G = nop
           ;
           G = tau
          )
         ),
         trans(C,G,E)
         ;
         G = out(I,J),
         trans(C,G,E),
         (I > A
          -> B = tau,
             trans(D,in(I,J),F)
          ;  (B = G,
              F = D
              ;
              B = tau,
              trans(D,in(I,J),F)
             )
         )
        )
     ;  F = D,
        trans(C,B,E),
        ((B = out(I,K)
          ;
          B = in(I,L)
         )
         -> I =< A
         ;  true
        )
    ).

handlechan(A,B,C) :-
    maxchan(A,C),
    enumlocal(B,C).

maxchan(A,B) :-
    max(A,0,B).

max([],A,A).
max([A|B],C,D) :-
    (A > C
     -> E = A
     ;  E = C
    ),
    max(B,E,D).

enumlocal([],A).
enumlocal([A|B],C) :-
    A  is  C + 1,
    enumlocal(B,A).

startstate(generator(A,B,C),generator_0(A,B,C,end)).
startstate(unit(A,B),unit_0(A,B,end)).
startstate(filter(A,B,C),filter_0(A,B,C,end)).
startstate(consumer(A,B,C),consumer_0(A,B,C,end)).
startstate(medium(A,B),medium_0(A,B,end)).
startstate(buf_filter(A,B),buf_filter_0(A,B,end)).
startstate(units(A,B,C),units_0(A,B,C,end)).
startstate(sieve(A,B,C),sieve_0(A,B,C,end)).
startstate(s1,s1_0(end)).
startstate(s2,s2_0(end)).
startstate(s3,s3_0(end)).
startstate(s4,s4_0(end)).
startstate(s5,s5_0(end)).

fDef(ae_finish,fAnd(fDiamSetMinus([],tt),fBoxMinus(action(finish),form(ae_finish)))).
fDef(ee_error,fOr(fDiam(action(error),tt),fDiamSetMinus([],form(ee_error)))).
