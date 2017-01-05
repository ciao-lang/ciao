message(first(Id)) :- typeof(Id, integer).
message(second(Id)) :- typeof(Id, integer).

pdef([medium(Read,Write,Buf),[17,1,17,24]],
	[choice([pref([in(Read,Msg),[18,2,18,11]],[medium(Read,Write,[Msg | Buf]),[19,2,19,31]]),[18,2,19,31]],[pref([(Buf \== []),[21,2,21,11]],[pref([strip_from_end(Buf,Msg,RNBuf),[22,2,22,32]],[pref([out(Write,Msg),[23,2,23,12]],[medium(Read,Write,RNBuf),[24,2,24,27]]),[23,2,24,27]]),[22,2,24,27]]),[21,2,24,27]]),[18,2,24,27]],
	[(RNBuf,'RNBuf'), (Msg,'Msg'), (Buf,'Buf'), (Write,'Write'), (Read,'Read')]).


strip_from_end([X], X, []).
strip_from_end([X,Y|Ys], Z, [X|Zs]) :- strip_from_end([Y|Ys], Z, Zs).

pdef([node(Right,Left,Id,N),[35,1,35,24]],
	[pref([out(Right,first(Id)),[36,2,36,18]],[nodeActive(Right,Left,N,Id,0),[37,2,37,34]]),[36,2,37,34]],
	[(N,'N'), (Id,'Id'), (Left,'Left'), (Right,'Right')]).

pdef([nodeActive(Right,Left,N,Maxi,Nbr),[39,1,39,37]],
	[choice([pref([in(Left,first(NId)),[40,2,40,18]],[if([(NId \== Maxi),[41,5,41,5]],[pref([out(Right,second(NId)),[42,12,42,30]],[nodeActive(Right,Left,N,Maxi,NId),[42,33,42,69]]),[42,11,42,71]],[if([(Maxi is (N - 1)),[43,14,43,14]],[pref([action(leader),[44,10,44,23]],[nodeActive(Right,Left,N,Maxi,NId),[44,26,44,62]]),[44,8,44,64]],[action(fail),[45,8,45,23]]),[43,11,45,23]]),[41,2,45,23]]),[40,2,45,23]],[pref([in(Left,second(NId)),[47,2,47,19]],[if([((Nbr > NId) , (Nbr > Maxi)),[48,5,48,5]],[pref([out(Right,first(Nbr)),[49,13,49,30]],[nodeActive(Right,Left,N,Nbr,Nbr),[49,33,49,68]]),[49,11,49,70]],[nodeInactive(Right,Left),[50,11,50,35]]),[48,2,50,35]]),[47,2,50,35]]),[40,2,50,35]],
	[(NId,'NId'), (Nbr,'Nbr'), (Maxi,'Maxi'), (N,'N'), (Left,'Left'), (Right,'Right')]).

pdef([nodeInactive(Right,Left),[52,1,52,25]],
	[pref([in(Left,Msg),[53,2,53,11]],[pref([out(Right,Msg),[54,2,54,12]],[nodeInactive(Right,Left),[55,2,55,26]]),[54,2,55,26]]),[53,2,55,26]],
	[(Msg,'Msg'), (Left,'Left'), (Right,'Right')]).

pdef([systemLeader(M),[61,1,61,15]],
	[par([one_node(Left,Right,0,M),[62,4,62,30]],[chain(Right,Left,1,M),[63,4,63,27]]),[62,4,63,27]],
	[(Right,'Right'), (Left,'Left'), (M,'M')]).

pdef([one_node(Right,Left,I,M),[65,1,65,27]],
	[par([node(Temp,Left,I,M),[66,4,66,25]],[medium(Temp,Right,[]),[67,4,67,26]]),[66,4,67,26]],
	[(Temp,'Temp'), (M,'M'), (I,'I'), (Left,'Left'), (Right,'Right')]).

pdef([chain(Right,Left,I,M),[69,1,69,24]],
	[if([(I is (M - 1)),[70,5,70,5]],[one_node(Right,Left,I,M),[71,8,71,34]],[par([one_node(Temp,Left,I,M),[72,11,72,36]],[pref([(I1 is (I + 1)),[73,8,73,16]],[chain(Right,Temp,I1,M),[73,19,73,43]]),[73,6,73,44]]),[72,8,74,8]]),[70,2,74,8]],
	[(I1,'I1'), (Temp,'Temp'), (M,'M'), (I,'I'), (Left,'Left'), (Right,'Right')]).

pdef([l2,[77,1,77,2]],
	[par([one_node(Right,Left,0,2),[78,4,78,30]],[one_node(Left,Right,1,2),[79,4,79,30]]),[78,4,79,30]],
	[(Left,'Left'), (Right,'Right')]).

pdef([l21,[81,1,81,3]],
	[par([node(T1,Left,0,2),[82,4,82,23]],[par([medium(T1,Right,[]),[83,4,83,24]],[par([node(T2,Right,1,2),[84,4,84,24]],[medium(T2,Left,[]),[85,4,85,23]]),[84,4,85,23]]),[83,4,85,23]]),[82,4,85,23]],
	[(T2,'T2'), (Right,'Right'), (Left,'Left'), (T1,'T1')]).

pdef([l22,[87,1,87,3]],
	[par([node(T1,Left,0,2),[88,4,88,23]],[par([node(T2,Right,1,2),[89,4,89,24]],[par([medium(T1,Right,[]),[90,4,90,24]],[medium(T2,Left,[]),[91,4,91,23]]),[90,4,91,23]]),[89,4,91,23]]),[88,4,91,23]],
	[(Right,'Right'), (T2,'T2'), (Left,'Left'), (T1,'T1')]).

fdef(ae_leader, and(diamAll(tt),boxMinus(action(leader),form(ae_leader)))).
fneg(ae_leader, neg_form(ae_leader)).
fdef(ee_fail, or(diam(action(fail),tt),diamAll(form(ee_fail)))).
fneg(ee_fail, neg_form(ee_fail)).
fdef(one_leader, and(boxMinus(action(leader),form(one_leader)),box(action(leader),form(no_leader)))).
fneg(one_leader, neg_form(one_leader)).
fdef(no_leader, neg_form(neg_no_leader)).
fdef(neg_no_leader, or(diam(action(leader),tt),diamMinusSet([],neg(no_leader)))).
fneg(no_leader, form(neg_no_leader)).
fdef(aa_true, neg_form(neg_aa_true)).
fdef(neg_aa_true, or(diam(action(fail),tt),diamMinusSet([],neg(aa_true)))).
fneg(aa_true, form(neg_aa_true)).
fdef(aa_leader, neg_form(neg_aa_leader)).
fdef(neg_aa_leader, or(boxAll(ff),diamMinus(action(leader),neg(aa_leader)))).
fneg(aa_leader, form(neg_aa_leader)).
fdef(deadlock, or(boxMinusSet([],ff),diamAll(form(deadlock)))).
fneg(deadlock, neg_form(deadlock)).

% ----------------------------------------------------

trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(medium_0(A,B,C,D),in(A,E),medium_0(A,B,[E|C],D)).
trans(medium_0(A,B,C,D),out(B,E),medium_0(A,B,F,D)) :-
    C \== [],
    strip_from_end(C,E,F).
trans(node_0(A,B,C,D,E),out(A,first(C)),nodeActive_0(A,B,D,C,0,E)).
trans(nodeActive_0(A,B,C,D,E,F),in(B,first(G)),nodeActive_2(A,C,B,G,D,F)) :-
    G \== D.
trans(nodeActive_0(A,B,C,D,E,F),in(B,first(G)),nodeActive_5(A,B,G,D,C,F)) :-
    not G \== D,
    D  is  C - 1.
trans(nodeActive_0(A,B,C,D,E,F),in(B,first(G)),nodeActive_6(F)) :-
    not G \== D,
    not D  is  C - 1.
trans(nodeActive_0(A,B,C,D,E,F),in(B,second(G)),nodeActive_9(A,C,B,E,F)) :-
    E > G,
    E > D.
trans(nodeActive_2(A,B,C,D,E,F),out(A,second(D)),nodeActive_0(A,C,B,E,D,F)).
trans(nodeActive_5(A,B,C,D,E,F),action(leader),nodeActive_0(A,B,E,D,C,F)).
trans(nodeActive_6(A),action(fail),A).
trans(nodeActive_9(A,B,C,D,E),out(A,first(D)),nodeActive_0(A,C,B,D,D,E)).
trans(nodeActive_0(A,B,C,D,E,F),in(B,second(G)),nodeInactive_0(A,B,F)) :-
    not (E > G  ','  E > D).
trans(nodeInactive_0(A,B,C),in(B,D),nodeInactive_1(A,B,D,C)).
trans(nodeInactive_1(A,B,C,D),out(A,C),nodeInactive_0(A,B,D)).
trans(one_node_3_0(nodeInactive_0(A,B,C),D,E),in(B,F),one_node_3_0(nodeInactive_1(A,B,F,C),D,E)).
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,first(I)),one_node_3_0(nodeActive_2(A,C,B,I,D,F),G,H)) :-
    I \== D.
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,first(I)),one_node_3_0(nodeActive_5(A,B,I,D,C,F),G,H)) :-
    not I \== D,
    D  is  C - 1.
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,first(I)),one_node_3_0(nodeActive_6(F),G,H)) :-
    not I \== D,
    not D  is  C - 1.
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,second(I)),one_node_3_0(nodeActive_9(A,C,B,E,F),G,H)) :-
    E > I,
    E > D.
trans(one_node_3_0(nodeActive_5(A,B,C,D,E,F),G,H),action(leader),one_node_3_0(nodeActive_0(A,B,E,D,C,F),G,H)).
trans(one_node_3_0(nodeActive_6(A),B,C),action(fail),one_node_3_0(A,B,C)).
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,second(I)),one_node_3_0(nodeInactive_0(A,B,F),G,H)) :-
    not (E > I  ','  E > D).
trans(one_node_3_0(nodeActive_0(A,B,C,D,D,E),F,G),in(B,first(H)),one_node_3_0(nodeActive_2(A,C,B,H,D,E),F,G)) :-
    H \== D.
trans(one_node_3_0(nodeActive_0(A,B,C,D,D,E),F,G),in(B,first(H)),one_node_3_0(nodeActive_5(A,B,H,D,C,E),F,G)) :-
    not H \== D,
    D  is  C - 1.
trans(one_node_3_0(nodeActive_0(A,B,C,D,D,E),F,G),in(B,first(H)),one_node_3_0(nodeActive_6(E),F,G)) :-
    not H \== D,
    not D  is  C - 1.
trans(one_node_3_0(nodeActive_0(A,B,C,D,D,E),F,G),in(B,second(H)),one_node_3_0(nodeActive_9(A,C,B,D,E),F,G)) :-
    D > H,
    D > D.
trans(one_node_3_0(nodeActive_5(A,B,C,D,E,F),G,H),action(leader),one_node_3_0(nodeActive_0(A,B,E,D,C,F),G,H)).
trans(one_node_3_0(nodeActive_6(A),B,C),action(fail),one_node_3_0(A,B,C)).
trans(one_node_3_0(nodeActive_0(A,B,C,D,D,E),F,G),in(B,second(H)),one_node_3_0(nodeInactive_0(A,B,E),F,G)) :-
    not (D > H  ','  D > D).
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,first(I)),one_node_3_0(nodeActive_2(A,C,B,I,D,F),G,H)) :-
    I \== D.
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,first(I)),one_node_3_0(nodeActive_5(A,B,I,D,C,F),G,H)) :-
    not I \== D,
    D  is  C - 1.
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,first(I)),one_node_3_0(nodeActive_6(F),G,H)) :-
    not I \== D,
    not D  is  C - 1.
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,second(I)),one_node_3_0(nodeActive_9(A,C,B,E,F),G,H)) :-
    E > I,
    E > D.
trans(one_node_3_0(nodeActive_5(A,B,C,D,E,F),G,H),action(leader),one_node_3_0(nodeActive_0(A,B,E,D,C,F),G,H)).
trans(one_node_3_0(nodeActive_6(A),B,C),action(fail),one_node_3_0(A,B,C)).
trans(one_node_3_0(nodeActive_0(A,B,C,D,E,F),G,H),in(B,second(I)),one_node_3_0(nodeInactive_0(A,B,F),G,H)) :-
    not (E > I  ','  E > D).
trans(one_node_3_0(A,medium_0(B,C,D,E),F),out(C,G),one_node_3_0(A,medium_0(B,C,H,E),F)) :-
    D \== [],
    strip_from_end(D,G,H).
trans(one_node_3_0(nodeInactive_1(A,B,C,D),medium_0(A,E,F,G),H),tau,one_node_3_0(nodeInactive_0(A,B,D),medium_0(A,E,[C|F],G),H)).
trans(one_node_3_0(nodeInactive_1(A,B,C,D),medium_0(A,E,[],F),G),tau,one_node_3_0(nodeInactive_0(A,B,D),medium_0(A,E,[C],F),G)).
trans(one_node_3_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,H,I),J),tau,one_node_3_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)|H],I),J)).
trans(one_node_3_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,[],H),I),tau,one_node_3_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)],H),I)).
trans(one_node_3_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,G,H),I),tau,one_node_3_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)|G],H),I)).
trans(one_node_3_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,[],G),H),tau,one_node_3_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)],G),H)).
trans(one_node_3_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,H,I),J),tau,one_node_3_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)|H],I),J)).
trans(one_node_3_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,[],H),I),tau,one_node_3_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)],H),I)).
trans(one_node_3_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,G,H),I),tau,one_node_3_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)|G],H),I)).
trans(one_node_3_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,[],G),H),tau,one_node_3_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)],G),H)).
trans(one_node_3_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,H,I),J),tau,one_node_3_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)|H],I),J)).
trans(one_node_3_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,[],H),I),tau,one_node_3_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)],H),I)).
trans(one_node_3_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,G,H),I),tau,one_node_3_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)|G],H),I)).
trans(one_node_3_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,[],G),H),tau,one_node_3_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)],G),H)).
trans(one_node_3_0(node_0(A,B,C,D,E),medium_0(A,F,G,H),I),tau,one_node_3_0(nodeActive_0(A,B,D,C,0,E),medium_0(A,F,[first(C)|G],H),I)).
trans(one_node_3_0(node_0(A,B,C,D,E),medium_0(A,F,[],G),H),tau,one_node_3_0(nodeActive_0(A,B,D,C,0,E),medium_0(A,F,[first(C)],G),H)).
trans(one_node_0(A,B,C,D,E),nop,one_node_3_0(node_0(F,B,C,D,end),medium_0(F,A,[],end),E)) :-
    handlechan([A,B],[F],G).
trans(one_node_3_0(end,end,A),nop,A).
trans(chain_5(A,B,C,D,E),nop,one_node_0(D,A,B,C,E)).
trans(chain_6(A,B,C,D,E),nop,chain_0(A,D,F,C,E)) :-
    F  is  B + 1.
trans(chain_0(A,B,C,D,E),nop,par(F,one_node_0(G,B,C,D,end),chain_0(A,G,H,D,end),E)) :-
    not C  is  D - 1,
    handlechan([A,B],[G],F),
    H  is  C + 1.
trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(chain_0(A,B,C,D,E),nop,one_node_0(A,B,C,D,E)) :-
    C  is  D - 1.
trans(systemLeader_3(A,B,C,D),nop,one_node_0(B,C,0,A,D)).
trans(systemLeader_4(A,B,C,D),nop,chain_0(C,B,1,A,D)).
trans(systemLeader_0(A,B),nop,par(C,one_node_0(D,E,0,A,end),chain_0(E,D,1,A,end),B)) :-
    handlechan([],[D,E],C).
trans(par(A,end,end,B),nop,B).
trans(par(A,B,C,D),E,par(A,F,G,D)) :-
    (partrans(A,E,B,C,F,G)
     ;
     partrans(A,E,C,B,G,F)
    ).
trans(l2_3_0(one_node_3_0(nodeActive_5(A,B,C,D,2,E),F,G),H,I),action(leader),l2_3_0(one_node_3_0(nodeActive_0(A,B,2,D,C,E),F,G),H,I)).
trans(l2_3_0(one_node_3_0(nodeActive_6(A),B,C),D,E),action(fail),l2_3_0(one_node_3_0(A,B,C),D,E)).
trans(l2_3_0(one_node_3_0(nodeActive_5(A,B,C,D,2,E),F,G),H,I),action(leader),l2_3_0(one_node_3_0(nodeActive_0(A,B,2,D,C,E),F,G),H,I)).
trans(l2_3_0(one_node_3_0(nodeActive_6(A),B,C),D,E),action(fail),l2_3_0(one_node_3_0(A,B,C),D,E)).
trans(l2_3_0(one_node_3_0(nodeActive_5(A,B,C,0,2,D),E,F),G,H),action(leader),l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),E,F),G,H)).
trans(l2_3_0(one_node_3_0(nodeActive_6(A),B,C),D,E),action(fail),l2_3_0(one_node_3_0(A,B,C),D,E)).
trans(l2_3_0(one_node_3_0(nodeInactive_1(A,B,C,D),medium_0(A,E,F,G),H),I,J),tau,l2_3_0(one_node_3_0(nodeInactive_0(A,B,D),medium_0(A,E,[C|F],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeInactive_1(A,B,C,D),medium_0(A,E,[],F),G),H,I),tau,l2_3_0(one_node_3_0(nodeInactive_0(A,B,D),medium_0(A,E,[C],F),G),H,I)).
trans(l2_3_0(one_node_3_0(nodeActive_2(A,2,B,C,D,E),medium_0(A,F,G,H),I),J,K),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,D,C,E),medium_0(A,F,[second(C)|G],H),I),J,K)).
trans(l2_3_0(one_node_3_0(nodeActive_2(A,2,B,C,D,E),medium_0(A,F,[],G),H),I,J),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,D,C,E),medium_0(A,F,[second(C)],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),medium_0(A,E,F,G),H),I,J),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),medium_0(A,E,[first(C)|F],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),medium_0(A,E,[],F),G),H,I),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),medium_0(A,E,[first(C)],F),G),H,I)).
trans(l2_3_0(one_node_3_0(nodeActive_2(A,2,B,C,D,E),medium_0(A,F,G,H),I),J,K),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,D,C,E),medium_0(A,F,[second(C)|G],H),I),J,K)).
trans(l2_3_0(one_node_3_0(nodeActive_2(A,2,B,C,D,E),medium_0(A,F,[],G),H),I,J),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,D,C,E),medium_0(A,F,[second(C)],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),medium_0(A,E,F,G),H),I,J),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),medium_0(A,E,[first(C)|F],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),medium_0(A,E,[],F),G),H,I),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),medium_0(A,E,[first(C)],F),G),H,I)).
trans(l2_3_0(one_node_3_0(nodeActive_2(A,2,B,C,0,D),medium_0(A,E,F,G),H),I,J),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),medium_0(A,E,[second(C)|F],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeActive_2(A,2,B,C,0,D),medium_0(A,E,[],F),G),H,I),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),medium_0(A,E,[second(C)],F),G),H,I)).
trans(l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),medium_0(A,E,F,G),H),I,J),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),medium_0(A,E,[first(C)|F],G),H),I,J)).
trans(l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),medium_0(A,E,[],F),G),H,I),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),medium_0(A,E,[first(C)],F),G),H,I)).
trans(l2_3_0(one_node_3_0(node_0(A,B,0,2,C),medium_0(A,D,E,F),G),H,I),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,0,C),medium_0(A,D,[first(0)|E],F),G),H,I)).
trans(l2_3_0(one_node_3_0(node_0(A,B,0,2,C),medium_0(A,D,[],E),F),G,H),tau,l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,0,C),medium_0(A,D,[first(0)],E),F),G,H)).
trans(l2_3_0(one_node_0(A,B,0,2,C),D,E),nop,l2_3_0(one_node_3_0(node_0(F,B,0,2,end),medium_0(F,A,[],end),C),D,E)) :-
    handlechan([A,B],[F],G).
trans(l2_3_0(one_node_3_0(end,end,A),B,C),nop,l2_3_0(A,B,C)).
trans(l2_3_0(A,one_node_3_0(nodeActive_5(B,C,D,E,2,F),G,H),I),action(leader),l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,E,D,F),G,H),I)).
trans(l2_3_0(A,one_node_3_0(nodeActive_6(B),C,D),E),action(fail),l2_3_0(A,one_node_3_0(B,C,D),E)).
trans(l2_3_0(A,one_node_3_0(nodeActive_5(B,C,D,E,2,F),G,H),I),action(leader),l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,E,D,F),G,H),I)).
trans(l2_3_0(A,one_node_3_0(nodeActive_6(B),C,D),E),action(fail),l2_3_0(A,one_node_3_0(B,C,D),E)).
trans(l2_3_0(A,one_node_3_0(nodeActive_5(B,C,D,1,2,E),F,G),H),action(leader),l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,1,D,E),F,G),H)).
trans(l2_3_0(A,one_node_3_0(nodeActive_6(B),C,D),E),action(fail),l2_3_0(A,one_node_3_0(B,C,D),E)).
trans(l2_3_0(A,one_node_3_0(nodeInactive_1(B,C,D,E),medium_0(B,F,G,H),I),J),tau,l2_3_0(A,one_node_3_0(nodeInactive_0(B,C,E),medium_0(B,F,[D|G],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeInactive_1(B,C,D,E),medium_0(B,F,[],G),H),I),tau,l2_3_0(A,one_node_3_0(nodeInactive_0(B,C,E),medium_0(B,F,[D],G),H),I)).
trans(l2_3_0(A,one_node_3_0(nodeActive_2(B,2,C,D,E,F),medium_0(B,G,H,I),J),K),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,E,D,F),medium_0(B,G,[second(D)|H],I),J),K)).
trans(l2_3_0(A,one_node_3_0(nodeActive_2(B,2,C,D,E,F),medium_0(B,G,[],H),I),J),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,E,D,F),medium_0(B,G,[second(D)],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeActive_9(B,2,C,D,E),medium_0(B,F,G,H),I),J),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,D,D,E),medium_0(B,F,[first(D)|G],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeActive_9(B,2,C,D,E),medium_0(B,F,[],G),H),I),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,D,D,E),medium_0(B,F,[first(D)],G),H),I)).
trans(l2_3_0(A,one_node_3_0(nodeActive_2(B,2,C,D,E,F),medium_0(B,G,H,I),J),K),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,E,D,F),medium_0(B,G,[second(D)|H],I),J),K)).
trans(l2_3_0(A,one_node_3_0(nodeActive_2(B,2,C,D,E,F),medium_0(B,G,[],H),I),J),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,E,D,F),medium_0(B,G,[second(D)],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeActive_9(B,2,C,D,E),medium_0(B,F,G,H),I),J),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,D,D,E),medium_0(B,F,[first(D)|G],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeActive_9(B,2,C,D,E),medium_0(B,F,[],G),H),I),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,D,D,E),medium_0(B,F,[first(D)],G),H),I)).
trans(l2_3_0(A,one_node_3_0(nodeActive_2(B,2,C,D,1,E),medium_0(B,F,G,H),I),J),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,1,D,E),medium_0(B,F,[second(D)|G],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeActive_2(B,2,C,D,1,E),medium_0(B,F,[],G),H),I),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,1,D,E),medium_0(B,F,[second(D)],G),H),I)).
trans(l2_3_0(A,one_node_3_0(nodeActive_9(B,2,C,D,E),medium_0(B,F,G,H),I),J),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,D,D,E),medium_0(B,F,[first(D)|G],H),I),J)).
trans(l2_3_0(A,one_node_3_0(nodeActive_9(B,2,C,D,E),medium_0(B,F,[],G),H),I),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,D,D,E),medium_0(B,F,[first(D)],G),H),I)).
trans(l2_3_0(A,one_node_3_0(node_0(B,C,1,2,D),medium_0(B,E,F,G),H),I),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,1,0,D),medium_0(B,E,[first(1)|F],G),H),I)).
trans(l2_3_0(A,one_node_3_0(node_0(B,C,1,2,D),medium_0(B,E,[],F),G),H),tau,l2_3_0(A,one_node_3_0(nodeActive_0(B,C,2,1,0,D),medium_0(B,E,[first(1)],F),G),H)).
trans(l2_3_0(A,one_node_0(B,C,1,2,D),E),nop,l2_3_0(A,one_node_3_0(node_0(F,C,1,2,end),medium_0(F,B,[],end),D),E)) :-
    handlechan([B,C],[F],G).
trans(l2_3_0(A,one_node_3_0(end,end,B),C),nop,l2_3_0(A,B,C)).
trans(l2_3_0(one_node_3_0(nodeInactive_0(A,B,C),D,E),one_node_3_0(F,medium_0(G,B,H,I),J),K),tau,l2_3_0(one_node_3_0(nodeInactive_1(A,B,L,C),D,E),one_node_3_0(F,medium_0(G,B,M,I),J),K)) :-
    H \== [],
    strip_from_end(H,L,M).
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,D,E),F,G),one_node_3_0(H,medium_0(I,B,J,K),L),M),tau,l2_3_0(one_node_3_0(nodeActive_2(A,2,B,N,C,E),F,G),one_node_3_0(H,medium_0(I,B,O,K),L),M)) :-
    J \== [],
    strip_from_end(J,first(N),O),
    N \== C.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,1,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeActive_5(A,B,M,1,2,D),E,F),one_node_3_0(G,medium_0(H,B,N,J),K),L)) :-
    I \== [],
    strip_from_end(I,first(M),N),
    not M \== 1.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,D,E),F,G),one_node_3_0(H,medium_0(I,B,J,K),L),M),tau,l2_3_0(one_node_3_0(nodeActive_9(A,2,B,D,E),F,G),one_node_3_0(H,medium_0(I,B,N,K),L),M)) :-
    J \== [],
    strip_from_end(J,second(O),N),
    D > O,
    D > C.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,D,E),F,G),one_node_3_0(H,medium_0(I,B,J,K),L),M),tau,l2_3_0(one_node_3_0(nodeInactive_0(A,B,E),F,G),one_node_3_0(H,medium_0(I,B,N,K),L),M)) :-
    J \== [],
    strip_from_end(J,second(O),N),
    not (D > O  ','  D > C).
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeActive_2(A,2,B,M,C,D),E,F),one_node_3_0(G,medium_0(H,B,N,J),K),L)) :-
    I \== [],
    strip_from_end(I,first(M),N),
    M \== C.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,1,1,C),D,E),one_node_3_0(F,medium_0(G,B,H,I),J),K),tau,l2_3_0(one_node_3_0(nodeActive_5(A,B,L,1,2,C),D,E),one_node_3_0(F,medium_0(G,B,M,I),J),K)) :-
    H \== [],
    strip_from_end(H,first(L),M),
    not L \== 1.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),E,F),one_node_3_0(G,medium_0(H,B,M,J),K),L)) :-
    I \== [],
    strip_from_end(I,second(N),M),
    C > N,
    C > C.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,C,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeInactive_0(A,B,D),E,F),one_node_3_0(G,medium_0(H,B,M,J),K),L)) :-
    I \== [],
    strip_from_end(I,second(N),M),
    not (C > N  ','  C > C).
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeActive_2(A,2,B,M,0,D),E,F),one_node_3_0(G,medium_0(H,B,N,J),K),L)) :-
    I \== [],
    strip_from_end(I,first(M),N),
    M \== 0.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeActive_6(D),E,F),one_node_3_0(G,medium_0(H,B,M,J),K),L)) :-
    I \== [],
    strip_from_end(I,first(N),M),
    not N \== 0.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeActive_9(A,2,B,C,D),E,F),one_node_3_0(G,medium_0(H,B,M,J),K),L)) :-
    I \== [],
    strip_from_end(I,second(N),M),
    C > N,
    C > 0.
trans(l2_3_0(one_node_3_0(nodeActive_0(A,B,2,0,C,D),E,F),one_node_3_0(G,medium_0(H,B,I,J),K),L),tau,l2_3_0(one_node_3_0(nodeInactive_0(A,B,D),E,F),one_node_3_0(G,medium_0(H,B,M,J),K),L)) :-
    I \== [],
    strip_from_end(I,second(N),M),
    not (C > N  ','  C > 0).
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeInactive_0(G,C,H),I,J),K),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,L,E),F),one_node_3_0(nodeInactive_1(G,C,M,H),I,J),K)) :-
    D \== [],
    strip_from_end(D,M,L).
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,H,I,J),K,L),M),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,N,E),F),one_node_3_0(nodeActive_2(G,2,C,O,H,J),K,L),M)) :-
    D \== [],
    strip_from_end(D,first(O),N),
    O \== H.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,1,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeActive_5(G,C,N,1,2,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    not N \== 1.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,H,I,J),K,L),M),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,N,E),F),one_node_3_0(nodeActive_9(G,2,C,I,J),K,L),M)) :-
    D \== [],
    strip_from_end(D,second(O),N),
    I > O,
    I > H.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,H,I,J),K,L),M),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,N,E),F),one_node_3_0(nodeInactive_0(G,C,J),K,L),M)) :-
    D \== [],
    strip_from_end(D,second(O),N),
    not (I > O  ','  I > H).
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,H,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeActive_2(G,2,C,N,H,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    N \== H.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,1,1,H),I,J),K),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,L,E),F),one_node_3_0(nodeActive_5(G,C,M,1,2,H),I,J),K)) :-
    D \== [],
    strip_from_end(D,first(M),L),
    not M \== 1.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,H,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeActive_9(G,2,C,H,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,second(N),M),
    H > N,
    H > H.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,H,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeInactive_0(G,C,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,second(N),M),
    not (H > N  ','  H > H).
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,1,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeActive_2(G,2,C,N,1,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    N \== 1.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,1,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeActive_5(G,C,N,1,2,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    not N \== 1.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,1,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeActive_9(G,2,C,H,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,second(N),M),
    H > N,
    H > 1.
trans(l2_3_0(one_node_3_0(A,medium_0(B,C,D,E),F),one_node_3_0(nodeActive_0(G,C,2,1,H,I),J,K),L),tau,l2_3_0(one_node_3_0(A,medium_0(B,C,M,E),F),one_node_3_0(nodeInactive_0(G,C,I),J,K),L)) :-
    D \== [],
    strip_from_end(D,second(N),M),
    not (H > N  ','  H > 1).
trans(l2_0(A),nop,l2_3_0(one_node_3_0(node_0(B,C,0,2,end),medium_0(B,D,[],end),end),one_node_3_0(node_0(E,D,1,2,end),medium_0(E,C,[],end),end),A)) :-
    handlechan([],[D,C],F),
    handlechan([D,C],[B],G),
    handlechan([C,D],[E],H).
trans(l2_3_0(end,end,A),nop,A).
trans(l21_7_0(nodeActive_5(A,B,C,D,E,F),G,H,I,J),action(leader),l21_7_0(nodeActive_0(A,B,E,D,C,F),G,H,I,J)).
trans(l21_7_0(nodeActive_6(A),B,C,D,E),action(fail),l21_7_0(A,B,C,D,E)).
trans(l21_7_0(nodeActive_5(A,B,C,D,E,F),G,H,I,J),action(leader),l21_7_0(nodeActive_0(A,B,E,D,C,F),G,H,I,J)).
trans(l21_7_0(nodeActive_6(A),B,C,D,E),action(fail),l21_7_0(A,B,C,D,E)).
trans(l21_7_0(A,B,nodeActive_5(C,D,E,F,G,H),I,J),action(leader),l21_7_0(A,B,nodeActive_0(C,D,G,F,E,H),I,J)).
trans(l21_7_0(A,B,nodeActive_6(C),D,E),action(fail),l21_7_0(A,B,C,D,E)).
trans(l21_7_0(A,B,nodeActive_5(C,D,E,F,G,H),I,J),action(leader),l21_7_0(A,B,nodeActive_0(C,D,G,F,E,H),I,J)).
trans(l21_7_0(A,B,nodeActive_6(C),D,E),action(fail),l21_7_0(A,B,C,D,E)).
trans(l21_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l21_7_0(nodeActive_2(A,C,B,L,D,E),F,G,medium_0(H,B,M,J),K)) :-
    I \== [],
    strip_from_end(I,first(L),M),
    L \== D.
trans(l21_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l21_7_0(nodeActive_5(A,B,L,D,C,E),F,G,medium_0(H,B,M,J),K)) :-
    I \== [],
    strip_from_end(I,first(L),M),
    not L \== D,
    D  is  C - 1.
trans(l21_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l21_7_0(nodeActive_6(E),F,G,medium_0(H,B,L,J),K)) :-
    I \== [],
    strip_from_end(I,first(M),L),
    not M \== D,
    not D  is  C - 1.
trans(l21_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l21_7_0(nodeActive_9(A,C,B,D,E),F,G,medium_0(H,B,L,J),K)) :-
    I \== [],
    strip_from_end(I,second(M),L),
    D > M,
    D > D.
trans(l21_7_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,H,I),J,K,L),tau,l21_7_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)|H],I),J,K,L)).
trans(l21_7_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,[],H),I,J,K),tau,l21_7_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)],H),I,J,K)).
trans(l21_7_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,G,H),I,J,K),tau,l21_7_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)|G],H),I,J,K)).
trans(l21_7_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,[],G),H,I,J),tau,l21_7_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)],G),H,I,J)).
trans(l21_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l21_7_0(nodeInactive_0(A,B,E),F,G,medium_0(H,B,L,J),K)) :-
    I \== [],
    strip_from_end(I,second(M),L),
    not (D > M  ','  D > D).
trans(l21_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l21_7_0(nodeActive_2(A,C,B,M,D,F),G,H,medium_0(I,B,N,K),L)) :-
    J \== [],
    strip_from_end(J,first(M),N),
    M \== D.
trans(l21_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l21_7_0(nodeActive_5(A,B,M,D,C,F),G,H,medium_0(I,B,N,K),L)) :-
    J \== [],
    strip_from_end(J,first(M),N),
    not M \== D,
    D  is  C - 1.
trans(l21_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l21_7_0(nodeActive_6(F),G,H,medium_0(I,B,M,K),L)) :-
    J \== [],
    strip_from_end(J,first(N),M),
    not N \== D,
    not D  is  C - 1.
trans(l21_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l21_7_0(nodeActive_9(A,C,B,E,F),G,H,medium_0(I,B,M,K),L)) :-
    J \== [],
    strip_from_end(J,second(N),M),
    E > N,
    E > D.
trans(l21_7_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,H,I),J,K,L),tau,l21_7_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)|H],I),J,K,L)).
trans(l21_7_0(nodeActive_2(A,B,C,D,E,F),medium_0(A,G,[],H),I,J,K),tau,l21_7_0(nodeActive_0(A,C,B,E,D,F),medium_0(A,G,[second(D)],H),I,J,K)).
trans(l21_7_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,G,H),I,J,K),tau,l21_7_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)|G],H),I,J,K)).
trans(l21_7_0(nodeActive_9(A,B,C,D,E),medium_0(A,F,[],G),H,I,J),tau,l21_7_0(nodeActive_0(A,C,B,D,D,E),medium_0(A,F,[first(D)],G),H,I,J)).
trans(l21_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l21_7_0(nodeInactive_0(A,B,F),G,H,medium_0(I,B,M,K),L)) :-
    J \== [],
    strip_from_end(J,second(N),M),
    not (E > N  ','  E > D).
trans(l21_7_0(nodeInactive_0(A,B,C),D,E,medium_0(F,B,G,H),I),tau,l21_7_0(nodeInactive_1(A,B,J,C),D,E,medium_0(F,B,K,H),I)) :-
    G \== [],
    strip_from_end(G,J,K).
trans(l21_7_0(nodeInactive_1(A,B,C,D),medium_0(A,E,F,G),H,I,J),tau,l21_7_0(nodeInactive_0(A,B,D),medium_0(A,E,[C|F],G),H,I,J)).
trans(l21_7_0(nodeInactive_1(A,B,C,D),medium_0(A,E,[],F),G,H,I),tau,l21_7_0(nodeInactive_0(A,B,D),medium_0(A,E,[C],F),G,H,I)).
trans(l21_7_0(node_0(A,B,0,2,C),medium_0(A,D,E,F),G,H,I),tau,l21_7_0(nodeActive_0(A,B,2,0,0,C),medium_0(A,D,[first(0)|E],F),G,H,I)).
trans(l21_7_0(node_0(A,B,0,2,C),medium_0(A,D,[],E),F,G,H),tau,l21_7_0(nodeActive_0(A,B,2,0,0,C),medium_0(A,D,[first(0)],E),F,G,H)).
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,H,I),J,K),tau,l21_7_0(A,medium_0(B,C,L,E),nodeActive_2(F,G,C,M,H,I),J,K)) :-
    D \== [],
    strip_from_end(D,first(M),L),
    M \== H.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,H,I),J,K),tau,l21_7_0(A,medium_0(B,C,L,E),nodeActive_5(F,C,M,H,G,I),J,K)) :-
    D \== [],
    strip_from_end(D,first(M),L),
    not M \== H,
    H  is  G - 1.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,H,I),J,K),tau,l21_7_0(A,medium_0(B,C,L,E),nodeActive_6(I),J,K)) :-
    D \== [],
    strip_from_end(D,first(M),L),
    not M \== H,
    not H  is  G - 1.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,H,I),J,K),tau,l21_7_0(A,medium_0(B,C,L,E),nodeActive_9(F,G,C,H,I),J,K)) :-
    D \== [],
    strip_from_end(D,second(M),L),
    H > M,
    H > H.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,H,I),J,K),tau,l21_7_0(A,medium_0(B,C,L,E),nodeInactive_0(F,C,I),J,K)) :-
    D \== [],
    strip_from_end(D,second(M),L),
    not (H > M  ','  H > H).
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,I,J),K,L),tau,l21_7_0(A,medium_0(B,C,M,E),nodeActive_2(F,G,C,N,H,J),K,L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    N \== H.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,I,J),K,L),tau,l21_7_0(A,medium_0(B,C,M,E),nodeActive_5(F,C,N,H,G,J),K,L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    not N \== H,
    H  is  G - 1.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,I,J),K,L),tau,l21_7_0(A,medium_0(B,C,M,E),nodeActive_6(J),K,L)) :-
    D \== [],
    strip_from_end(D,first(N),M),
    not N \== H,
    not H  is  G - 1.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,I,J),K,L),tau,l21_7_0(A,medium_0(B,C,M,E),nodeActive_9(F,G,C,I,J),K,L)) :-
    D \== [],
    strip_from_end(D,second(N),M),
    I > N,
    I > H.
trans(l21_7_0(A,medium_0(B,C,D,E),nodeActive_0(F,C,G,H,I,J),K,L),tau,l21_7_0(A,medium_0(B,C,M,E),nodeInactive_0(F,C,J),K,L)) :-
    D \== [],
    strip_from_end(D,second(N),M),
    not (I > N  ','  I > H).
trans(l21_7_0(A,medium_0(B,C,D,E),nodeInactive_0(F,C,G),H,I),tau,l21_7_0(A,medium_0(B,C,J,E),nodeInactive_1(F,C,K,G),H,I)) :-
    D \== [],
    strip_from_end(D,K,J).
trans(l21_7_0(A,B,nodeActive_2(C,D,E,F,G,H),medium_0(C,I,J,K),L),tau,l21_7_0(A,B,nodeActive_0(C,E,D,G,F,H),medium_0(C,I,[second(F)|J],K),L)).
trans(l21_7_0(A,B,nodeActive_2(C,D,E,F,G,H),medium_0(C,I,[],J),K),tau,l21_7_0(A,B,nodeActive_0(C,E,D,G,F,H),medium_0(C,I,[second(F)],J),K)).
trans(l21_7_0(A,B,nodeActive_9(C,D,E,F,G),medium_0(C,H,I,J),K),tau,l21_7_0(A,B,nodeActive_0(C,E,D,F,F,G),medium_0(C,H,[first(F)|I],J),K)).
trans(l21_7_0(A,B,nodeActive_9(C,D,E,F,G),medium_0(C,H,[],I),J),tau,l21_7_0(A,B,nodeActive_0(C,E,D,F,F,G),medium_0(C,H,[first(F)],I),J)).
trans(l21_7_0(A,B,nodeActive_2(C,D,E,F,G,H),medium_0(C,I,J,K),L),tau,l21_7_0(A,B,nodeActive_0(C,E,D,G,F,H),medium_0(C,I,[second(F)|J],K),L)).
trans(l21_7_0(A,B,nodeActive_2(C,D,E,F,G,H),medium_0(C,I,[],J),K),tau,l21_7_0(A,B,nodeActive_0(C,E,D,G,F,H),medium_0(C,I,[second(F)],J),K)).
trans(l21_7_0(A,B,nodeActive_9(C,D,E,F,G),medium_0(C,H,I,J),K),tau,l21_7_0(A,B,nodeActive_0(C,E,D,F,F,G),medium_0(C,H,[first(F)|I],J),K)).
trans(l21_7_0(A,B,nodeActive_9(C,D,E,F,G),medium_0(C,H,[],I),J),tau,l21_7_0(A,B,nodeActive_0(C,E,D,F,F,G),medium_0(C,H,[first(F)],I),J)).
trans(l21_7_0(A,B,nodeInactive_1(C,D,E,F),medium_0(C,G,H,I),J),tau,l21_7_0(A,B,nodeInactive_0(C,D,F),medium_0(C,G,[E|H],I),J)).
trans(l21_7_0(A,B,nodeInactive_1(C,D,E,F),medium_0(C,G,[],H),I),tau,l21_7_0(A,B,nodeInactive_0(C,D,F),medium_0(C,G,[E],H),I)).
trans(l21_7_0(A,B,node_0(C,D,1,2,E),medium_0(C,F,G,H),I),tau,l21_7_0(A,B,nodeActive_0(C,D,2,1,0,E),medium_0(C,F,[first(1)|G],H),I)).
trans(l21_7_0(A,B,node_0(C,D,1,2,E),medium_0(C,F,[],G),H),tau,l21_7_0(A,B,nodeActive_0(C,D,2,1,0,E),medium_0(C,F,[first(1)],G),H)).
trans(l21_0(A),nop,l21_7_0(node_0(B,C,0,2,end),medium_0(B,D,[],end),node_0(E,D,1,2,end),medium_0(E,C,[],end),A)) :-
    handlechan([],[B,C],F),
    handlechan([B,C],[D],G),
    handlechan([C,D],[E],H).
trans(l21_7_0(end,end,end,end,A),nop,A).
trans(l22_7_0(nodeActive_5(A,B,C,D,E,F),G,H,I,J),action(leader),l22_7_0(nodeActive_0(A,B,E,D,C,F),G,H,I,J)).
trans(l22_7_0(nodeActive_6(A),B,C,D,E),action(fail),l22_7_0(A,B,C,D,E)).
trans(l22_7_0(nodeActive_5(A,B,C,D,E,F),G,H,I,J),action(leader),l22_7_0(nodeActive_0(A,B,E,D,C,F),G,H,I,J)).
trans(l22_7_0(nodeActive_6(A),B,C,D,E),action(fail),l22_7_0(A,B,C,D,E)).
trans(l22_7_0(A,nodeActive_5(B,C,D,E,F,G),H,I,J),action(leader),l22_7_0(A,nodeActive_0(B,C,F,E,D,G),H,I,J)).
trans(l22_7_0(A,nodeActive_6(B),C,D,E),action(fail),l22_7_0(A,B,C,D,E)).
trans(l22_7_0(A,nodeActive_5(B,C,D,E,F,G),H,I,J),action(leader),l22_7_0(A,nodeActive_0(B,C,F,E,D,G),H,I,J)).
trans(l22_7_0(A,nodeActive_6(B),C,D,E),action(fail),l22_7_0(A,B,C,D,E)).
trans(l22_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l22_7_0(nodeActive_2(A,C,B,L,D,E),F,G,medium_0(H,B,M,J),K)) :-
    I \== [],
    strip_from_end(I,first(L),M),
    L \== D.
trans(l22_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l22_7_0(nodeActive_5(A,B,L,D,C,E),F,G,medium_0(H,B,M,J),K)) :-
    I \== [],
    strip_from_end(I,first(L),M),
    not L \== D,
    D  is  C - 1.
trans(l22_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l22_7_0(nodeActive_6(E),F,G,medium_0(H,B,L,J),K)) :-
    I \== [],
    strip_from_end(I,first(M),L),
    not M \== D,
    not D  is  C - 1.
trans(l22_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l22_7_0(nodeActive_9(A,C,B,D,E),F,G,medium_0(H,B,L,J),K)) :-
    I \== [],
    strip_from_end(I,second(M),L),
    D > M,
    D > D.
trans(l22_7_0(nodeActive_2(A,B,C,D,E,F),G,medium_0(A,H,I,J),K,L),tau,l22_7_0(nodeActive_0(A,C,B,E,D,F),G,medium_0(A,H,[second(D)|I],J),K,L)).
trans(l22_7_0(nodeActive_2(A,B,C,D,E,F),G,medium_0(A,H,[],I),J,K),tau,l22_7_0(nodeActive_0(A,C,B,E,D,F),G,medium_0(A,H,[second(D)],I),J,K)).
trans(l22_7_0(nodeActive_9(A,B,C,D,E),F,medium_0(A,G,H,I),J,K),tau,l22_7_0(nodeActive_0(A,C,B,D,D,E),F,medium_0(A,G,[first(D)|H],I),J,K)).
trans(l22_7_0(nodeActive_9(A,B,C,D,E),F,medium_0(A,G,[],H),I,J),tau,l22_7_0(nodeActive_0(A,C,B,D,D,E),F,medium_0(A,G,[first(D)],H),I,J)).
trans(l22_7_0(nodeActive_0(A,B,C,D,D,E),F,G,medium_0(H,B,I,J),K),tau,l22_7_0(nodeInactive_0(A,B,E),F,G,medium_0(H,B,L,J),K)) :-
    I \== [],
    strip_from_end(I,second(M),L),
    not (D > M  ','  D > D).
trans(l22_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l22_7_0(nodeActive_2(A,C,B,M,D,F),G,H,medium_0(I,B,N,K),L)) :-
    J \== [],
    strip_from_end(J,first(M),N),
    M \== D.
trans(l22_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l22_7_0(nodeActive_5(A,B,M,D,C,F),G,H,medium_0(I,B,N,K),L)) :-
    J \== [],
    strip_from_end(J,first(M),N),
    not M \== D,
    D  is  C - 1.
trans(l22_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l22_7_0(nodeActive_6(F),G,H,medium_0(I,B,M,K),L)) :-
    J \== [],
    strip_from_end(J,first(N),M),
    not N \== D,
    not D  is  C - 1.
trans(l22_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l22_7_0(nodeActive_9(A,C,B,E,F),G,H,medium_0(I,B,M,K),L)) :-
    J \== [],
    strip_from_end(J,second(N),M),
    E > N,
    E > D.
trans(l22_7_0(nodeActive_2(A,B,C,D,E,F),G,medium_0(A,H,I,J),K,L),tau,l22_7_0(nodeActive_0(A,C,B,E,D,F),G,medium_0(A,H,[second(D)|I],J),K,L)).
trans(l22_7_0(nodeActive_2(A,B,C,D,E,F),G,medium_0(A,H,[],I),J,K),tau,l22_7_0(nodeActive_0(A,C,B,E,D,F),G,medium_0(A,H,[second(D)],I),J,K)).
trans(l22_7_0(nodeActive_9(A,B,C,D,E),F,medium_0(A,G,H,I),J,K),tau,l22_7_0(nodeActive_0(A,C,B,D,D,E),F,medium_0(A,G,[first(D)|H],I),J,K)).
trans(l22_7_0(nodeActive_9(A,B,C,D,E),F,medium_0(A,G,[],H),I,J),tau,l22_7_0(nodeActive_0(A,C,B,D,D,E),F,medium_0(A,G,[first(D)],H),I,J)).
trans(l22_7_0(nodeActive_0(A,B,C,D,E,F),G,H,medium_0(I,B,J,K),L),tau,l22_7_0(nodeInactive_0(A,B,F),G,H,medium_0(I,B,M,K),L)) :-
    J \== [],
    strip_from_end(J,second(N),M),
    not (E > N  ','  E > D).
trans(l22_7_0(nodeInactive_0(A,B,C),D,E,medium_0(F,B,G,H),I),tau,l22_7_0(nodeInactive_1(A,B,J,C),D,E,medium_0(F,B,K,H),I)) :-
    G \== [],
    strip_from_end(G,J,K).
trans(l22_7_0(nodeInactive_1(A,B,C,D),E,medium_0(A,F,G,H),I,J),tau,l22_7_0(nodeInactive_0(A,B,D),E,medium_0(A,F,[C|G],H),I,J)).
trans(l22_7_0(nodeInactive_1(A,B,C,D),E,medium_0(A,F,[],G),H,I),tau,l22_7_0(nodeInactive_0(A,B,D),E,medium_0(A,F,[C],G),H,I)).
trans(l22_7_0(node_0(A,B,0,2,C),D,medium_0(A,E,F,G),H,I),tau,l22_7_0(nodeActive_0(A,B,2,0,0,C),D,medium_0(A,E,[first(0)|F],G),H,I)).
trans(l22_7_0(node_0(A,B,0,2,C),D,medium_0(A,E,[],F),G,H),tau,l22_7_0(nodeActive_0(A,B,2,0,0,C),D,medium_0(A,E,[first(0)],F),G,H)).
trans(l22_7_0(A,nodeActive_0(B,C,D,E,E,F),medium_0(G,C,H,I),J,K),tau,l22_7_0(A,nodeActive_2(B,D,C,L,E,F),medium_0(G,C,M,I),J,K)) :-
    H \== [],
    strip_from_end(H,first(L),M),
    L \== E.
trans(l22_7_0(A,nodeActive_0(B,C,D,E,E,F),medium_0(G,C,H,I),J,K),tau,l22_7_0(A,nodeActive_5(B,C,L,E,D,F),medium_0(G,C,M,I),J,K)) :-
    H \== [],
    strip_from_end(H,first(L),M),
    not L \== E,
    E  is  D - 1.
trans(l22_7_0(A,nodeActive_0(B,C,D,E,E,F),medium_0(G,C,H,I),J,K),tau,l22_7_0(A,nodeActive_6(F),medium_0(G,C,L,I),J,K)) :-
    H \== [],
    strip_from_end(H,first(M),L),
    not M \== E,
    not E  is  D - 1.
trans(l22_7_0(A,nodeActive_0(B,C,D,E,E,F),medium_0(G,C,H,I),J,K),tau,l22_7_0(A,nodeActive_9(B,D,C,E,F),medium_0(G,C,L,I),J,K)) :-
    H \== [],
    strip_from_end(H,second(M),L),
    E > M,
    E > E.
trans(l22_7_0(A,nodeActive_2(B,C,D,E,F,G),H,medium_0(B,I,J,K),L),tau,l22_7_0(A,nodeActive_0(B,D,C,F,E,G),H,medium_0(B,I,[second(E)|J],K),L)).
trans(l22_7_0(A,nodeActive_2(B,C,D,E,F,G),H,medium_0(B,I,[],J),K),tau,l22_7_0(A,nodeActive_0(B,D,C,F,E,G),H,medium_0(B,I,[second(E)],J),K)).
trans(l22_7_0(A,nodeActive_9(B,C,D,E,F),G,medium_0(B,H,I,J),K),tau,l22_7_0(A,nodeActive_0(B,D,C,E,E,F),G,medium_0(B,H,[first(E)|I],J),K)).
trans(l22_7_0(A,nodeActive_9(B,C,D,E,F),G,medium_0(B,H,[],I),J),tau,l22_7_0(A,nodeActive_0(B,D,C,E,E,F),G,medium_0(B,H,[first(E)],I),J)).
trans(l22_7_0(A,nodeActive_0(B,C,D,E,E,F),medium_0(G,C,H,I),J,K),tau,l22_7_0(A,nodeInactive_0(B,C,F),medium_0(G,C,L,I),J,K)) :-
    H \== [],
    strip_from_end(H,second(M),L),
    not (E > M  ','  E > E).
trans(l22_7_0(A,nodeActive_0(B,C,D,E,F,G),medium_0(H,C,I,J),K,L),tau,l22_7_0(A,nodeActive_2(B,D,C,M,E,G),medium_0(H,C,N,J),K,L)) :-
    I \== [],
    strip_from_end(I,first(M),N),
    M \== E.
trans(l22_7_0(A,nodeActive_0(B,C,D,E,F,G),medium_0(H,C,I,J),K,L),tau,l22_7_0(A,nodeActive_5(B,C,M,E,D,G),medium_0(H,C,N,J),K,L)) :-
    I \== [],
    strip_from_end(I,first(M),N),
    not M \== E,
    E  is  D - 1.
trans(l22_7_0(A,nodeActive_0(B,C,D,E,F,G),medium_0(H,C,I,J),K,L),tau,l22_7_0(A,nodeActive_6(G),medium_0(H,C,M,J),K,L)) :-
    I \== [],
    strip_from_end(I,first(N),M),
    not N \== E,
    not E  is  D - 1.
trans(l22_7_0(A,nodeActive_0(B,C,D,E,F,G),medium_0(H,C,I,J),K,L),tau,l22_7_0(A,nodeActive_9(B,D,C,F,G),medium_0(H,C,M,J),K,L)) :-
    I \== [],
    strip_from_end(I,second(N),M),
    F > N,
    F > E.
trans(l22_7_0(A,nodeActive_2(B,C,D,E,F,G),H,medium_0(B,I,J,K),L),tau,l22_7_0(A,nodeActive_0(B,D,C,F,E,G),H,medium_0(B,I,[second(E)|J],K),L)).
trans(l22_7_0(A,nodeActive_2(B,C,D,E,F,G),H,medium_0(B,I,[],J),K),tau,l22_7_0(A,nodeActive_0(B,D,C,F,E,G),H,medium_0(B,I,[second(E)],J),K)).
trans(l22_7_0(A,nodeActive_9(B,C,D,E,F),G,medium_0(B,H,I,J),K),tau,l22_7_0(A,nodeActive_0(B,D,C,E,E,F),G,medium_0(B,H,[first(E)|I],J),K)).
trans(l22_7_0(A,nodeActive_9(B,C,D,E,F),G,medium_0(B,H,[],I),J),tau,l22_7_0(A,nodeActive_0(B,D,C,E,E,F),G,medium_0(B,H,[first(E)],I),J)).
trans(l22_7_0(A,nodeActive_0(B,C,D,E,F,G),medium_0(H,C,I,J),K,L),tau,l22_7_0(A,nodeInactive_0(B,C,G),medium_0(H,C,M,J),K,L)) :-
    I \== [],
    strip_from_end(I,second(N),M),
    not (F > N  ','  F > E).
trans(l22_7_0(A,nodeInactive_0(B,C,D),medium_0(E,C,F,G),H,I),tau,l22_7_0(A,nodeInactive_1(B,C,J,D),medium_0(E,C,K,G),H,I)) :-
    F \== [],
    strip_from_end(F,J,K).
trans(l22_7_0(A,nodeInactive_1(B,C,D,E),F,medium_0(B,G,H,I),J),tau,l22_7_0(A,nodeInactive_0(B,C,E),F,medium_0(B,G,[D|H],I),J)).
trans(l22_7_0(A,nodeInactive_1(B,C,D,E),F,medium_0(B,G,[],H),I),tau,l22_7_0(A,nodeInactive_0(B,C,E),F,medium_0(B,G,[D],H),I)).
trans(l22_7_0(A,node_0(B,C,1,2,D),E,medium_0(B,F,G,H),I),tau,l22_7_0(A,nodeActive_0(B,C,2,1,0,D),E,medium_0(B,F,[first(1)|G],H),I)).
trans(l22_7_0(A,node_0(B,C,1,2,D),E,medium_0(B,F,[],G),H),tau,l22_7_0(A,nodeActive_0(B,C,2,1,0,D),E,medium_0(B,F,[first(1)],G),H)).
trans(l22_0(A),nop,l22_7_0(node_0(B,C,0,2,end),node_0(D,E,1,2,end),medium_0(B,E,[],end),medium_0(D,C,[],end),A)) :-
    handlechan([],[B,C],F),
    handlechan([B,C],[D,E],G),
    handlechan([B,C,D,E],[],H).
trans(l22_7_0(end,end,end,end,A),nop,A).

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

startstate(medium(A,B,C),medium_0(A,B,C,end)).
startstate(node(A,B,C,D),node_0(A,B,C,D,end)).
startstate(nodeActive(A,B,C,D,E),nodeActive_0(A,B,C,D,E,end)).
startstate(nodeInactive(A,B),nodeInactive_0(A,B,end)).
startstate(systemLeader(A),systemLeader_0(A,end)).
startstate(one_node(A,B,C,D),one_node_0(A,B,C,D,end)).
startstate(chain(A,B,C,D),chain_0(A,B,C,D,end)).
startstate(l2,l2_0(end)).
startstate(l21,l21_0(end)).
startstate(l22,l22_0(end)).

fDef(ae_leader,fAnd(fDiamSetMinus([],tt),fBoxMinus(action(leader),form(ae_leader)))).
fDef(ee_fail,fOr(fDiam(action(fail),tt),fDiamSetMinus([],form(ee_fail)))).
fDef(one_leader,fAnd(fBoxMinus(action(leader),form(one_leader)),fBox(action(leader),form(no_leader)))).
fDef(no_leader,neg_form(neg_no_leader)).
fDef(neg_no_leader,fOr(fDiam(action(leader),tt),fDiamSetMinus([],form(neg_no_leader)))).
fDef(aa_true,neg_form(neg_aa_true)).
fDef(neg_aa_true,fOr(fDiam(action(fail),tt),fDiamSetMinus([],form(neg_aa_true)))).
fDef(aa_leader,neg_form(neg_aa_leader)).
fDef(neg_aa_leader,fOr(fBoxSetMinus([],ff),fDiamMinus(action(leader),form(neg_aa_leader)))).
fDef(deadlock,fOr(fBoxSetMinus([],ff),fDiamSetMinus([],form(deadlock)))).
