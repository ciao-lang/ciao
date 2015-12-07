:- module(itinerary,_,[fuzzy]).



tmethod(1,X,0) :~ {X .<. 4}.
tmethod(1,X,M) :~ {X .>=.4, X .=<. 4.5, M / 2 .=. X - 4}.
tmethod(1,X,M) :~ {X .>. 4.5 , X .=<. 5, M / 2 .=. 5 - X}.
tmethod(1,X,0) :~ {X .>. 5}.

cmethod(1,2.4,1) :~  .
cmethod(1,X,0) :~ {X .<>. 2.4}  .

evaluation(M,X,Mu) :~ tmethod(M,T,Mt), cmethod(M,C,Mc),{ X .=. T + C}.
