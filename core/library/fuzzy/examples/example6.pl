:- module(example6,_,[fuzzy]).



english(X,T,M) :~ 
	alive(X,T,Mv),
	born_in_Eng(X).

english(X,T,M) :~
	alive(X,T,Mv),
	{father(X,Y)},
	english(Y,T1,Mi),
	{T1 .=<. T}.

alive(john,D,1) :~ {D .=<.1990, D.>=. 1920}.
alive(john,D,0) :~ {D .<. 1920}.
alive(john,D,0) :~ {D .>. 1990}.
alive(peter,D,1) :~ {D .=<.2001, D.>=. 1950}.
alive(peter,D,0) :~ {D.<. 1950}. 
	
father(peter,john).

born_in_Eng(peter).
