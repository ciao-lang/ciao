:- module(example3,_,[fuzzy]).



potential_customer(X,Mu):~ 
	{customer(X),
	Mu .>=. 0.7}.

top_potential_customer(X,Mu):~
	{customer(X),
	Mu .>=. 0.9}.

good_credit_customer(X,Mu):~
	balance_level(X,Y,Mu),
	{Mu .>=. 0.7}.

customer(john).
customer(richard).

balance_level(john,400,0.7):~ .
balance_level(richard,500,0.8):~ .
