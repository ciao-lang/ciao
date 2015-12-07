
int(int).
nnegint(nnegint).
anyfd(anyfd).                            
rat(rat).
intexpr(int).
intexpr(anyfd).
intexpr(X1+X2) :- 
	intexpr(X1), 
        intexpr(X2).
intexpr(X1-X2) :- 
	intexpr(X1), 
        intexpr(X2).
intexpr(X1*X2) :- 
	intexpr(X1), 
	intexpr(X2).

nnegexpr(anyfd).
nnegexpr(X1+X2) :- 
	nnegexpr(X1), 
        nnegexpr(X2).
nnegexpr(X1*X2) :- 
	nnegexpr(X1), 
	nnegexpr(X2).


fdom(X1..X2):-
	int(X1),
	int(X2).
fdom(X1:X2):-
	int(X1),
	int(X2).
fdom([X1]):-
	int(X1).
fdom([X1|X2]):-
	int(X1),
	fdomlist(X2).
fdomlist([]).
fdomlist([X1|X2]):-
	int(X1),
	fdomlist(X2).
	
fd_or_list(anyfd).
fd_or_list([]).
fd_or_list([X1|X2]):-
	anyfd(X1),
	fdlist(X2).

fdlist([]).
fdlist([X1|X2]):-
	anyfd(X1),
	fdlist(X2).
	
r_diseq(anyfd).
r_diseq(X1+X2):-
	anyfd(X1),
	int(X2).

lmethod(first_fail).
lmethod(most_constrained).
lmethod(smallest).
lmethod(largest).
lmethod(max_regret).


rat(rat).

ratexpr(rat).
ratexpr(X1+X2) :- 
	ratexpr(X1), 
        ratexpr(X2).
ratexpr(X1-X2) :- 
	ratexpr(X1), 
        ratexpr(X2).
ratexpr(X1*X2) :- 
	ratexpr(X1), 
	ratexpr(X2).
