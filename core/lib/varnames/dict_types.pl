:- module(dict_types, _, [assertions, regtypes]).

:- regtype varnamesl(D)
     # "@var{D} is a list of @tt{Name=Var} pairs, where @tt{Var} is a 
	variable and @tt{Name} its name.".

varnamesl(D):- list(D,varnamepair).

:- regtype varnamepair/1.

varnamepair(X=Y):- varname(X), var(Y).

:- regtype varname(N) # "@var{N} is a term representing a variable name.".

varname('$VAR'(S)):- string(S).
varname('$VAR'(A)):- atm(A).
varname(A) :- atm(A). % Representation used in ciaopp: p_unit:program/2
