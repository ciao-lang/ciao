
%%============================================================================ 
%% CNF.pl
%% Convertor of Boolean formulae to CNF
%% Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

:- module(cnf,[cnf/2,cnf_dl/2]).

cnf(F,Cnf) :- cnf_dl(F,Cnf-[]).

cnf_dl(F,[[B]|Cnf1]-Cnf2) :- iff(F,+,B,Cnf2,Cnf1).


iff(V,_,B,Acc,Acc) :- var(V), !, V=B.
iff(1,_,B,Acc,Acc) :- !, B=1.
iff(0,_,B,Acc,Acc) :- !, B=0.

iff(-X,+,B,Acc,CNF) :- !, iff(X,-,BX,Acc,CNF), neglit(BX,B).
iff(-X,-,B,Acc,CNF) :- !, iff(X,+,BX,Acc,CNF), neglit(BX,B).
iff(-X,*,B,Acc,CNF) :- !, iff(X,*,BX,Acc,CNF), neglit(BX,B).

iff((X+Y),Polarity,B,Acc,CNF) :- !,
	iff(X,Polarity,BX,Acc,AccX),
	iff(Y,Polarity,BY,AccX,AccXY),
	(
	    Polarity == + -> CNF = [[-B,BX,BY]|AccXY]
	;
	    Polarity == - -> CNF = [[B,-BX],[B,-BY]|AccXY]
	;
	    CNF = [[-B,BX,BY], [B,-BX], [B,-BY] | AccXY]
	).

iff((X*Y),Polarity,B,Acc,CNF) :- !,
	iff(X,Polarity,BX,Acc,AccX),
	iff(Y,Polarity,BY,AccX,AccXY),
	(
	    Polarity == + -> CNF = [[-B,BX],[-B,BY]|AccXY]
	;   
	    Polarity == - -> CNF = [[B,-BX,-BY]|AccXY]
	;
	    CNF = [[B,-BX,-BY], [-B,BX], [-B,BY] | AccXY]
	).

iff((X==Y),Polarity,B,Acc,CNF) :- !,
	iff(X,*,BX,Acc,AccX),
	iff(Y,*,BY,AccX,AccXY),
	(
	    Polarity == + -> CNF = [[-BX,BY,-B],[BX,-BY,-B] | AccXY]
	;   
	    Polarity == - -> CNF = [[-BX,-BY,B],[BX,BY,B] | AccXY]
	;
	    CNF =  [[-BX,BY,-B],[BX,-BY,-B],[-BX,-BY,B],[BX,BY,B] | AccXY]
	).

%% iff((X xor Y),Polarity,B,Acc,CNF) :- !,
%% 	iff(X,*,BX,Acc,AccX),
%% 	iff(Y,*,BY,AccX,AccXY),
%% 	(
%% 	    Polarity == + -> CNF = [[-BX,-BY,-B],[BX,BY,-B] | AccXY]
%% 	;
%% 	    Polarity == - -> CNF = [[-BX,BY,B],[BX,-BY,B] | AccXY]
%% 	;
%% 	    CNF = [[-BX,BY,B],[BX,-BY,B],[-BX,-BY,-B],[BX,BY,-B] | AccXY]
%% 	).

iff((X->Y;Z),Polarity,B,Acc,CNF) :- !,
	iff(X,*,BX,Acc,AccX),
	iff(Y,Polarity,BY,AccX,AccXY),
	iff(Z,Polarity,BZ,AccXY,AccXYZ),
	(   
	    Polarity == + -> CNF = [[-BX,BY,-B],[BX,BZ,-B],[BY,BZ,-B]|AccXYZ]
	;   
	    Polarity == - -> CNF = [[-BX,-BY,B],[BX,-BZ,B],[-BY,-BZ,B]|AccXYZ]
	;
	    CNF = [[-BX,BY,-B], [BX,BZ,-B], [BY,BZ,-B],
	           [-BX,-BY,B], [BX,-BZ,B], [-BY,-BZ,B] | AccXYZ]
	).

neglit(V,-V) :- var(V), !.
neglit(-V,V).
neglit(0,1).
neglit(1,0).

