:- module(pretty_print,
	[ 
	  pretty_print/2,
	  pretty_print/3,
	  pretty_print/4
	],
	[ assertions,
	  regtypes,
	  fsyntax
	]).

:- use_module(library(operators)).
:- use_module(library(vndict)).
:- use_module(library(write)).

%% -----------------------------------------------------------------------
:- doc(title,"A simple pretty-printer for Ciao programs").

:- doc(author, "The CLIP Group").

:- doc(module,"This library module writes out to standard output a 
	clause or a list of clauses.").

%% :- doc(bug,"1. Bodies that finish with an operator can not be read in.
%%     E.g., foo(X) :- X='?' is written foo(X) :- X= ?.
%%     Should be foo(X) :- X= ? .
%% ").

:- doc( bug , "2.- If the priority of and operator, &/1 or &/2, is
redefined with lower priority than :-/2 or ./1, the written term is
incorrect because it does not include parenthesis to make Ciao
associate and operator first." ).

%% -----------------------------------------------------------------------

:- pred pretty_print(Cls,Flags) : clauses * list(flag)
	# "Prints each clause in the list @var{Cls} after numbering its
	  variables.".

:- pred pretty_print(Cls,Flags,Ds) : clauses * list(flag) * varnamedict
	# "Prints each clause in the list @var{Cls} after using the 
	  corresponding variable names dictionary in @var{Ds} to
	  name its variables.".

:- doc(doinclude,clauses/1).

%% :- typedef clauses ::= [] ; [clause|clauses] ; clause .
:- regtype clauses/1.

clauses := [].
clauses := [~clause|~clauses].
clauses := ~clause.

:- doc(doinclude,clause/1).

%% :- typedef clause ::= clterm ; ^((clterm,any)) .
:- regtype clause/1.

clause := ~clterm.
clause := ^((~clterm,~term)).

:- doc(doinclude,clterm/1).

%% :- typedef clterm ::= ^clause(goal,body) ; ^directive(body)
%% 	            ; ^((goal:-body)) ; goal .
:- regtype clterm/1.

clterm := clause(~callable,~body).
clterm := directive(~body).
clterm := (~callable :- ~body).
clterm := ~callable.

:- doc(doinclude,body/1).
:- doc(body/1,"A well formed body, including cge expressions and
   &-concurrent expressions. The atomic goals may or may not have
   a key in the form @tt{^(goal:any)}, and may or may not be
   module qualified, but if they are it has to be in the form
   @tt{^(^(moddesc:goal):any)}.").

:- regtype body(X)
	# "@var{X} is a printable body.".

body(X):- body(X).

:- doc(doinclude,flag/1).
:- doc(flag/1,
	"A keyword @tt{ask/1} flags whether to output @em{asks} or
         @em{whens} and @tt{nl/1} whether to separate clauses with
	 a blank line or not.").
:- regtype flag(X)
	# "@var{X} is a flag for the pretty-printer.".
flag(ask(A)):- ok_ans(A).
flag(nl(B)):- ok_ans(B).

:- regtype ok_ans(X)
	# "@var{X} is an answer for yes/no questions.".
ok_ans(yes).
ok_ans(no).

% check hooks:
curr_hooks(L,A,B):- hooks(L,no,A,no,B).

hooks([],A,A,B,B).
hooks([X|Xs],A0,A,B0,B):-
	hook(X,A0,B0,A1,B1),
	hooks(Xs,A1,A,B1,B).

hook(ask(A),_A,B,A,B):- ok_ans(A).
hook(nl(B),A,_B,A,B):- ok_ans(B).

%% -----------------------------------------------------------------------

pretty_print(Cls,Hooks):-
	pretty_print(Cls,Hooks,_nodict).

pretty_print(Cls,Hooks,Ds):-
	current_output(CO),
	pretty_print(CO,Cls,Hooks,Ds).

pretty_print(S,Cls,Hooks,Ds):-
	curr_hooks(Hooks,Key,Nl),
	pretty_printK(S,Cls,Ds,Key,Nl).

pretty_printK(_,[],_Ds,_Key,_Nl):- !.
pretty_printK(S,[(Cl,_)|T],[D|Ds],Key,Nl) :- !,
	\+ \+ pretty_print0(S,Cl,D,Key,Nl),
	pretty_printK(S,T,Ds,Key,Nl).
pretty_printK(S,[Cl|T],[D|Ds],Key,Nl) :- !,
	\+ \+ pretty_print0(S,Cl,D,Key,Nl),
	pretty_printK(S,T,Ds,Key,Nl).
pretty_printK(S,(Cl,_),D,Key,Nl) :- !,
	\+ \+ pretty_print0(S,Cl,D,Key,Nl).
pretty_printK(S,Cl,D,Key,Nl) :- 
	\+ \+ pretty_print0(S,Cl,D,Key,Nl).

pretty_print0(S,Cl,D,K,N):- var(D), !,
	numbervars(Cl,0,_),
	pp(Cl,K,S),
	nl(S),
	separator(N,S).
pretty_print0(S,Cl,D,K,N):-
	rename(Cl,D),
	pp(Cl,K,S),
	nl(S),
	separator(N,S).

separator(yes,S):- nl(S).
separator(no,_).

pp(directive(D),_K,S):- !,
	write(S,':- '), 
	writeq(S,D),
	write(S,'.').
pp((H :- B),K,S):- !,
	pp(clause(H,B),K,S).
pp(clause(H,true),_K,S):- !,
	writeq(S,H),
	write(S,'.').
pp(clause(H,!),_K,S):- !,
	writeq(S,H),
 	write(S,' :- !.').
pp(clause(H,B),K,S):- !,
	writeq(S,H),
	write(S,' :-'), nl(S),
	ppb(B,8,K,S),
	write(S,'.').
pp(H,K,S):-
	pp(clause(H,true),K,S).

ppb((A,B),Tab,K,S) :- !,
	ppb(A,Tab,K,S),
	write(S,','), nl(S),
	ppb(B,Tab,K,S).
ppb('&'(A,B),Tab,K,S) :- !,
	ppc(A,Tab,K,S),
	write(S,' &'), nl(S),
	ppc(B,Tab,K,S).
% ppb('&>'(A,B),Tab,K,S) :- !,
% 	ppc(A,Tab,K,S),
% 	write(S,' &> '),
% 	ppb(B,0,K,S).
% ppb('<&'(A),Tab,K,S) :- !,
% 	ppc(A,Tab,K,S),
% 	write(S,' <&').
ppb(('&'(A)),Tab,K,S) :- !,
	ppb(A,Tab,K,S),
	write(S,' &').
ppb(true(B),Tab,K,S) :- !,
	tab(S,Tab), write(S,'true('), nl(S),
	NTab1 is Tab+4,
	NTab2 is Tab+7,
	ppc(B,NTab2,K,S), nl(S),
	tab(S,NTab1), write(S,')').
ppb((A->B;C),Tab,K,S) :- !,
	tab(S,Tab), write(S,'('), nl(S),
	NTab1 is Tab+2,
	NTab2 is Tab+5,
	ppb(A,NTab1,K,S),
	write(S,' ->'), nl(S),
	ppb(B,NTab2,K,S), nl(S),
	tab(S,Tab), write(S,';'), nl(S),
	ppb(C,NTab2,K,S), nl(S),
	tab(S,Tab), write(S,')').
ppb((A->B),Tab,K,S) :- !,
	tab(S,Tab), write(S,'('), nl(S),
	NTab1 is Tab+2,
	NTab2 is Tab+5,
	ppb(A,NTab1,K,S),
	write(S,' ->'), nl(S),
	ppb(B,NTab2,K,S), nl(S),
	tab(S,Tab), write(S,')').
ppb((A;B),Tab,K,S) :- !,
	tab(S,Tab), write(S,'('), nl(S),
	NTab is Tab+5,
	ppb(A,NTab,K,S), nl(S),
	tab(S,Tab), write(S,';'), nl(S),
	ppb(B,NTab,K,S), nl(S),
	tab(S,Tab), write(S,')').
ppb('=>'(A,B),Tab,K,S) :- !,
	tab(S,Tab), write(S,'('), nl(S),
	NTab is Tab+5,
	ppb(A,NTab,K,S), nl(S),
	tab(S,Tab), write(S,'=>'), nl(S),
	ppb(B,NTab,K,S), nl(S),
	tab(S,Tab), write(S,')').
% Not anymore!!!
%% ppb(A:_,Tab,K) :- !,
%%  	ppg(A,Tab,K).
ppb(A,Tab,K,S) :-
 	ppg(A,Tab,K,S).

ppc('&'(A,B),Tab,K,S) :- !,
	ppc(A,Tab,K,S),
	write(S,' &'), nl(S),
	ppc(B,Tab,K,S).
% ppc('&>'(A,B),Tab,K,S) :- !,
% 	ppc(A,Tab,K,S),
% 	write(S,' &> '),
% 	ppb(B,0,K,S).
% ppc('<&'(A),Tab,K,S) :- !,
% 	ppc(A,Tab,K,S),
% 	write(S,' <&').
ppc(X,Tab,K,S) :-
	functor(X,F,2),
%	( F=',' ; F='=>' ; F=';' ; F='->' ), !,
% for the rest, '(' is written by ppb itself:
	F=',' , !,
	tab(S,Tab), write(S,'('), nl(S),
	NTab is Tab+1,
	ppb(X,NTab,K,S), nl(S),
	tab(S,Tab), write(S,')').
ppc(A,Tab,K,S) :-
	ppb(A,Tab,K,S).

% when/2 to a guarded ask
ppg(when(A,B),Tab,yes,S) :- !,
	ppb((ask(A)->B),Tab,ask,S),
	write(S,' & ').
% complex-ask with an &
ppg(ask(A,B),Tab,yes,S) :- !,
	tab(S,Tab),
	writeq(S,ask(A,B)),
	write(S,' & ').
% simple or qualified atomic goal
ppg(A,Tab,_K,S) :-
	functor(A,F,_),
	current_op(X,_,F),
	current_op(Y,_,','),
	X >= Y, !,
	tab(S,Tab),
	write(S,'( '), writeq(S,A), write(S,' )').
ppg(A,Tab,_K,S) :-
	tab(S,Tab),
	writeq(S,A).

