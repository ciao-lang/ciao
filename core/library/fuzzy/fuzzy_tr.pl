:- module(fuzzy_tr,[fuzzy_pred/3,fuzzy_pred2/3],[]).

:- use_module(library(aggregates), [findall/4]).
:- use_module(library(terms), [copy_args/3]).
:- use_module(library(messages), [error_message/2]).

:- use_module(library(compiler/c_itf)).

:- data fpred/1.

:- data fclause/2.

:- data frule/3.

:- data fnegclause/3.
%:- data fagrclause/5.

:- data faggr/4.

:- data fdefault/2.

:- include(library(fuzzy/ops)).
%:- include(library(clpr/ops)).
:- include(library(clpqr/clpqr_ops)).

fuzzy_pred2(clause(H,'$add_contr'(B,T)),clause(H,NB),M):-
	         obtain_crisp(B,BPrime,M,T),
	         add_contr(H,BPrime,T,NB,M).
fuzzy_pred2(clause(H,('$add_neg'(O,A),B)),clause(H,B),M):-
	(
	    fpredicate(O/A,M) -> true
	;
	    error_message("~p is not a fuzzy predicate",[O/A])
	).



build_cl(X1,M1,X2,M2,Name,(H :- ExpMax , ExpMin  ),Comp):-
	M1 == M2,!,
	functor(H,Name,2),
	arg(2,H,M1),
	arg(1,H,X),
	( 
	    X1 =< X2 ->
	    ( 
		Comp == comp ->
		ExpMax = (X .=<. X2)
	    ;
		ExpMax = (X .<. X2)
	    ),
	    ExpMin = ( X.>=. X1)
	;
	    (
		Comp == comp ->
		ExpMin = ( X.>=. X2)
	    ;
		ExpMin = ( X.>. X2)
	    ),
	    ExpMax = (X .=<. X1)
	).

build_cl(X1,M1,X2,M2,Name,(H :- ExpMax , ExpMin, X1MX2 * M .=. M1MM2 * X + Resto),Comp):-
	functor(H,Name,2),
	arg(2,H,M),
	arg(1,H,X),
	X1MX2 is X1 - X2,
	M1MM2 is M1 - M2,
	Resto is M2 * X1 - M1 * X2,
	( 
	    X1 =< X2 ->
	    ( 
		Comp == comp ->
		ExpMax = (X .=<. X2)
	    ;
		ExpMax = (X .<. X2)
	    ),
	    ExpMin = ( X.>=. X1)
	;
	    (
		Comp == comp ->
		ExpMin = ( X.>=. X2)
	    ;
		ExpMin = ( X.>. X2)
	    ),
	    ExpMax = (X .=<. X1)
	).
	
build_cls(Name,[(X1,M1),(X2,M2)],[CL]):- 
	   build_cl(X1,M1,X2,M2,Name,CL,comp).

build_cls(Name,[(X1,M1),(X2,M2)|Lista],[CL|Cls]):-
	    build_cl(X1,M1,X2,M2,Name,CL,incomp),
	    build_cls(Name,[(X2,M2)|Lista],Cls).
	
add_fuzzy_clause(Cls,Cls1):-
	findall(c(H,B,T),retract_fact(frule(H,B,T)),L,[]),
	add_fuzzy_clause0(L,Cls,Cls1).

add_fuzzy_clause0([],Cls,Cls).
add_fuzzy_clause0([c(H,B,Type)|L],Cls1,Cls2):-
	Type == fact,!,
	assertz_fact(frule(H,B,Type)),
	add_fuzzy_clause0(L,Cls1,Cls2).
add_fuzzy_clause0([c(H,B,Type)|L],Cls,Cls2):-
	obtain_crisp2(B,Crisps,[]),
	assertz_fact(frule(H,B,Type)), % cambio
	fuzzyfing(Crisps,Cls,Cls1),
	add_fuzzy_clause0(L,Cls1,Cls2).

fuzzyfing([],C,C).
fuzzyfing([Name/Ar|Crisp],Cls,Cls2):-
	A is Ar + 1,
	atom_concat('$f_',Name,F),
	( 
	    fpred(F/A) ->  Cls1 = Cls
	;
	    assertz_fact(fpred(F/A)),
	    functor(N,Name,Ar),
	    functor(H,F,A),
	    copy_args(Ar,H,N),
	    arg(A,H,M),
	    Cls = [(H:-if(N,M = 1,M = 0))|Cls1]
	),
	fuzzyfing(Crisp,Cls1,Cls2).


obtain_crisp2((A,B),Crisps,Tail):-
	obtain_crisp2(A,Crisps,T1),
	obtain_crisp2(B,T1,Tail).

obtain_crisp2({_A},Crisp,Crisp).  % to manage crisp calls
obtain_crisp2(A,Crisp,Crisp):-
	functor(A,F,Ar),
	fpred(F/Ar),!.
obtain_crisp2(A,[Name/Ar|Crisp],Crisp):-
	functor(A,Name,Ar).



obtain_crisp((A,B),(NA,NB),M,T):-
	obtain_crisp(A,NA,M,T),
	obtain_crisp(B,NB,M,T).

obtain_crisp(A,A,_,fact). 
obtain_crisp({A},A,_,_).  % to manage crisp calls
obtain_crisp(A,A,M,_):-
	functor(A,F,Ar),
	fpredicate(F/Ar,M),!.
obtain_crisp(A,H,_,_):-
	functor(A,Name,Ar),
       	Ar1 is Ar + 1,
	atom_concat('$f_',Name,F),
	functor(H,F,Ar1),
	copy_args(Ar,H,A).

add_default_clause([],Cls,Cls).
add_default_clause([C],NewCs,Cls):-
	C = (HC :- _),
	functor(HC,F,A),
	functor(HD,F,A),
	arg(A,HD,M),
	(
	    fdefault(F/A,Default) ->
	    (
		Default == fail ->
		NewCs = [C|Cls]
	    ;
		(
		    Default = [X,Y] -> 
		    D = (HD :- M.>=.X,M.=<.Y),
		    NewCs = [C,D|Cls]
		;
		    (
			Default == unknown ->
			D = (HD :- M.>=.0,M.=<.1),
			NewCs = [C,D|Cls]
		    ;
			error_message("default value for predicate ~p is not well defined",[F])
		    )
		)
	    )
	;
	    D = (HD :- M.>=.0,M.=<.1),
	    NewCs = [C,D|Cls]
	).
add_default_clause([C1,C2|Clauses],[C1|Cls1],Negated):- 
	C1 = (HC1 :- _),
	C2 = (HC2 :- _),
	functor(HC1,F,A),
	functor(HC2,F,A),!,
	add_default_clause([C2|Clauses],Cls1,Negated).
add_default_clause([C1,C2|Clauses],NewCs,Negated):- 
	C1 = (HC :- _),
	functor(HC,F,A),
	functor(HD,F,A),
	arg(A,HD,M),
	(
	    fdefault(F/A,Default) ->
	    (
		Default == fail ->
		NewCs = [C1|Cls1]
	    ;
		(
		    Default = [X,Y] -> 
		    D = (HD :- M.>=.X,M.=<.Y),
		    NewCs = [C1,D|Cls1]
		;
		    (
			Default == unknown ->
			D = (HD :- M.>=.0,M.=<.1),
			NewCs = [C1,D|Cls1]
		    ;
			error_message("default value for predicate ~p is not well defined",[F])
		    )
		)
	    )
	;
	    D = (HD :- M.>=.0,M.=<.1),
	    NewCs = [C1,D|Cls1]
	),
	add_default_clause([C2|Clauses],Cls1,Negated).


fuzzy_pred(end_of_file,Cls,_) :-
	!,
%        assertz_fact(fpred('=>'/4)),
	add_fuzzy_clause(Cls,Cls1),

	findall(CL,(retract_fact(frule(H,B,T)),
	            CL = (H:-'$add_contr'(B,T)) 
%	            add_contr(H,B,T,CL)
		   ),
		   Clauses,[]
	       ),

        add_default_clause(Clauses,Cls1,Negated),

	findall(CL,(retract_fact(fnegclause(N,O,A)),
		 add_neg(N,O,A,CL)
		 ),
		   Negated,Is_fuzzy
	       ),
 	findall(CL,(retract_fact(fpred(F/A)),
 	            CL = (:- is_fuzzy(F,A,truth))
 		   ),
 		   Is_fuzzy,[end_of_file]
 	       ),
	       retractall_fact(fdefault(_,_)).


	
fuzzy_pred( (Name :# fuzzy_predicate Lista),Cls,_):-
	!,
	( 
	    assertz_fact(fpred(Name/2)),
	    list(Lista) ->
	     build_cls(Name,Lista,Cls)
	;
	    error_message("fuzzy predicate ~p is not well defined",[Name])
	).
	
fuzzy_pred((H := ),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,true,fact)).

fuzzy_pred((H := B),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,B,less)).

fuzzy_pred((H :~  ),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,true,fact)).

% explicit aggregator:
fuzzy_pred((H :~ B0),[],_):-
	nonvar(B0),
	functor(B0,Aggr,1),
	faggr(Aggr,_,_,_), !,
	arg(1,B0,B),
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,B,Aggr)).

fuzzy_pred((:- aggr A <# I ## M #> F ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,I,M,F)), !.

fuzzy_pred((:- aggr A ## M #> F ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,M,F)), !.

fuzzy_pred((:- aggr A <# I ## M ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,I,M,id)), !.

fuzzy_pred((:- aggr A <# I ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,I,A,id)), !.

fuzzy_pred((:- aggr A #> F ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,A,F)), !.

fuzzy_pred((:- aggr A ## M ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,M,id)), !.

fuzzy_pred((:- aggr A),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,A,id)), !.


% default
fuzzy_pred((H :~  B),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,B,min)).


fuzzy_pred((Npred :# fnot Opred/A),[],_):-
	!,
	(
	    fpred(Npred/A) -> true
	;
	    assertz_fact(fpred(Npred/A))
	),
	assertz_fact(fnegclause(Npred,Opred,A)).


fuzzy_pred(( F :# fuzzy Name/Ar ),[(H:-if(N,M = 1,M = 0))],_):-
	!,
	A is Ar + 1,
	assertz_fact(fpred(F/A)),
	functor(N,Name,Ar),
	functor(H,F,A),
	copy_args(Ar,H,N),
	arg(A,H,M).

fuzzy_pred((:- default(Name/Ar,Default)),[],_):-
	assertz_fact(fdefault(Name/Ar,Default)).

fuzzy_pred(A,A,_).


addconstrain([],Mu,(Mu .>=.0,Mu .=<.1)).
addconstrain([X|RestV],Mu,(X.>=.Mu,RestC)):-
	addconstrain(RestV,Mu,RestC).


add_contr(H,B,less,(B,AB),M):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[],M),
 	addconstrain(ListVar,Mu,AB).

add_contr(H,_B,fact,(Mu .>=.0,Mu .=<.1 ),_M):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu).

% add_contr(H,B,min,(B,AB),M):- !,
%  	functor(H,_,Ar),
%  	arg(Ar,H,Mu),
%  	memfunct(B,ListVar,[],M),
% 	AB = (minim(ListVar,Mu),Mu .>=.0,Mu .=<.1).

% add_contr(H,B,luka,(B,AB),M):- !,
%  	functor(H,_,Ar),
%  	arg(Ar,H,Mu),
%  	memfunct(B,ListVar,[],M),
% 	AB = (lukalist(ListVar,Mu),Mu .>=.0,Mu .=<.1).

% add_contr(H,B,prod,(B,AB),M):- !,
%  	functor(H,_,Ar),
%  	arg(Ar,H,Mu),
%  	memfunct(B,ListVar,[],M),
%         (
% 	    ListVar == [] ->
% 	    AB = (Mu .>=.0,Mu .=<.1)
% 	;
% 	    transprod(ListVar,Prod),
% 	    AB = (Mu .=. Prod,Mu .>=.0,Mu .=<.1)
% 	).

% add_contr(H,B,max,(B,AB),M):- !,
%  	functor(H,_,Ar),
%  	arg(Ar,H,Mu),
%  	memfunct(B,ListVar,[],M),
% 	AB = (maxim(ListVar,Mu),Mu .>=.0,Mu .=<.1).

% add_contr(H,B,dluka,(B,AB),M):- !,
%  	functor(H,_,Ar),
%  	arg(Ar,H,Mu),
%  	memfunct(B,ListVar,[],M),
% 	AB = (dlukalist(ListVar,Mu),Mu .>=.0,Mu .=<.1).

% add_contr(H,B,dprod,(B,AB),M):- !,
%  	functor(H,_,Ar),
%  	arg(Ar,H,Mu),
%  	memfunct(B,ListVar,[],M),
% 	AB = (dprodlist(ListVar,Mu),Mu .>=.0,Mu .=<.1).
 
add_contr(H,B,Any,(B,AB),M):-
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[],M),
	faggr(Any,IAny,MAny,FAny),
	AB = (preinject(ListVar,IAny,TempVar),
	inject(TempVar,MAny,MuTemp),
	postinject(TempVar,MuTemp,FAny,Mu),
	Mu .>=.0,Mu .=<.1).
 
add_neg(N,O,A,(NewPred :- '$add_neg'(O,A),OldPred,Mu .=. 1 - MuO)):-
%	(
%	    fpred(O/A) ->
	    functor(OldPred,O,A),
	    functor(NewPred,N,A),
	    Arm1 is A - 1,
	    copy_args(Arm1,OldPred,NewPred),
	    arg(A,NewPred,Mu),
	    arg(A,OldPred,MuO).
%	;
%	    error_message("~p is not a fuzzy predicate",[O/A])
%	).
			


% transprod([X],X).
% transprod([X|RestV],(X * RestP)):-
% 	transprod(RestV,RestP).

memfunct((A,B),ListVar,Tail,M):-
	!,
 	memfunct(A,ListVar,TListVar,M),
 	memfunct(B,TListVar,Tail,M).
%memfunct({_A},R,R,_M).  % to manage crisp calls
memfunct(A,[X|R],R,M):-
 	functor(A,F,Ar),
 	fpredicate(F/Ar,M),!,
 	arg(Ar,A,X).
memfunct(_,R,R,_).
% memfunct(_,R,R,_):-
% 	error_message("something is wrong",[]).


fpredicate(F/Ar,M):-
    defines_module(B,M),
    decl(B,is_fuzzy(F,Ar,_Type)).
fpredicate(F/Ar,M):-
    imports(M,_M2,F,Ar,M2),
    defines_module(B,M2),
    decl(B,is_fuzzy(F,Ar,_Type)).
