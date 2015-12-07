:- module(_,[translation1/3,translation2/3],[]).

:- use_module(library(aggregates), [findall/4]).
:- use_module(library(terms_vars), [varsbag/3]).


:- use_module(library(compiler/c_itf)).


:- data pred_replace/2.
:- data pred_susp/2.
:- data toexport/2.
:- data todelay/2.
:- data pred_user/2.

:- include(library(andorra/andorra_builtins_exports)).




translation1(end_of_file,Cls,_) :- 
	      !,
	       aggregates:findall(CL,
	                          (pred_replace(F,A),
				  interpr(F,A,CL)
				  ),
				  Cls,Rest),
               aggregates:findall(CL,
 	                         (pred_replace(F,A),
 	                          interpr1(F,A,CL)
 				 ),
 				 Rest,Suspended),  
              aggregates:findall(CL,
 	                         (retract_fact(pred_susp(H,B)),
				  interpr2(H,B,CL)
				  ),
 				 Suspended,Delayed), 
               aggregates:findall(CL,
 	                         (retract_fact(todelay(F,A)),
	                          interpr3(F,A,CL)
 				 ),
 				 Delayed,Exported),  
              aggregates:findall((:- export(F/A)),
 	                        retract_fact(toexport(F,A)),
 				 Exported,[end_of_file]),
				 retractall_fact(pred_replace(_,_)),
				 retractall_fact(pred_user(_,_)).


translation1((:- determinate(H,true)),(NH :- HSusp),_):-
	!,
	functor(H,F,A),
	H =.. [F|Arguments],
        assertz_fact(pred_user(F,A)),
          NA is A + 3,
 	  atom_concat(F,'_andorra',NewF),
          asserta_fact(toexport(NewF,NA)),
 	  functor(NH,NewF,NA),
	  atom_concat(F,'_susp',NF),
          (A = 0 -> 
	   NH =.. [NewF,L,L1,In],
            HSusp =.. [NF,L,L1,In]
	  ;      
	   Arguments = [X|Args],   
	   NH =.. [NewF,X,L,L1,In|Args],
           HSusp =.. [NF,X,L,L1,In|Args]).


translation1((:- determinate(H,false)),(NH :- L= [S|L0],
                                     suspend_andorra(S,[],HSusp,false,builtin)),_):-
	!,
	functor(H,F,A),
	H =.. [F|Arguments],
        assertz_fact(pred_user(F,A)),
          NA is A + 3,
 	  atom_concat(F,'_andorra',NewF),
          asserta_fact(toexport(NewF,NA)),
 	  functor(NH,NewF,NA),
	  atom_concat(F,'_susp',NF),
          (A = 0 -> 
	   NH =.. [NewF,L,L1,In],
            HSusp =.. [NF,L0,L1,In]
	  ;      
	   Arguments = [X|Args],   
	   NH =.. [NewF,X,L,L1,In|Args],
           HSusp =.. [NF,X,L0,L1,In|Args]).


translation1((:- determinate(H,Cond)),(NH :- 
	                 simplify(Cond,NewCond),
			 (NewCond = true -> L=L0, HSusp ; 
			     L=[S|L0],
%			     varset(Cond,LS),
			     obtain_vars(NewCond,LS),!,
			     suspend_andorra(S,LS,HSusp,NewCond,builtin)
			     )),_):-

	!,
	functor(H,F,A),
	H =.. [F|Arguments],
        assertz_fact(pred_user(F,A)),
          NA is A + 3,
 	  atom_concat(F,'_andorra',NewF),
          asserta_fact(toexport(NewF,NA)),
 	  functor(NH,NewF,NA),
	  atom_concat(F,'_susp',NF),
          (A = 0 -> 
	   NH =.. [NewF,L,L1,In],
	    HOrig = F,
            HSusp =.. [NF,L0,L1,In]
	  ;      
	   Arguments = [X|Args],   
	   NH =.. [NewF,X,L,L1,In|Args],
	   HOrig =.. [F,X|Args],
           HSusp =.. [NF,X,L0,L1,In|Args]).


	

translation1((:- D),(:- D),_):- 
 	    !.	

translation1(0,[],_):-
 	    !.

translation1((H :- B),[],_):-
	!,
	functor(H,F,A),
        ( pred_replace(F,A) -> true ; assertz_fact(pred_replace(F,A))), 
        assertz_fact(pred_susp(H,B)).

translation1((H),[],_):-
	!,
	functor(H,F,A),
        ( pred_replace(F,A) -> true; assertz_fact(pred_replace(F,A))), 
        assertz_fact(pred_susp(H,true)).

interpr(F,A,(H :- NH,wakeup(L1,L2))):-
	functor(H,F,A),
	H =.. [Name|Args],
	atom_concat(Name,'_andorra',NewF),
        (Args = [X|T] -> NH =.. [NewF,X,L1,L2,L1|T] ;
	                 NH =.. [NewF,L1,L2,L1]).     

	

interpr3(F,A,(NH :- '$change',
	            L = [S|L1],
		    suspend_andorra(S,[],HOrig,false,builtin)
	             )):-
          NA is A + 3,
 	  atom_concat(F,'_andorra2',NewF), %.. rra2
      	  functor(NH,NewF,NA),
          (A = 0 -> 
	   arg(1,NH,L),
	   arg(2,NH,L1),
	   HOrig = F
	  ;                                    
	      arg(2,NH,L),
	      arg(3,NH,L1),
	      NH =.. [_,X,_,_,_|Args],
	      HOrig =.. [F,X|Args]).

interpr1(F,A,NewClause):- 
          \+ pred_user(F,A),
	  findall(p(Hsusp,Bsusp),(pred_susp(Hsusp,Bsusp),functor(Hsusp,F,A)),ListClaus,[]),
	  ( 
	      ListClaus = [_] ->
	      NewClause = (NH :- L = L0,HSusp)
	  ;
	      NewClause = (NH :- 
	      verify_det(HOrig,LGyCs,NewLGyCs),
	      ( NewLGyCs = [] -> fail ;
		  ( NewLGyCs = [_] -> L = L0,HSusp ;
		      L = [S|L0],
		      varset(HOrig,LS),
		      suspend_andorra(S,LS,HSusp,HOrig,LGyCs),!
		  )
	      ))

	  ),
          NA is A + 3,
 	  atom_concat(F,'_andorra',NewF),
          asserta_fact(toexport(NewF,NA)),
 	  functor(NH,NewF,NA),
	  atom_concat(F,'_susp',NF),
          (A = 0 -> 
	   arg(1,NH,L),
	   arg(2,NH,L1),
	   arg(3,NH,In),
	   HOrig = F,
	   HSusp =.. [NF,L0,L1,In]
	  ;                                    
	      arg(2,NH,L),
	      arg(3,NH,L1),
	      arg(4,NH,In),
	      NH =.. [_,X,_,_,_|Args],
	      HOrig =.. [F,X|Args],
	      HSusp =.. [NF,X,L0,L1,In|Args]
	  ),
	  obtain_clauses(F,A,L0,L1,LGyCs,In).

          
obtain_clauses(F,A,L0,L1,Cls,In):-
	findall(guard_clause(H,_R,B),(pred_susp(H,B),
	                           functor(H,F,A)),Cls1,[]),
	replace_bodies(Cls1,Cls,L0,L1,In).			   


replace_bodies([],[],_,_,_).
replace_bodies([guard_clause(H,_R,B)|Rest],[guard_clause(H,R)|NRest],L0,L1,In):- 
	prepare_body(H,B,L0,L1,_NB,R,In),
	replace_bodies(Rest,NRest,L0,L1,In).

prepare_body(_,true,L0,L1,(L0 = L1),[],_):-
	!.
prepare_body(H,B,L0,L1,NB,R,In):- 
	interprbody(H,B,L0,L1,NB,R,_FC,In).




interpr2(H,true,NH):-   
	 !,
         H =.. [Name|Arg],
 	 atom_concat(Name,'_susp',NewName),
	 (Arg = [X|T] -> 
	  NH =.. [NewName,X,L,L,_In|T] 
	 ;
	     NH =.. [NewName,L,L,_In]),
	 functor(NH,F,Ar),
         asserta_fact(toexport(F,Ar)).


interpr2(H,B,(NH :- NB)):- 
	H =.. [Name|Arg],
	atom_concat(Name,'_susp',NewName),
	(Arg = [X|T] -> 
	 NH =.. [NewName,X,L1,Lnp1,In|T]
	;
	    NH =.. [NewName,L1,Lnp1,In]
	),     
        functor(NH,F,Ar),
        asserta_fact(toexport(F,Ar)),
	interprbody(H,B,L1,Lnp1,NB,_,_,In).


interprbody(_,true,_,_,true,[],_,_):-
	!.
interprbody(H,(A,B),L1,Lnp1,(NA,NB),R,FirstCut,In):-
	!,
	A =.. [Name|Arg],
	functor(A,F,Ar),
            
	( sensitive(F/Ar) 
	-> 
	NA = (wakeup(In,L1),L1 = L2,A),
	NIn = L1 
	;
	    NIn = In,
	    ( unchained(F/Ar)
	    -> 
	    NA = (L1 = L2,A)
	    ;
%		atom_concat(Name,'_andorra',NewName),
                (definite(F/Ar) -> atom_concat(Name,'_andorra',NewName);atom_concat(Name,'_andorra2',NewName)),
		(Arg = [X|T] -> 
		 NA =.. [NewName,X,L1,L2,In|T]
		;
		    NA =.. [NewName,L1,L2,In]
		)
	    )
        ),
	( var(FirstCut),
	  is_test(F/Ar),
	  depends(A,H)
        -> 
	R = [A|Rest] 
        ;
	   R = Rest
        ),
	( definite(F/Ar)
	-> true 
	; (todelay(F,Ar) -> true 
	  ; 
	      assertz_fact(todelay(F,Ar))
	  )
	), 
	( Name == ! -> FirstCut = after ; true),   
        interprbody(H,B,L2,Lnp1,NB,Rest,FirstCut,NIn).
        
interprbody(H,A,L1,Lnp1,NA,R,FirstCut,In):-
	 A =.. [Name|Arg],
        functor(A,F,Ar),              
	( sensitive(F/Ar) 
	-> 
	NA = (wakeup(In,L1),L1=Lnp1,A)
	;
	    ( unchained(F/Ar)
	    -> 
	    NA = (L1=Lnp1,A)
	    ;
%		atom_concat(Name,'_andorra',NewName),
                (definite(F/Ar) -> atom_concat(Name,'_andorra',NewName);atom_concat(Name,'_andorra2',NewName)),

		(Arg = [X|T] 
		-> 
		NA =.. [NewName,X,L1,Lnp1,In|T]
		; 
		    NA =.. [NewName,L1,Lnp1,In]
		)
	    )
	),
	( var(FirstCut),
	  is_test(F/Ar),
       	  depends(A,H)
	-> 
	R = [A]
	; R = []
	),
	(definite(F/Ar)
	-> true
	; 
	    (todelay(F,Ar) -> true ; assertz_fact(todelay(F,Ar))
	    )
	).


sensitive(F/Ar):- 
	sensitive_builtin(_,F,Ar,_), 
	  \+ pred_replace(F,Ar).

unchained(F/Ar):-
	determinate_builtin(_,F,Ar,_),
	\+ pred_replace(F,Ar) .


definite(F/Ar):-  pred_replace(F,Ar).
definite(F/Ar):-  builtin_export(_,F,Ar,_).
definite(F/Ar):-  determinate_builtin(_,F,Ar,_).
definite(F/Ar):-  sensitive_builtin(_,F,Ar,_).  

is_test(F/Ar):- 
	  builtin_constrain(_,F,Ar,_),
	  \+ pred_replace(F,Ar). 


depends(A,H):- 
	varsbag(A,Vsa,[]),
	varsbag(H,Vsh,[]),
	nondisjuncts(Vsa,Vsh).

nondisjuncts([H|_],Vsh):-
	is_in(H,Vsh),!.
nondisjuncts([_|T],Vsh):-
	nondisjuncts(T,Vsh).

is_in(H,[Y|_]):- 
	H==Y,!.
is_in(H,[_|R]):-
	is_in(H,R).



translation2(clause(H,('$change',B)),clause(H,Body),_):-
	functor(H,F,A),
	H =.. [_|Args],
	atom_concat(NF,'2',F),
	(imports(_,M,NF,A,_) 
%	(imports(_,M,F,A,_)
	-> 
	NH =.. [NF|Args],
	Body =  M:NH
%	Body = M:H
	;
	    Body = B).
