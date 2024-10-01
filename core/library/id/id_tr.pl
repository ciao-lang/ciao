:- module(_, [idclause/3,trbody/3],[assertions, datafacts]).

:- use_module(library(aggregates), [findall/4]).
:- use_module(library(lists), [append/3]).

:- data toexport/2.
:- data id_pred/2.
:- data id_call/2.
:- data only_first/1.

:- use_module(library(compiler/c_itf)).

idclause(end_of_file, Cls , _) :- 
	      !,
	      retractall_fact(id_call(_,_)),
              aggregates:findall(CL,
	                         (retract_fact(id_pred(H,B)),
	                          interpr(H,B,CL)
				 ),
				 Cls,Exportados),
             aggregates:findall((:- export(F/A)),
	                        retract_fact(toexport(F,A)),
				 Exportados,[end_of_file]),
	      retractall_fact(only_first(_)).

idclause((:- iterative(Call,Init,Formul)),
         (C :- '$iterdeep'(NC,Formul,unlimited)),_) :- 
	    !,
	    Call = H/A,
	    functor(C,H,A),
            C =.. [Name|L],
	    atom_concat('$$',Name,NewName),
	    NC =.. [NewName,0,Init,C,_|L],
	    asserta_fact(id_call(H,A)).

idclause((:- iterative(Call,Init,Formul,MaxDepth)),
         (C :- '$iterdeep'(NC,Formul,MaxDepth)),_) :- 
	    !,
	    Call = H/A,
	    functor(C,H,A),
            C =.. [Name|L],
	    atom_concat('$$',Name,NewName),
	    NC =.. [NewName,0,Init,C,_|L],
	    asserta_fact(id_call(H,A)).



idclause((:- D),(:- D),_):- 
	    !.	

idclause(0,[],_):-
	    !.

idclause((H :- B),Tr,_):- 
	    !,
	    functor(H,F,A),
	    ( id_call(F,A) -> Tr = []
	    ; Tr = (H :- B)),
            assertz_fact(id_pred(H, B)).

idclause((H),Tr,_):- 
            !,
    	    functor(H,F,A),
	    ( id_call(F,A) -> Tr = []
	    ; Tr = (H)),
            assertz_fact(id_pred(H,true)).




interpr(H,_,CL):-
	functor(H,F,A),
	(only_first(F) -> fail
	;
	 (asserta_fact(only_first(F)),
          NA is A + 4,
          atom_concat('$$',F,NF),
	  functor(NH,NF,NA),
	  arg(1,NH,Depth),
          arg(2,NH,Cut),
          arg(3,NH,Goal),        
          asserta_fact(toexport(NF,NA)),
          CL = (NH :- 
	       Depth > Cut,
               asserta_fact('$more_sol'(Goal)),
               !,fail)
         )
	).



interpr(H,B, (NH :-  '$$$replacebody'(B,Depth,Cut,Goal,DepthSol))) :- 
	H =.. [Name|L],
	atom_concat('$$',Name,NewName),
	NH =.. [NewName,Depth,Cut,Goal,DepthSol|L]. 
	

	

trbody(clause(H,('$$$replacebody'(B,Depth,Cut,Goal,DepthSol))),clause(H,Body),M):-
	interbody(B,NDepth,Cut,Goal,ListDepthSol,NB,Deeper,M),
	( Deeper == yes  -> Body = (NDepth is Depth + 1,(NB,'$max_depthsol'(ListDepthSol,DepthSol)))
	 ; Body = (NB,DepthSol = Depth) 
	 ).
	
	
interbody((A,B),Depth,Cut,Goal,ListDepthSol,(NA,NB),Deeper,M) :-
	!,
	interbody(A,Depth,Cut,Goal,LA,NA,Deeper,M), 
	interbody(B,Depth,Cut,Goal,LB,NB,Deeper,M),
	append(LA,LB,ListDepthSol).

interbody(true,_,_,_,[],true,_,_):- 
	!.

interbody(B,Depth,Cut,Goal,ListDepthSol,NB,Deeper,M) :-
        functor(B,_,A),
		  B =.. [Name|L],
		  atom_concat('$$',Name,NewName),
         NA is A + 4,
	( (defines(M,NewName,NA) ; imports(M,_,NewName,NA,_)) -> 
	      (
		  NB =.. [NewName,Depth,Cut,Goal,DepthSol|L],
                  ListDepthSol = [DepthSol], 
		  Deeper = yes
	      );
	    (NB = B,
	     ListDepthSol = [])
	). 
