% The pcpe runtime-query package
:- use_package(assertions).

:- doc(title,"PCPE runtime query specification").
:- doc(author,"Claudio Ochoa").

:- doc(module, "This library package provides syntax allowing to
	specify runtime queries, needed for evaluating the multiple
	solutions provided by PCPE when time efficiency is being
	measured").

:- decl pcpe_rtquery(Query) # "Declares a runtime query to be run when
	evaluating the specialized programs obtained by PCPE when
	measuring time-efficiency. @var{Query} should be a predicate
	defined in the current program, called with partially
	instantiated arguments. 

   @bf{Example:}

@begin{verbatim} 
:- pcpe_rtquery(test(A,1,2,3)).  
@end{verbatim}

".
