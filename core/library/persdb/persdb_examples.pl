:- use_package([assertions]).

:- doc(title,"Using the persdb library").

:- doc(author,"The CLIP Group").

:- doc(module,"Through the following examples we will try to
	illustrate the two mains ways of declaring and using
	persistent predicates: statically (the preferred method) and
	dynamically (necessary when the new persistent predicates have
	to be defined at run-time). The final example is a small
	application implementing a simple persistent queue.

        @comment{main applications of the persistent predicates. These
	are using persistent predicates from a prolog @concept{top level}
	and using them in @concept{standalone programs}. Both share 
	the same base which is achieving persistency in a straight forward,
	transparent way but with a subtle difference, which is the update
	of the persistence set. 

        In a standalone program the persistence set is updated each time
        the program is run but, in a top level it only happens whenever 
        the top level is started. This is due to the fact of the top level
        being the executable program and not the program itself. In fact, 
	this is the most logical way to do it because as long as there is 
        a top level holding the state of the persistent predicates, there 
        is no need to waste machine resources making updates. 

        Anyway, if a program launched from a top level needs to update 
        the persistence sets of any persistent predicate it can be done 
        by using the methods @pred{update_files/0} and @pred{update_files/1}.}

@section{An example of persistent predicates (static version)}

@begin{verbatim}
@includeverbatim{persdb/examples/example_static.pl}
@end{verbatim}

@section{An example of persistent predicates (dynamic version)}

@begin{verbatim}
@includeverbatim{persdb/examples/example_dynamic.pl}
@end{verbatim}

@section{A simple application / a persistent queue}
@begin{verbatim}
@includeverbatim{persdb/examples/queue.pl}
@end{verbatim}

").

main.
