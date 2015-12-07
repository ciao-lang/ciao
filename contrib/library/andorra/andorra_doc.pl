:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Andorra execution").  

:- doc(author, "Claudio Vaucheret").
:- doc(author, "Francisco Bueno").

:- doc(module,

"This package allows the execution under the Basic Andorra Model
@cite{andorra-principle}. The model classifies goals as a
@index{determinate goal}, if at most one clause matches the goal, or
nondeterminate goal, otherwise. In this model a goal is delayed until
either it becomes determinate or it becomes the leftmost goal and no
determinate goal is available. The implementation of this selection
rule is based on the use of attributed variables
@cite{holzbaur-plilp92,holzbaur-phd}.

In order to test determinacy we verify only the heads of clauses and
builtins in the bodies of clauses before the first cut, if any. 
By default, determinacy of a goal is detected dynamically: when called,
if at most one clause matches, it is executed; otherwise, it is 
delayed. For goals delayed the test is repeated each time a variable 
appearing in the goal is instantiated.
In addition, efficiency can be improved by using declarations
that specify the determinacy conditions. These will be considered
for testing instead of the generic test on all clauses that can match.

As with any other Ciao package, the andorra computation rule affects
only the module that uses the package. If execution passes across
two modules that use the computation rule, determinate goals are run
in advance @em{within} one module and also within the other module.
But determinate goals of one module do not run ahead of goals of the
other module.

It is however possible to preserve the computation rule for calls to
predicates defined in other modules. These modules should obviously also
use this package. In addition @em{all} predicates from such modules should
imported, i.e., the directive @tt{:- use_module(module)}, should be used in
this case instead of @tt{:- use_module(module,[...])}.  Otherwise calls to
predicates outside the module will only be called  when they became the leftmost goal.
").

:- include(library(andorra/andorraops)).

:- doc(determinate(Pred,Cond),"Declares determinacy conditions for a
	predicate. Conditions @var{Cond} are on variables of arguments
        of @var{Pred}. For example, in:
@begin{verbatim}
:- determinate(member(A,B,C), ( A ?\= term(B,[1])  ; C?\=[_|_]) ).

member(A,[A|B],B).
member(A,[B|C],[B|D]) :-
        A\==B,
        member(A,C,D).
@end{verbatim}
        the declaration states that a call @tt{member(A,B,C)} is
        determinate when either @tt{A} doesn't unify with the first
        argument of @tt{B} or @tt{C} doesn't unify with @tt{[_|_]}.").
:- decl determinate(Pred,Cond) : predname * detcond
      # "States that the predicate @var{Pred} is determinate 
         when @var{Cond} holds.".

:- doc(doinclude,detcond/1).
:- doc(detcond/1,"Defined by: @includedef{detcond/1}
@begin{itemize}
@item @tt{ground/1} and @tt{nonvar/1} have the usual meaning.
@item @tt{instatiated(A,Path)} means that the subterm of @tt{A}
        addressed by @tt{Path} is not a variable. @tt{Path} is a 
        list of integer numbers describing a path to the subterm 
        regarding the whole term A as a tree. For example,
        @tt{instantiated(f(g(X),h(i(Z),Y)),[2,1])} tests whether 
        @tt{i(Z)} is not a variable. 
@item @tt{Term1 ?\\= Term2} means ``terms @tt{Term1} and @tt{Term2}
        do not unify (when instantiated)''. @tt{Term1} and @tt{Term2}
        can be either an argument of the predicate or a term 
        @tt{term(V,Path)}, which
        refers to the subterm of @tt{V} addressed by @tt{Path}. 
@item @tt{Term1 ?= Term2} means ``terms @tt{Term1} and @tt{Term2}
        unify (when instantiated)''. The same considerations above
        apply to @tt{Term1} and @tt{Term2}.
@item any other test that does not unify variables can also be used
        ( @tt{==/2}, @tt{\\==/2}, @tt{atomic/1}).
@end{itemize}").

:- prop detcond(X) + regtype
      # "@var{X} is a determinacy condition.".

detcond(ground(X)):- var(X).
detcond(nonvar(X)):- var(X).
detcond(instatiated(A,Path)):- var(A), list(Path,int).
detcond(Term1 ?\= Term2):- path(Term1), path(Term2).
detcond(Term1 ?= Term2):- path(Term1), path(Term2).
detcond(Test):- test(Test).

:- doc(hide,test/1).

:- doc(doinclude,path/1).
:- doc(path/1,"Defined by: @includedef{path/1}").
:- prop path/1 + regtype.

path(X):- var(X).
path(X):- list(X,int).

:- doc(appendix,"The andorra transformation will include the 
   following predicates into the code of the module that uses the
   package. Be careful not to define predicates by these names:
   @begin{itemize}
   @item @tt{detcond_andorra/4}
   @item @tt{path_andorra/4}
   @item @tt{detcond_susp/4}
   @item @tt{path_susp/4}
   @item @tt{list_andorra2/5}
   @item @tt{test_andorra2/4}
   @end{itemize}").
