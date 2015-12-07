:- use_package(assertions).
:- doc(nodoc,assertions). 

:- doc( title, "Attributed Variables Package").

:- doc(author, "R@'{e}my Haemmerl@'{e}").  
:- doc(author, "Christian Holzbaur").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").

:- doc(module, "This package implements @concept{attributed variables}
in the style of Holzbaur @cite{holzbaur-phd}.  It provides a way to
associate to variables one or several arbitrary terms called
@concept{attributes}. By allowing the user to redefine the unification
of attributed variables, this extension makes possible the design of
coroutining facilities (see subsection @ref{Example}) and clean interfaces
between Prolog and constraints solvers. 

Attributes are private to module and each variable can have at most
one attribute in each module.  
Attributes are handled by predicate provided by @lib{attr_rt} module.


@lib{attr} package imports automatically attributes variables manipulation
predicates, @tt{put_attr_local/2}, @tt{get_attr_local/2}, and @tt{del_attr_local/2}
from module @tt{'attr/attr_rt'}, and set up the following hooks:

@begin{itemize} 

@item @tt{attr_unify_hook(AttValue, VarValue)}.

Hook that must be defined in the module using package @tt{attr}. It is
called after the attributed variable of that module has been unified
with a non-var term, possibly another attributed
variable. @var{AttValue} is the attribute that was associated to the
variable in this module and @var{VarValue} is the new value of the
variable. Normally this predicate fails to veto binding the variable
to @var{VarValue}, forcing backtracking to undo the binding. If
@var{VarValue} is another attributed variable the hook often combines
the two attribute and associates the combined attribute with
@var{VarValue} using @tt{attr_rt:put_attr_local/2}.

@item @tt{attribute_goal(Var, S0, S)}.

This optional hook, if it is defined, is used by
@tt{attr_rt:copy_term/3} to project attributes of that module to
residual goals, and by the toplevel to obtain residual goals after
executing a query. The predicate is supposed to unified @var{S0} with
a different list containing the residual goals and which have @var{S}
as tail. For the sake of simplicity, it can be defined using
@lib{dcg}.  (See DCG non-terminal @tt{attribute_goal(Var)} in example
below.)


@item @tt{attr_portray_hook(Attribute, Var)}.

Called by @pred{write_term/2} for each attribute associate to the
variable @var{Var} if the option option attributes(portray) is in
effect. If the hook succeeds the variable is considered to be printed
otherwise @tt{Module =... } is printed to indicate the existence of an
attribute defined in module @tt{Module}.  At call time @var{Attribute}
is the actual value of the attribute associate to the variables
@var{Var}.

@end{itemize}

@section{Example}

In the following example we give an implementation of
@tt{freeze/2}. We name it @tt{myfreeze/2} in order to avoid a name
clash with the built-in predicate of the same name. The code is 
available in the module @tt{library(attr/example/myfreeze)}

@begin{verbatim}
@includeverbatim{attr/examples/myfreeze.pl}
@end{verbatim}


").