:- module(attributes, [], [noprelude, assertions, nortchecks]).

:- doc(title, "Low-level attributed variables").

:- doc(author,"Christian Holzbaur").
:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").

:- doc(module, "These predicates allow the manipulation of
   @index{attributed variables}. Attributes are special terms which
   are attached to a (free) variable, and are hidden from the normal
   Prolog computation. They can only be treated by using the
   predicates below.

   @begin{note}
   Direct usage of this module is discouraged. Prefer package @lib{attr}.
   @end{note}
   ").

:- doc(summary, "This library implements @index{attributed variables},
   which provide a mechanism for extensible unification. The library
   is based on the DMCAI CLP Library by Christian Holzbaur.").

:- doc(copyright,"Original copyright @copyright{} 1992 DMCAI

Department of Medical Cybernetics and Artificial Intelligence 
University of Vienna 
Freyung 6 
A-1010 Vienna, Austria").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_basic)).

:- if(defined(optim_comp)).
:- '$native_include_c_source'(.(attributes)).
:- endif.

:- export(attach_attribute/2).
:- trust pred attach_attribute(Var,Attr) : var * nonvar => var * nonvar
   # "Attach attribute @var{Attr} to @var{Var}.".
:- if(defined(optim_comp)).
:- '$props'(attach_attribute/2, [impnat=cblt(bu2_attach_attribute,0)]).
:- else.
attach_attribute(X, Y) :- attach_attribute(X, Y). % (compiled inline, hook for interpreter)
:- endif.

:- if(defined(optim_comp)).
% JFMC: I added this to avoid garbage collection problems
:- export(attach_attribute_weak/2). 
:- trust pred attach_attribute_weak(Var,Attr) : var * nonvar => var * nonvar
   # "Attach attribute @var{Attr} to @var{Var} (but delete unbound CVAs in GC).".
:- '$props'(attach_attribute_weak/2, [impnat=cblt(bu2_attach_attribute_weak,0)]).
:- endif.

:- export(get_attribute/2).
:- trust pred get_attribute(Var,Attr) : var(Var) => nonvar(Attr)
   # "Unify @var{Attr} with the attribute of @var{Var}, or fail if
   @var{Var} has no attribute.".
:- if(defined(optim_comp)).
:- '$props'(get_attribute/2, [impnat=cfunre(fu1_get_attribute,no)]).
:- else.
get_attribute(X, Y) :- get_attribute(X, Y). % (compiled inline, hook for interpreter)
:- endif.

:- export(update_attribute/2).
:- trust pred update_attribute(Var,Attr) : var * nonvar => var * nonvar
   # "Change the attribute of attributed variable @var{Var} to
   @var{Attr}.".
:- if(defined(optim_comp)).
:- '$props'(update_attribute/2, [impnat=cblt(bu2_update_attribute,0)]).
:- else.
update_attribute(X, Y) :- update_attribute(X, Y). % (compiled inline, hook for interpreter)
:- endif.

:- export(detach_attribute/1).
:- trust pred detach_attribute(Var) : var => var
   # "Take out the attribute from the  attributed variable @var{Var}.".
:- if(defined(optim_comp)).
:- '$props'(detach_attribute/1, [impnat=cblt(bu1_detach_attribute,0)]).
:- else.
detach_attribute(X) :- detach_attribute(X). % (compiled inline, hook for interpreter)
:- endif.

:- multifile verify_attribute/2.
:- trust pred verify_attribute(Attr, Term): nonvar * nonvar => nonvar * nonvar
   # "@em{A user defined predicate.} This predicate is called when an
   attributed variable with attribute @var{Attr} is about to be
   unified with the non-variable term @var{Term}.  The user should
   define this predicate (as multifile) in the modules implementing
   special unification.".

:- multifile combine_attributes/2.
:- trust pred combine_attributes(Var1, Var2): var * var => var * var
   # "@em{A user defined predicate.} This predicate is called when two
   attributed variables with attributes @var{Var1} and @var{Var2} are
   about to be unified.  The user should define this predicate (as
   multifile) in the modules implementing special unification.".

% ---------------------------------------------------------------------------

:- doc(appendix, "Note that @pred{combine_attributes/2} and
@pred{verify_attribute/2} are not called with the attributed variables
involved, but with the corresponding attributes instead.  The reasons
are:

@begin{itemize} 

@item There are simple applications which only refer to the
attributes.

@item If the application wants to refer to the attributed variables
themselves, they can be made part the attribute term.  The
implementation of @pred{freeze/2} utilizes this technique.  Note that
this does not lead to cyclic structures, as the connection between an
attributed variable and its attribute is invisible to the pure parts
of the Prolog implementation. 

@item If attributed variables were passed as arguments, the user
code would have to refer to the attributes through an extra call to
@pred{get_attribute/2}.

@item As the/one attribute is the first argument to each of the two
predicates, indexing applies. Note that attributed variables
themselves look like variables to the indexing mechanism.

@end{itemize}

However, future improvements may change or extend the interface to
attributed variables in order to provide a richer and more expressive
interface.

For customized output of attributed variables, please refer to the
documentation of the predicate @pred{portray_attribute/2}.  ").
