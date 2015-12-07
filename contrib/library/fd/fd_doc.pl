:- use_package([assertions,regtypes, isomodes]).
:- doc(nodoc,assertions).
:- doc(nodoc,regtypes).
:- doc(nodoc,isomodes).

:- doc(title, "Constraint programming over finite domains").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").
:- doc(author, "Manuel Carro").
:- doc(copyright,"@include{DocCopyright.lpdoc}").



:- doc(summary, "An implementation of constraint programming over
 finite domains.").

:- doc(module,  

"

@begin{alert}
This package is not anymore maintained. Prefer package
@lib{clpfd} which provides similar features.
@end{alert}

This package is a very preliminary implementation of a finite domain
solver. Examples can be found in the source and library directories.

@begin{itemize}
@item SEND + MORE = MONEY:
@end{itemize}

@begin{verbatim}
@includeverbatim{fd/examples/smm.pl}
@end{verbatim}

@begin{itemize}
@item Queens:
@end{itemize}

@begin{verbatim}
@includeverbatim{fd/examples/queensfd.pl}
@end{verbatim}

").

:- include(fd_syntax).

:- doc(doinclude, fd_item/1).

:- prop fd_item(FD_item) + regtype # "@var{FD_item} is a finite domain
entity, i.e. either a finite domains variable or an integer.".

fd_item(FD_item) :- var(FD_item).
fd_item(FD_item) :- int(FD_item).

:- doc(doinclude, fd_range/1).

:- prop fd_range(FD_range) + regtype # "@var{FD_range} is the range of
a finite domain entity.".

fd_range(R) :- list(R, fd_subrange).

:- doc(doinclude, fd_subrange/1).

:- prop fd_subrange/1 + regtype # "A subrange is a pair representing a
single interval.".

fd_subrange([Lower|Upper]) :-
	int(Lower),
	int(Upper).

:- doc(doinclude, fd_store/1).
 
:- prop fd_store(FD_store) + regtype # "@var{FD_store} is a
representation of the constraint store of a finite domain entity.".

fd_store(S) :- list(S, fd_store_entity).

:- doc(doinclude, fd_store_entity/1).
:- prop fd_store_entity/1 + regtype # "Representation of primitive constraints. ".

fd_store_entity(min).
fd_store_entity(max).
fd_store_entity(min_plus_c).
fd_store_entity(max_plus_c).
fd_store_entity(min_sub_c).
fd_store_entity(max_sub_c).
fd_store_entity(min_mult_c).
fd_store_entity(max_mult_c).
fd_store_entity(min_sub_max).
fd_store_entity(max_sub_min).
fd_store_entity(min_plus_min).
fd_store_entity(max_plus_max).
fd_store_entity(min_mult_min).
fd_store_entity(max_mult_max).
fd_store_entity(min_div_max).
fd_store_entity(max_div_min).

:- pred labeling(Vars) : list(fd_item) # "Implements the labeling
process. Assigns values to the input variables @var{Vars}. On exit all
variables are instantiated to a consistent value. On backtracking,
the predicate returns all possible assignments. No labeling heuristics
implemented so far, i.e. variables are instantiated in their order of
appearance.".

labeling(_L).

:- pred pitm(+V, -MiddlePoint) : fd_item * int # "Returns in
@var{MiddlePoint} the intermediate value of the range of @var{V}. In
case @var{V} is a ground integer value the returned value is @var{V}
itself.".

pitm(_V, _Point).

:- pred choose_var(+ListOfVars, -Var, -RestOfVars) : list(fd_item) *
fd_item * list(fd_item) # "Returns a finite domain item @var{Var} from
a list of fd items @var{ListOfVars} and the rest of the list
@var{RestOfVars}in a deterministic way. Currently it always returns
the first item of the list.".

choose_var(_ListOfVars, _Var, _RestOfVars).

:- pred choose_free_var(+ListOfVars, -Var) : list(fd_item) * var #
"Returns a free variable @var{Var} from a list of fd items
@var{ListOfVars}. Currently it always returns the first free variable
of the list.".

choose_free_var(_ListOfVars, _Var).

:- pred choose_var_nd(+ListOfVars, -Var) : list(fd_item) * fd_item #
"Returns non deterministically an fd item @var{Var} from a list of fd
items @var{ListOfVars} .".

choose_var_nd(_ListOfVars, _Var).

:- pred choose_value(+Var, -Value) : fd_item * int # "Produces an
integer value @var{Value} from the domain of @var{Var}. On
backtracking returns all possible values for @var{Var}.".

choose_value(_Var, _Value).

:- pred retrieve_range(+Var, -Range) : var * fd_range # "Returns in
@var{Range} the range of an fd item @var{Var}.".

retrieve_range(_Var, _Range).

:- pred retrieve_store(+Var, -Store) : var * fd_store # "Returns in
@var{Store} a representation of the constraint store of an fd item
@var{Var}.".

retrieve_store(_Var, _Store). 

:- pred glb(+Var, -LowerBound) : fd_item * int # "Returns in
@var{LowerBound} the lower bound of the range of @var{Var}.".

glb(_Var, _LowerBound).

:- pred lub(+Var, -UpperBound) : fd_item * int # "Returns in
@var{UpperBound} the upper bound of the range of @var{Var}.".

lub(_Var, _UpperBound).

:- pred bounds(+Var, -LowerBound, -UpperBound) : fd_item * int * int #
"Returns in @var{LowerBound} and @var{UpperBound} the lower and upper
bounds of the range of @var{Var}.".

bounds(_Var, _LowerBound, _UpperBound).

:- pred retrieve_list_of_values(+Var, -ListOfValues) : fd_item *
list(int) # "Returns in @var{ListOfValues} an enumeration of al the
values in the range of @var{Var}".

retrieve_list_of_values(_Var, _ListOfValues).
