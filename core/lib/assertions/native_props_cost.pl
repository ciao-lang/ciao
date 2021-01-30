% (included file)
:- doc(section, "Data sizes, cost, termination").

:- if(defined(optim_comp)).
:- else.
% TODO: @item @tt{me} (unsupported): The mean of @tt{ub} and @tt{lb}.
% TODO: @item @tt{ome} (unsupported): Complexity order of @tt{me}.
% TODO: @item @tt{olb} (supported in @lib{infercost} analysis): Omega.
% TODO: @item @tt{oub} (supported in @lib{infercost} analysis): Big O.
:- export(approx/1).
:- doc(approx/1,"The types of approximations (bounding) supported by the size
   and cost analyses (see also @lib{resources_basic.pl}).

   @begin{itemize}
   @item @tt{ub}: for upper bounds.
   @item @tt{lb}: for lower bounds.
   @item @tt{exact}: for an exact expression.
   @item @tt{o}: for the big-O.
   @end{itemize}
").
:- regtype approx(X) # "@var{X} represents an approximation.".
approx(ub).
approx(lb).
approx(me).
approx(oub).
approx(olb).
approx(o).
approx(ome).
approx(exact).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(resource_id/1).

:- doc(resource_id/1,"A resource is a numerical property that varies
   (is consumed) throughout the execution of a piece of
   code. Resources can be predefined (i.e., in a library) or
   user-defined.  Examples are computational steps, time spent, memory
   usage, bytes sent over a wire, database operations, energy
   consumed, etc.").
:- regtype resource_id(X)
   # "@var{X} is the name of a resource (see @lib{resources_decl}).".
resource_id(T) :- atm(T).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(cost_expression/1).

:- doc(cost_expression/1,"A cost expression is a symbolic function
   representing the cost of executing a piece of code in terms of the
   size of its input arguments and possibly other parameters. It is a
   term built from elements of the lattice of real numbers (see
   @pred{number_lattice}), numerical constants (see
   @pred{numeric_constant/1}), size metrics (see @pred{size_metric})
   and the following functors:

   @begin{itemize}

   @item @pred{- /1}: sign reversal.

   @item @pred{+ /1}: identity.

   @item @pred{-- /1}: decrement by one.

   @item @pred{++ /1}: increment by one.

   @item @pred{+ /2}: addition.

   @item @pred{- /2}: subtraction.

   @item @pred{* /2}: multiplication.

   @item @pred{/ /2}: division.

   @item @pred{** /2}: exponentiation.

   @item @pred{exp/2}: exponentiation (DEPRECATED use @pred{** /2}).

   @item @pred{log/2}: logarithm given the base.

   @item @pred{log10/1}: 10-base logarithm.

   @item @pred{log2/1}: 2-base logarithm.

   @item @pred{exp/1}: exponential (@em{e} to the power of).

   @item @pred{log/1}: natural logarithm (base @em{e}).

   @item @pred{sqrt/1}: square root.

   @item @pred{fact/1}: factorial.

   @item @pred{max/2}: maximum.

   @item @pred{min/2}: minimum.

   @item @pred{sum(Index,LowerBound,UpperBound,Exp)}: summation of
   @var{Exp} from @var{LowerBound} to @var{UpperBound} given the index
   variable @var{Index}.

   @item @pred{prod(Index,LowerBound,UpperBound,Exp)}: product of
   @var{Exp} from @var{LowerBound} to @var{UpperBound} given the index
   variable @var{Index}.

   @end{itemize}
").
:- regtype cost_expression(X) # "@var{X} is a cost expression.".
cost_expression(X) :- numeric_constant(X).
cost_expression(X) :- size_term(X).
cost_expression(X) :- number_lattice(X).
cost_expression(+X) :- cost_expression(X).
cost_expression(-X) :- cost_expression(X).
cost_expression(++X) :- cost_expression(X).
cost_expression(--X) :- cost_expression(X).
cost_expression(X+Y) :- cost_expression(X), cost_expression(Y).
cost_expression(X-Y) :- cost_expression(X), cost_expression(Y).
cost_expression(X*Y) :- cost_expression(X), cost_expression(Y).
cost_expression(X/Y) :- cost_expression(X), cost_expression(Y).
cost_expression(X**Y) :- cost_expression(X), cost_expression(Y).
cost_expression(exp(X)) :- cost_expression(X).
cost_expression(log(X,Y)) :- cost_expression(X), cost_expression(Y).
cost_expression(log(X)) :- cost_expression(X).
cost_expression(log2(X)) :- cost_expression(X).
cost_expression(log10(X)) :- cost_expression(X).
cost_expression(sqrt(X)) :- cost_expression(X).
cost_expression(fact(X)) :- cost_expression(X).
cost_expression(max(X,Y)) :- cost_expression(X), cost_expression(Y).
cost_expression(min(X,Y)) :- cost_expression(X), cost_expression(Y).
cost_expression(sum(Index,LowerBound,UpperBound,Expr)) :-
    indexvar(Index),
    cost_expression(LowerBound),
    cost_expression(UpperBound),
    agg_expression(Expr).
cost_expression(prod(Index,LowerBound,UpperBound,Expr)) :-
    indexvar(Index),
    cost_expression(LowerBound),
    cost_expression(UpperBound),
    agg_expression(Expr).
cost_expression(exp(X,Y)) :- cost_expression(X), cost_expression(Y).
:- endif.
% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(agg_expression/1).
:- doc(agg_expression/1,"An aggregation expression is an expression
   appearing in an aggregation function (@pred{sum/4},
   @pred{prod/4}). An aggregation expression can be a regular cost
   expression (see @pred{cost_expression/1}) or it may include index
   variables as well (see @pred{indexvar/1}).
").
:- regtype agg_expression(X)
   # "@var{X} is an aggregation expression.".
agg_expression(X) :- numeric_constant(X).
agg_expression(X) :- indexvar(X).
agg_expression(X) :- size_term(X).
agg_expression(X) :- number_lattice(X).
agg_expression(+X) :- agg_expression(X).
agg_expression(-X) :- agg_expression(X).
agg_expression(++X) :- agg_expression(X).
agg_expression(--X) :- agg_expression(X).
agg_expression(X+Y) :- agg_expression(X), agg_expression(Y).
agg_expression(X-Y) :- agg_expression(X), agg_expression(Y).
agg_expression(X*Y) :- agg_expression(X), agg_expression(Y).
agg_expression(X/Y) :- agg_expression(X), agg_expression(Y).
agg_expression(X**Y) :- agg_expression(X), agg_expression(Y).
agg_expression(exp(X)) :- agg_expression(X).
agg_expression(log(X,Y)) :- agg_expression(X), agg_expression(Y).
agg_expression(log(X)) :- agg_expression(X).
agg_expression(log2(X)) :- agg_expression(X).
agg_expression(log10(X)) :- agg_expression(X).
agg_expression(sqrt(X)) :- agg_expression(X).
agg_expression(fact(X)) :- agg_expression(X).
agg_expression(max(X,Y)) :- agg_expression(X), agg_expression(Y).
agg_expression(min(X,Y)) :- agg_expression(X), agg_expression(Y).
agg_expression(sum(Index,LowerBound,UpperBound,Expr)) :-
    indexvar(Index),
    agg_expression(LowerBound),
    agg_expression(UpperBound),
    agg_expression(Expr).
agg_expression(prod(Index,LowerBound,UpperBound,Expr)) :-
    indexvar(Index),
    agg_expression(LowerBound),
    agg_expression(UpperBound),
    agg_expression(Expr).
agg_expression(exp(X,Y)) :- agg_expression(X), agg_expression(Y).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(numeric_constant/1).
:- doc(numeric_constant/1,"A numeric constant is a fixed, well-defined
   real number.
").
:- regtype numeric_constant(X) # "@var{X} represents a numeric constant.".
numeric_constant(e).
:- endif.
% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(size_term/1).
:- doc(size_term/1,"A size term is a term in a cost expression (see
   @pred{cost_expression/1}) representing the size of a term in a given
   metric (see @pred{measure_t/1}).
").
:- regtype size_term(X) # "@var{X} represents the size of a term.".
size_term(int(X)) :- term(X).
size_term(length(X)) :- term(X).
size_term(size(X)) :- term(X).
size_term(depth(X)) :- term(X).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(indexvar/1).
:- doc(indexvar/1,"A variable used as an index in an aggregation
   expression.
").
:- regtype indexvar(X)
   # "@var{X} is an index variable.".
indexvar($(X)) :- gnd(X).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(number_lattice/1).
:- doc(number_lattice/1,"A lattice containing real numbers, @tt{inf} and @tt{bot}.
").
:- regtype number_lattice(X) # "@var{X} is a number, @tt{inf} or @tt{bot}.".
number_lattice(inf).
number_lattice(bot).
number_lattice(X) :- number(X).
:- endif.

% --------------------------------------------------------------------------
:- doc(bug,"size/3 vs. size_ub, size_lb redundant...").

:- if(defined(optim_comp)).
:- else.
:- export(size/2).
:- doc(size(X,Y),"The exact size (for any approximation) of term
   @var{X} is given by expression @var{Y}, which may depend on the
   size of other terms. @tt{size(X,Y)} is equivalent to
   @tt{size(exact,X,Y)}.
").
:- prop size(X, Y)
   : ( term(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is the size of argument @var{X}, for any approximation.".

size(_, _).
% :- impl_defined(size/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(size/3).
:- doc(size(A,X,Y),"The size of term @var{X} for the approximation
   @var{A} is given by expression @var{Y}, which may depend on the
   size of other terms.
").
:- prop size(A, X, Y)
   : ( approx(A), term(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is the size of argument @var{X}, for the approximation @var{A}.".

size(_, _, _).
% :- impl_defined(size/3).
:- endif.

:- export(size_lb/2).
:- doc(size_lb(X, Y), "The minimum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_lb(X, Y)
   : ( term(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is a lower bound on the size of argument @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(size_lb/2, [impnat=indefinable]).
:- else.
:- impl_defined(size_lb/2).
:- endif.

:- export(size_ub/2).
:- doc(size_ub(X, Y), "The maximum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_ub(X, Y)
   : ( term(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is a upper bound on the size of argument @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(size_ub/2, [impnat=indefinable]).
:- else.
:- impl_defined(size_ub/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(size_o/2).
:- prop size_o(X, Y)
   : ( term(X), cost_expression(Y) )
   + no_rtcheck
   # "The size of argument @var{X} is in the order of the expression
   @var{Y}.".

:- impl_defined(size_o/2).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(size_metric/3).
:- meta_predicate size_metric(goal, ?, ?).
:- prop size_metric(Head, Var, Metric)
   : ( callable(Head), term(Var), measure_t(Metric) )
   + no_rtcheck
   # "@var{Metric} is the measure used to determine the size of the
   terms that @var{Var} is bound to, for any type of approximation.".

size_metric(Goal, _, _) :- call(Goal).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(size_metric/4).
:- meta_predicate size_metric(goal, ?, ?, ?).
:- prop size_metric(Head, Approx, Var, Metric)
   : ( callable(Head), approx(Approx), term(Var), measure_t(Metric) )
   + no_rtcheck
   # "@var{Metric} is the measure used to determine the size of the
   terms that variable @var{Var} bound to, for the approximation
   @var{Approx}.". % depth/1 See resources_basic

size_metric(Goal, _, _, _) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(measure_t/1).
:- doc(measure_t/1,"The types of term size measures currently
   supported in size and cost analysis (see also in
   @lib{resources_basic.pl}).
   @begin{itemize}
   @item @tt{int}: The size of the term (which is an integer) is the
     integer value itself.
   @item @tt{length}: The size of the term (which is a list) is its
     length.
   @item @tt{size}: The size is the overall of the term (number of
     subterms).
   @item @tt{depth([_|_])}: The size of the term is its depth.
   @item @tt{void}: Used to indicate that the size of this argument
     should be ignored.
   @end{itemize}
").
%% @item @tt{depth([_|_])}:
%%    The size of the term is the number of rule
%%    applications of its type definition.

:- regtype measure_t(X) # "@var{X} is a term size metric.".

measure_t(void).
measure_t(int).
measure_t(length).
measure_t(size).
measure_t(depth([_|_])).
:- endif.

% --------------------------------------------------------------------------

:- export(steps_lb/2).
:- doc(steps_lb(X, Y), "The minimum computation (in resolution steps)
   spent by any call of the form @var{X} is given by the expression
   @var{Y} @cite{low-bounds-ilps97,granularity-jsc}").

:- meta_predicate steps_lb(goal, ?).
:- prop steps_lb(X, Y)
   : ( callable(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is a lower bound on the cost of any call of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(steps_lb/2, [impnat=indefinable]).
:- else.
steps_lb(Goal, _) :- call(Goal).
:- endif.

:- export(steps_ub/2).
:- doc(steps_ub(X, Y), "The maximum computation (in resolution steps)
   spent by any call of the form @var{X} is given by the expression
   @var{Y} @cite{caslog,granularity-jsc}.").

:- meta_predicate steps_ub(goal, ?).
:- prop steps_ub(X, Y)
   : ( callable(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is a upper bound on the cost of any call of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(steps_ub/2, [impnat=indefinable]).
:- else.
steps_ub(Goal, _) :- call(Goal).
:- endif.

:- export(steps/2).
:- doc(steps(X, Y), "The computation (in resolution steps) spent by
   any call of the form @var{X} is given by the expression @var{Y}").

:- meta_predicate steps(goal, ?).
:- prop steps(X, Y)
   : ( callable(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is the cost (number of resolution steps) of any call of the
   form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(steps/2, [impnat=indefinable]).
:- else.
steps(Goal, _) :- call(Goal).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(steps_o/2).
:- doc(steps_o(X, Y), "The big O expression of the computation (in
   resolution steps) spent by any call of the form @var{X} is given by
   the expression @var{Y}").

:- meta_predicate steps_o(goal, ?).
:- prop steps_o(X, Y)
   : ( callable(X), cost_expression(Y) )
   + no_rtcheck
   # "@var{Y} is the complexity order of the cost of any call of the form
   @var{X}.".

steps_o(Goal, _) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
% TODO: Move rsize related regtypes here?
:- export(rsize/2). % TODO:[new-resources]
:- prop rsize(Var,SizeDescr) + no_rtcheck
   # "@var{Var} has its size defined by @var{SizeDescr}.".
:- impl_defined(rsize/2).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(costb/4). % TODO:[new-resources]
:- doc(costb(Goal,Resource,Lower,Upper),"@var{Lower}
   (res. @var{Upper}) is a (safe) lower (res. upper) bound on the cost
   of the computation of @var{Goal} in terms of @var{Resource} units.
").
:- meta_predicate costb(goal, ?, ?, ?).
:- prop costb(Goal,Resource,Lower,Upper)
   : ( callable(Goal), resource_id(Resource), cost_expression(Lower),
       cost_expression(Upper) )
   + no_rtcheck
   # "@var{Lower} (resp. @var{Upper}) is a (safe) lower (resp. upper)
     bound on the cost of the computation of @var{Goal} expressed in
     terms of @var{Resource} units.".
:- impl_defined(costb/4).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(cost/4).
:- doc(cost(Goal,Approx,Resource,Expr),"@var{Expr} is a (safe) upper
   or lower bounds, depending on the value of @var{Approx} (see
   @pred{approx/1}), of the cost of computation of the goal @var{Goal}
   in terms of @var{Resource} units.
").
:- meta_predicate cost(goal, ?, ?, ?).
:- prop cost(Goal,Approx,Resource,Expr)
   : ( callable(Goal), approx(Approx), resource_id(Resource),
       cost_expression(Expr) )
   + no_rtcheck
   # "@var{Expr} is a safe upper or lower bounds (depending on
     @var{Approx}) of the cost of computing @var{Goal} in terms of
     @var{Resource} units.".
:- impl_defined(cost/4).
:- endif.

% --------------------------------------------------------------------------

:- export(terminates/1).
:- doc(terminates(X), "Calls of the form @var{X} always terminate.").

:- meta_predicate terminates(goal).
:- prop terminates(X)
   : callable(X)
   + no_rtcheck
   # "All calls of the form @var{X} terminate.".

:- if(defined(optim_comp)).
:- '$props'(terminates/1, [impnat=indefinable]).
:- else.
terminates(Goal) :- call(Goal).
:- endif.
