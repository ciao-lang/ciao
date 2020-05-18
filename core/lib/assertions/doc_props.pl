:- module(doc_props, [
    doc_incomplete/1
], [assertions]).

:- prop doc_incomplete(X)
# "Documentation is still incomplete: @var{X} may not conform the
   functionality documented.".

:- if(defined(optim_comp)).
:- '$props'(doc_incomplete/1, [impnat=indefinable]).
:- else.
:- impl_defined(doc_incomplete/1).
:- endif.
