:- module(clpq_attr, [], [library(clpqr/clpqr_options)]).

:- if(defined(clpqr__use_multi_attributes)).

:- use_module(library(attr/attr_rt)).

:- export(get_attribute/2).
get_attribute(X, Attr):-
	get_attr(X, clpq_rt, Attr).

:- export(attach_attribute/2). 
attach_attribute(X, Attr):-
	put_attr(X, clpq_rt, Attr).

:- export(update_attribute/2).
update_attribute(X, Attr):-
	put_attr(X, clpq_rt, Attr).
					
:- export(detach_attribute/1).
detach_attribute(X):-
	del_attr(X, clpq_rt).

:- else. % if(defined(clpqr__use_multi_attributes)).

:- reexport(engine(attributes), [ get_attribute/2,
				  attach_attribute/2, 
				  update_attribute/2, 
				  detach_attribute/1 ]).

:- endif. % if(defined(clpqr__use_multi_attributes)).