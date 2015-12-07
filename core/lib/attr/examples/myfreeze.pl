:- module(myfreeze, [myfreeze/2], [attr, dcg]).

:- meta_predicate(myfreeze(?, goal)). 

myfreeze(X, Goal) :- 
	( nonvar(X) ->
	    call(Goal)
 	; get_attr_local(X, Fb) ->
	    meta_conj(Fb, Goal, C),
	    put_attr_local(X, C)   % rescue conjunction
	; put_attr_local(X, Goal)
	).

:- if(defined(optim_comp)).        
attr_unify_hook(Fa, Other) :-
	( nonvar(Other) ->
	    '$trust_metatype'(Fa, goal),
	    call(Fa) 
	; get_attr_local(Other, Fb) ->
	    meta_conj(Fa, Fb, C),
	    put_attr_local(Other, C)  % rescue conjunction
	; put_attr_local(Other, Fa)   % rescue conjunction
	).
:- else.
attr_unify_hook(Fa, Other) :-
	( nonvar(Other) ->
	    call(Fa) 
	; get_attr_local(Other, Fb) ->
	    meta_conj(Fa, Fb, C),
	    put_attr_local(Other, C)  % rescue conjunction
	; put_attr_local(Other, Fa)   % rescue conjunction
	).
:- endif.

attribute_goals(X) --> 
	[myfreeze:myfreeze(X, G)],
	{get_attr_local(X, G)}.

attr_portray_hook(G, Var):- 
	display(Var), 
	display('<-myfrozen('), 
	display(G), 
	display(')').

% A (meta) conjunction of two goals
meta_conj('$:'(Fa), '$:'(Fb), '$:'('basiccontrol:,'(Fa, Fb))).

