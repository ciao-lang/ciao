:- module(parse_spec, [parse_functor_spec/5], []).

% Parse predicate spec or a list of them.

:- use_module(engine(internals), 
        [module_concat/3, '$setarg'/4, '$current_predicate'/2]).

:- meta_predicate parse_functor_spec(?,?,?,goal,?).

parse_functor_spec(Arg, _, _, _, _) :-
	nonvar(Arg), Arg=[], !.
parse_functor_spec(Arg, Module, GoalArg, Goal, Doing) :-
	nonvar(Arg), Arg=[X|Rest], !,
	Flag=f(0),
	(   
            functor_spec(X, Name, Low, High),
            module_concat(Module, Name, PredName),
	    '$current_predicate'(PredName, GoalArg),
	    functor(GoalArg, _, N),
	    N >= Low, N =< High,
	    '$setarg'(1, Flag, 1, true),
	    call(Goal),
	    fail
	;   
            Goal = listing1(_) -> 
            true
	;   
            Flag=f(0),
	    X=GoalArg,
	    throw(error(domain_error(predicate_indicator_list,Arg), Doing))
	;   true
	),
	parse_functor_spec(Rest, Module, GoalArg, Goal, Doing).
parse_functor_spec(Arg, Module, GoalArg, Goal, Doing) :-
	parse_functor_spec([Arg], Module, GoalArg, Goal, Doing).

functor_spec(Name/Low-High, Name, Low, High) :-
	atom(Name),
	integer(Low), integer(High), !.
functor_spec(Name/(Low-High), Name, Low, High) :-
	atom(Name),
	integer(Low), integer(High), !.
functor_spec(Name/Arity, Name, Arity, Arity) :-
	atom(Name),
	integer(Arity), !.
functor_spec(Name, Name, 0, 255) :-		% 255 is max. arity
	atom(Name).
