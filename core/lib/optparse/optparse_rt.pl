:- module(optparse_rt, [parse_args/4], [hiord]).

:- meta_predicate parse_args(?, pred(4), pred(1), goal).

parse_args([], _ExecOption, _DefaultAction, Usage) :-
	Usage,
	halt(1).
parse_args([A|As], ExecOption, DefaultAction, Usage) :-
	parse_args_(A, As, ExecOption, DefaultAction, Usage).

:- meta_predicate parse_args_(?, ?, pred(4), pred(1), goal).
parse_args_(Arg, Args0, ExecOption, DefaultAction, Usage) :-
	ExecOption(Arg, Terminate, Args0, Args),
	!,
	handle_remainings(Terminate, Args, ExecOption, DefaultAction, Usage).
parse_args_(Arg, Args, _, DefaultAction, _) :-
	DefaultAction([Arg|Args]).

:- meta_predicate handle_remainings(?, ?, pred(4), pred(1), goal).
handle_remainings(finished, _,    _,          _,             _).
handle_remainings(continue, Args, ExecOption, DefaultAction, Usage) :-
	parse_args(Args, ExecOption, DefaultAction, Usage).
