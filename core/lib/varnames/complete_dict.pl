:- module(complete_dict, [complete_dict/4, set_undefined_names/3],
	    [assertions, basicmodes]).

:- use_module(library(varnames/dict_types), [varnamesl/1]).
:- use_module(library(iso_misc),             [compound/1]).

:- pred complete_dict(?term, +list(var), +varnamesl, -varnamesl).

complete_dict(Term, Dict, Exclude, EDict) :-
	complete_dict_arg(Term, Dict, Exclude, 1, _, [], EDict).

set_undefined_names([],              Idx,  Idx).
set_undefined_names([Name = _|Dict], Idx0, Idx) :-
	(
	    var(Name) ->
	    new_name(Dict, Name, Idx0, Idx1)
	;
	    Idx0 = Idx1
	),
	set_undefined_names(Dict, Idx1, Idx).

complete_dict_args(N, Term, Dict, Exclude, Idx0, Idx, EDict0, EDict) :-
	arg(N, Term, Arg),
	!,
	complete_dict_arg(Arg, Dict, Exclude, Idx0, Idx1, EDict0, EDict1),
	N1 is N + 1,
	complete_dict_args(N1, Term, Dict, Exclude, Idx1, Idx, EDict1,
	    EDict).
complete_dict_args(_, _, _, _, Idx, Idx, EDict, EDict).

complete_dict_arg(Arg, Dict, Exclude, Idx0, Idx, EDict0, EDict) :-
	var(Arg),
	!,
	(
	    ( member(Value, Exclude)
	    ; nonvar(Dict), member(Name = Value, Dict)
	    ; member(Name = Value, EDict0) ),
	    Arg == Value ->
	    EDict = EDict0,
	    ( var(Name) ->
		new_name(Dict, Name, Idx0, Idx)
	    ; Idx = Idx0
	    )
	;
	    Idx = Idx0,
	    EDict = [Name = Arg|EDict0]
	).
complete_dict_arg(Arg, Dict, Exclude, Idx0, Idx, EDict0, EDict) :-
	compound(Arg),
	!,
	complete_dict_args(1, Arg, Dict, Exclude, Idx0, Idx, EDict0, EDict).
complete_dict_arg(_, _, _, Idx, Idx, EDict, EDict).

new_name(Dict, Name, Idx0, Idx) :-
	atom_number(AIdx0, Idx0),
	atom_concat('_', AIdx0, Name0),
	(
	    member(Name1 = _, Dict),
	    nonvar(Name1),
	    Name1 = Name0 ->
	    Idx1 is Idx0 + 1,
	    new_name(Dict, Name, Idx1, Idx)
	;
	    Name = Name0,
	    Idx is Idx0 + 1
	).
