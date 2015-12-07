:- module(_, _, [dcg]).

get_value_from_string(String, Name, Value) :-
	get_value_from_string2(Name, Value, String, _Tail).

get_value_from_string2(Name, Value) -->
	"\n",
	!,
	get_value_from_string2(Name, Value).
get_value_from_string2(Name, Value) -->
	"#",
	any_text(_Comment),
	!,
	get_value_from_string2(Name, Value).

get_value_from_string2(Name, Value) -->
	equality(Name1, Value1),
	!,
	(
	    {Name = Name1},
	    {Value = Value1}
	;
	    get_value_from_string2(Name, Value)
	).

any_text([C|Cs]) --> [C], {C \== 0'\n}, any_text(Cs).
any_text([]) --> "".

char_name(C) --> [C], {C \== 0'=}.

name([C|Cs]) --> char_name(C), name(Cs).
name([C]) --> char_name(C).

equality(Name, Value) -->
	name(Name),
	"=",
	any_text(Value).
