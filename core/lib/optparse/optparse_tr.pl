:- module(optparse_tr, [optparse_tr/3], [assertions, nortchecks, define_flag]).

:- use_module(library(hiordlib),   [map/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(llists),     [flatten/2]).
:- use_module(library(lists),      [append/3, length/2]).
:- use_module(library(messages)).
:- use_module(library(compiler/c_itf_internal), [location/1]).

:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales (bug fixes)").

:- doc(module,

"Package that provides parsing of options in a consistent way.  The
options can come from the preprocess flags or defined by the user.
To configure it, use the following declarations:

@begin{verbatim}
:- simple_option(Option, Action, Help, Terminates, Args0, Args).
@end{verbatim}

Configures a user defined option.

@begin{verbatim}
:- flag_based_option(FlagName, FlagValue, Option, Help).
@end{verbatim}

Configures an option to modify a prolog flag.

@begin{verbatim}
:- default_action(Action, Args).
@end{verbatim}

Configure the action to execute over the remaining arguments if no
more options are available.

In the module that use this package, the predicate @pred{parse_args/1} is
defined in order to facilitate parsing of command line options.

").

simplify_value(yes, []) :- !.
simplify_value(on,  []) :- !.
simplify_value(X,   [0'-|S]) :-
	atom_codes(X, S).

underscore_to_minus(0'_, 0'-) :- !.
underscore_to_minus(X,   X).

flag_name_value_option(Name, Value0, Option) :-
	atom_codes(Name, NameS0),
	simplify_value(Value0, ValueS),
	map(NameS0, underscore_to_minus, NameS),
	append([0'-, 0'-|NameS], ValueS, Option).

:- data help_option/2.

option_to_clause(Option, Action, Terminate, Args0, Args,
	    (exec_option(Option, Terminate, Args0, Args) :- Action)).

optparse_tr(0,           _, _) :- cleanup_db.
optparse_tr(end_of_file, _, _) :- cleanup_db.
optparse_tr(( :- simple_option(Options0, Action, Help, Terminate, Args0,
		    Args) ), Clauses, _) :-
	flatten([Options0], Options1),
	map(Options1, option_to_clause(Action, Terminate, Args0, Args),
	    Clauses),
	map(Options1, atom_codes, Options),
	assertz_fact(help_option(Options, Help)).
optparse_tr((:- flag_based_option(Flag, Value, Option, Help)), Clauses, _) :-
	flag_name_value_option(Flag, Value, SLongOption),
	% TODO: WRONG! This is using runtime flags at
	% compile-time. Currently, there is not way to accurately
	% read the values of define_flag, so this warning is
	% better to be disabled (JFMC).
%%	location(Loc),
%%	( define_flag(Flag, Values, Default) ->
%%	    ( member(Value, Values) ->
%%		( Value == Default ->
%%		    note_message(Loc,
%%			"Value ~w for flag ~w is already the default.",
%%			[Value, Flag])
%%		; true
%%		)
%%	    ; warning_message(Loc, "Value ~w for flag ~w not allowed",
%%	          [Value, Flag])
%%	    )
%%	; warning_message(Loc, "Flag ~w undefined", [Flag])
%%	),
	atom_codes(LongOption, SLongOption),
	( Option == [] ->
	    Clauses = Clauses0,
	    Options = [SLongOption]
	; Clauses = [
              ( exec_option(Option, continue, Args, Args) :-
		  set_prolog_flag(Flag, Value) )
              |Clauses0
          ],
	  atom_codes(Option, S),
	  Options = [S, SLongOption]
	),
	Clauses0 = [
              ( exec_option(LongOption, continue, Args, Args) :-
		set_prolog_flag(Flag, Value) )
        ],
	assertz_fact(help_option(Options, Help)).
optparse_tr((:- default_action(Action, Args)),
	    [(default_action(Args) :- Action)], _).
optparse_tr((:- base_message(Action, Message0)),
	    [(usage_message(Message)),
		( usage :- Action,
		    usage_message(Message1),
		    message(['Usages:\n', $$(Message1)]) )], _) :-
	findall(OptionStr, help_option_str(OptionStr), Options0),
	flatten([Message0, Options0], Message).

flat_options([],        []).
flat_options([Op|OpSS], OpS) :-
	flat_options2(OpSS, Op, OpS).

flat_options2([],        Op,  Op).
flat_options2([Op|OpSS], Op0, OpS0) :-
	append(Op0, ", " || OpS1, OpS0),
	flat_options2(OpSS, Op, OpS1).

help_option_str(OptionStr) :-
	help_option(Options, Help),
	flat_options(Options, FOptions),
	length(FOptions, N),
	(N >= 8 -> C = "\n\t" ; C = "\t"),
	flatten(["\n"||FOptions, C, Help], OptionStr).

cleanup_db :-
	retractall_fact(help_option(_, _)).
