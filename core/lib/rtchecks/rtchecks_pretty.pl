:- module(rtchecks_pretty,
        [
            pretty_messages/1,
            pretty_prop/3,
            rtcheck_to_messages/3
        ],
        [assertions]).

:- use_module(library(lists), [append/3, reverse/2, select/3]).
:- use_module(library(assertions/native_props), [is_det/1]).
:- use_module(library(hiordlib),                [map/3]).
:- use_module(library(rtchecks/compact_list),   [compact_list/2]).
:- use_module(library(rtchecks/rtchecks_utils), [rtcheck_error/1]).
:- use_module(library(varnames/dict_types),     [varnamesl/1]).
:- use_module(library(varnames/apply_dict),     [apply_dict/4]).
:- use_module(library(varnames/complete_dict),  [complete_dict/4]).

:- doc(author, "Edison Mera").      % code
:- doc(author, "Nataliia Stulova"). % documentation

:- doc(module, "This module contains predicates used to transform
        run-time errors into human-readable messages.").

% TODO: possibly move rtchecks_basic:get_pretty_names/5 and its
%       related predicates here.

% Used in the rtchecks and unittest libraries only
:- pred pretty_messages(Messages) : list(Messages, message_info)
        # "Sets the @tt{write_strings} flag to the value @tt{on},
        removes duplicated items in the list of @var{Messages} and
        prints them to the current output. After finishing, sets the
        flag value to @tt{off}.".

pretty_messages(Messages) :-
	push_prolog_flag(write_strings, on),
	compact_list(Messages, Messages1),
	messages(Messages1),
	fail.
pretty_messages(_) :-
	pop_prolog_flag(write_strings).

:- pred rtcheck_to_messages(RTCheck, Messages0, Messages)
	:: (list(Messages0, message_info), list(Messages, message_info))
        : rtcheck_error(RTCheck) + is_det
        # "Converts a single run-time error @var{RTCheck} into a list of
           one or multiple text messages @var{Messages0}. @var{Messages}
           is the tail.".

rtcheck_to_messages(rtcheck(Type, Pred0, Dict, Prop0, Valid0, Positions0),
	    Messages0, Messages) :-
	pretty_prop(t(Pred0, Prop0, Valid0, Positions0), Dict,
	    t(Pred, Prop, Valid, Positions)),
	map(Positions, position_to_message, PosMessages0),
	reverse(PosMessages0, PosMessages),
	Text = ['Run-time check failure in assertion for:\n\t',
	    ''({Pred}),
	    '.\nIn *', Type, '*, unsatisfied property: \n\t',
	    ''({Prop}), '.'|Text0],
	( Valid = [] -> Text0 = Text1
	; Text0 = ['\nBecause: \n\t', ''({Valid}), '.'|Text1]
	),
	(
	    select(message_lns(S, Ln0, Ln1, MessageType, Text2),
		PosMessages, PosMessages1) ->
	    (Text2 == [] -> Text1 = [] ; Text1 = [' '|Text2]),
	    Message = message_lns(S, Ln0, Ln1, MessageType, Text)
	;
	    Text1 = [],
	    Message = message(error, Text),
	    PosMessages1 = PosMessages
	),
	append([Message|PosMessages1], Messages, Messages0).

% TODO: pretty_prop/3 is  used only in the rtchecks and unittest libs,
%       and it almost duplicates varnames/pretty_names:pretty_names/3
%       predicate. Should pretty_names/4 with an additinal Idemp
%       parameter be introduced (see varnames lib) to varnames/pretty_names,
%       pretty_prop/3 might be replaced safely by it.

:- pred pretty_prop(Prop, Dict, PrettyProp)
        : (term(Prop), varnamesl(Dict), var(PrettyProp))
        # "Adds variables of the term @var{Prop} into the variable names
           dictionary @var{Dict} if they were not there, and creates a
           copy of the initial term, @var{PrettyProp}, by applying the
           extended dictionary on @var{Prop}.".

pretty_prop(Prop, Dict0, PrettyProp) :-
	complete_dict(Prop, Dict0, [], EDict),
	append(Dict0, EDict, Dict),
	apply_dict(Prop, Dict, yes, PrettyProp).

:- pred position_to_message(Position, Message)
        :  struct(Position)
        => message_info(Message)
        # "Converts the location information of a run-time error into
           a message.".

position_to_message(predloc(Pred, loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error,
		['Failed in ', ''(Pred), '.'])).
position_to_message(callloc(Pred, loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error,
		['Failed during invocation of ', ''(Pred)])).
position_to_message(litloc(Lit, loc(S, Ln0, Ln1)-(Pred)),
	    message_lns(S, Ln0, Ln1, error,
		['Failed when invocation of ', ''(Pred),
		    ' called ', ''(Lit), ' in its body.'])).
position_to_message(asrloc(loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error, [])).
position_to_message(pploc(loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error, [])).
