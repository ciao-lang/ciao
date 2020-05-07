:- module(rtchecks_pretty,
    [
        pretty_prop/3,
        rtcheck_to_messages/2,
        rtcheck_to_message/3
    ],
    [assertions, regtypes]).

:- doc(author, "Edison Mera").      % code
:- doc(author, "Nataliia Stulova"). % documentation

:- doc(module, "This module contains predicates used to transform
    run-time errors into human-readable messages.").

:- use_module(engine(messages_basic), [message_info/1]).
:- use_module(library(lists), [append/3, reverse/2, select/3]).
:- use_module(library(assertions/native_props), [is_det/1]).
:- use_module(library(hiordlib), [maplist/3, foldl/4]).
:- use_module(library(varnames/dict_types),     [varnamesl/1]).
:- use_module(library(varnames/apply_dict),     [apply_dict/4]).
:- use_module(library(varnames/complete_dict),  [complete_dict/4]).

% TODO: possibly move rtchecks_basic:get_pretty_names/5 and its
%       related predicates here.

:- regtype rtcheck_error/1 #
    "Specifies the format of a run-time check exception.".

rtcheck_error(rtcheck(Type, _Pred, Dict, _Prop, _Valid, Locs)) :-
    rtcheck_type(Type),
    list(Dict),
    list(Locs).

:- regtype rtcheck_type/1 # "Specifies the type of run-time errors.".

rtcheck_type(comp).
rtcheck_type(pp_check).
rtcheck_type(success).
rtcheck_type(compat).
rtcheck_type(compatpos).
rtcheck_type(calls).

:- pred rtcheck_to_messages(RTCheck, Messages)
    : rtcheck_error(RTCheck) => list(message_info, Messages) + is_det
    # "Converts a single run-time error @var{RTCheck} into a list of
       one or multiple text messages @var{Messages0}. @var{Messages}
       is the tail.".


rtcheck_to_messages(E, Messages) :-
    E = rtcheck(Type, Pred0, Dict, Prop0, Valid0, Positions0),
    (nonvar(Positions0) -> true ; Positions0=[]), % TODO: Kludge: Find out where free variables as Positions are introduced and fix it
    pretty_prop(t(Pred0, Prop0, Valid0, Positions0), Dict,
        t(Pred, Prop, Valid, Positions)),
    maplist(position_to_message, Positions, PosMessages0),
    reverse(PosMessages0, PosMessages),
    rtcheck_to_message_(Type, Pred, Prop, Valid, Text, Text1),
    (
        select(message_lns(S, Ln0, Ln1, MessageType, Text2),
            PosMessages, PosMessages1) ->
        (Text2 == [] -> Text1 = [] ; Text1 = [' '|Text2]), % \n ?
        Message = message_lns(S, Ln0, Ln1, MessageType, Text)
    ;
        Text1 = [],
        Message = message(error, Text),
        PosMessages1 = PosMessages
    ),
    Messages1 = [Message|PosMessages1],
    compact_list(Messages1, Messages). % needed to handle repeated position messages. % TODO: do it before, with Postitions0 or Positions, or even in rtchecks_send/6


rtcheck_to_message(E, Text, TextTail) :-
    E = rtcheck(Type, Pred0, Dict, Prop0, Valid0, _),
    pretty_prop(t(Pred0, Prop0, Valid0), Dict,
        t(Pred, Prop, Valid)),
    rtcheck_to_message_(Type, Pred, Prop, Valid, Text, TextTail).

rtcheck_to_message_(Type, Pred, Prop, Valid, Text, Text1) :-
    Text = ['Run-time check failure in assertion for:\n\t',
        ''({Pred}),
        '.\nIn *', Type, '*, unsatisfied property: \n\t',
        ''({Prop}), '.'|Text0],
    ( Valid = [] -> Text0 = Text1
    ;
        actual_props_to_messages(Valid,ActualProps,Text1),
        Text0 = ['\nWhere:'| ActualProps]
    ).

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


actual_props_to_messages(ActualProps,Messages,Tail) :-
    foldl(actual_prop_to_message, ActualProps, Messages, Tail).

actual_prop_to_message(X=Y,['\n\t', ''({var(X)}) | Tail], Tail) :-
    X==Y, !.
% temporary fix to write var(X) instead of X=X
actual_prop_to_message(X,['\n\t', ''({X}) | Tail], Tail).

% TODO: Use toplevel to do this right (e.g. X=Y instead of X=_1, Y=_1).

% ---------------------------------------------------------------------------
% TODO: ugly implementation, try to avoid it!

:- use_module(library(lists), [length/2, append/3]).

:- test compact_list(A, B) :
    (A = [1, 2, 2, 2, 2, 3, 3, 4, 3, 4, 3, 4, 3, 4, 1, 5, 7, 1, 5, 7])
    => (B = [1, 2, 3, 4, 1, 5, 7]) + not_fails.

:- pred compact_list(L, R) : list(L) => list(R)
    # "Delete repeated sequences in a list.".

compact_list(L, R) :-
    compact_list_(L, 1, R).

compact_list_(L, N, R) :-
    ( compact_list_n(L, N, R0) ->
        N1 is N + 1,
        compact_list_(R0, N1, R)
    ; L = R
    ).

compact_list_n(L, N, R) :-
    length(L1, N),
    append(L1, R1, L),
    compact_list_n_(L, L1, R1, N, R).

compact_list_n_(L, L1, R1, N, R) :-
    length(L2, N),
    append(L2, R2, R1),
    ( L1 == L2 ->
        ( compact_list_n_(R1, L2, R2, N, R) ->
            true
        ; R1 = R
        )
    ; L = [E|L0],
      ( compact_list_n(L0, N, R0) ->
          R = [E|R0]
      ; R = L
      )
    ).

