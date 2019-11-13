:- module(errhandle, [], [noprelude, assertions]).

:- doc(title, "Default exception handler and pretty printer").
:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- use_module(engine(basiccontrol)).
:- use_module(engine(exceptions)).
:- use_module(engine(hiord_rt), [call/1]).
:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [rt_modexp/4]).
:- endif.
%
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(io_basic)).
:- use_module(engine(messages_basic)).
:- if(defined(optim_comp)).
:- else.
:- use_module(library(system), [system_error_report/1]).
:- use_module(library(rtchecks/rtchecks_pretty), [
    rtcheck_to_messages/2 % TODO: make it optional?
   ]).
:- endif.

:- export(error_protect/2).
:- meta_predicate error_protect(goal, goal).
:- pred error_protect(Goal, OnError) # "Execute @var{Goal} with a
   default exception handler (which shows the exception and executes
   @var{OnError}.".

error_protect(Goal, OnError) :-
    % TODO: catch all errors?
    RTError = rtcheck(_,_,_,_,_,_),
    E = error(_,_),
    catch(catch(Goal,
              RTError, handle_error(RTError, OnError)),
        E, handle_error(E, OnError)).

:- meta_predicate handle_error(?, goal).
handle_error(E, OnError) :-
    default_error_message(E),
    call(OnError).

:- export(default_error_message/1).
:- pred default_error_message(E) # "Default pretty printer for the
   exception term @var{E}.".

:- if(defined(optim_comp)).
:- else.
default_error_message(E) :- E = rtcheck(_,_,_,_,_,_), !,
    rtcheck_to_messages(E, Messages), % TODO: merge with get_error_message/2
    messages(Messages).
:- endif.
default_error_message(E) :-
    get_error_message(E, Message),
    display(user_error, '{'),
    message(error, Message).

get_error_message(error(Error, Where), Message0) :-
    nonvar(Error), nonvar(Where),
    get_where(Where, Message0, Message),
    get_error(Error, Message, ['}']),
    !.
get_error_message(E, Message) :- % TODO: merge with exceptions:no_handler/1
    Message = ['No handle found for thrown exception ', ~~(E), '}'].

get_where(unknown/ -1, T, T) :- !. % TODO: why? document
get_where(P/N-A, [P,'/',N,', arg ',A,' - '|T], T) :- !.
get_where(P/N,   [P,'/',N,' - '|T],            T) :- !.
get_where(W,     [W,' - '|T],                  T).

:- if(defined(optim_comp)).
get_error(system_error, ['system error'|T], T) :- !. % TODO: add system_error_report/1
:- else.
get_error(system_error, ['system error: ', SER|T], T) :- !,
    system_error_report(SER).
:- endif.
get_error(syntax_error, ['syntax error'|T], T) :- !.
get_error(resource_error(Type), [ResourceError|T], T) :- !, 
    translate_resource_error(Type, ResourceError).
get_error(user_error, ['user error (C interface?)'|T], T) :- !.
get_error(evaluation_error(Type, Culprit),
    ['evaluation error ', Type, ' in ', Culprit|T], T) :- !.
get_error(representation_error(Type),
    ['representation error ', Type|T], T) :- !.
% File does not exist 
get_error(permission_error(open, _Stream, Culprit),
    ['could not open: ', Culprit|T], T) :- !.
% Permission error
get_error(permission_error(Operation, Object, Culprit),
    ['permission error: ', Operation,
     ' not allowed on ', Object,
     ' with value ', Culprit|T], T) :- !.
get_error(existence_error(Type, Culprit),
    ['existence error: ', Type,
     ':', ~~(Culprit), ' does not exist'|T], T) :- !.
get_error(type_error(Type, Culprit),
    ['expected ', Readable,
     ', found ', Culprit|T], T) :- !,
    translate_ball(Type, Readable).
get_error(instantiation_error,
    ['argument is not sufficiently instantiated'|T], T):-!.
get_error(uninstantiation_error(Culprit), 
    ['expected an (unbound) variable, found ', Culprit|T], T):-!.
% Value of correct type but outside allowed range
get_error(domain_error(Domain, Culprit),
    ['expected ', Readable,
     ', found ', Culprit|T], T) :- !,
    translate_ball(Domain, Readable).
get_error(syntax_error([L0, L1, Msg, ErrorLoc]),
    ['syntax error '|Message], T) :- !,
    add_lines(L0, L1, ['\n', [](Msg), ':', ErrorLoc|T], Message).
get_error(unintercepted_signal(Signal), Msg, T) :- !, % TODO: deprecate
    Msg = ['No handle found for sent signal ', ~~(Signal)|T].

translate_ball(atom,      'a non-numeric atom') :- !.
translate_ball(atomic,    'an atom (including a number)') :- !.
translate_ball(byte,      'a byte') :- !.
translate_ball(callable,  'a callable structure (i.e., a goal)') :- !.
translate_ball(character, 'a character') :- !.
translate_ball(compound,  'a non-atomic term (a structure)') :- !.
translate_ball(evaluable, 'an arithmetically evaluable expression') :- !.
translate_ball(in_byte,             'a byte') :- !.
translate_ball(in_character,        'a character') :- !.
translate_ball(integer,             'an integer') :- !.
translate_ball(list,                'a list') :- !.
translate_ball(number,              'a number') :- !.
translate_ball(predicate_indicator, 'a predicate indicator') :- !.
% TODO: RH : remove in ISO corregendum 2.
%translate_ball(variable,            'an (unbound) variable') :- !.
%
translate_ball(flag_value, 'a valid flag value') :- !.
%
translate_ball(Ball, Ball) :- !. % Not yet a readable form

translate_resource_error(heap, 'heap overflow') :- !.
translate_resource_error(_, 'resource error').

:- if(defined(optim_comp)).
% TODO: from messages_basic.pl
add_lines(L0, L1, Message, ['(lns ', L0, '-', L1, ') '|Message]).
:- endif.
