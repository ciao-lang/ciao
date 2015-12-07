%% ---------------------------------------------------
%%
%% GENERAL EVENT HANDLING IN CIAO/Prolog
%%
%% Angel Fernandez Pineda
%%
%% First developed in Oct-98
%%
%% ---------------------------------------------------
%%
%% Event-Driven system definitions:
%%
%% Event :               any prolog term
%% Event Handler :       any prolog predicate
%% Event Queue :         any prolog stream
%% ---------------------------------------------------

:- module(events,
        [
                add_handler/2,
                delete_handler/2,
                call_handlers/1,
                event_loop/1,
                event_loop/2
        ]).

%% ---------------------------------------------------

:- use_module(library(read), [read_term/3]).

%% ---------------------------------------------------

:- data event_handler/2.

%% ---------------------------------------------------
%%
%% Set a new handler for an event
%%
%% ---------------------------------------------------

:- meta_predicate(add_handler(?,goal)).

add_handler(Event,Handler) :-
        nonvar(Event),
        assertz_fact(event_handler(Event,Handler)).

%% ---------------------------------------------------
%%
%% Remove any handler which unifies with the given
%% params...
%%
%% ---------------------------------------------------

:- meta_predicate(delete_handler(?,goal)).

delete_handler(Event,Handler) :-
        retractall(event_handler(Event,Handler)).

%% ---------------------------------------------------
%%
%% Call all handlers associated with Event.
%% Notice that Event may contain free variables !!!
%%
%% This predicate provides the chance to
%% implement a user-defined event loop.
%% ---------------------------------------------------

call_handlers(Event) :-
        event_handler(Event,Handler),
        launch(Handler),
        fail.

call_handlers(_).


launch(Handler) :-
        call(Handler),
        !.

launch(_).

%% ---------------------------------------------------
%%
%% Enter a loop in order to receive events and
%% launch associated handlers.
%%
%% A "stop" event may be defined in event_loop/2...
%%
%% Assume non-deterministic execution order for the
%% handlers.
%% 
%% ---------------------------------------------------

event_loop(Stream) :-
        ( read_term(Stream,Event,[]) ->
                true
                ;
                ( throw(invalid_prolog_event(Stream)), fail )
        ),
        call_handlers(Event),
        event_loop(Stream).

event_loop(_).


event_loop(Stream,StopEvent) :-
        nonvar(StopEvent),
        ( read_term(Stream,Event,[]) ->
                true
                ;
                ( throw(invalid_prolog_event(Stream)), fail )
        ),
        Event \== StopEvent,
        call_handlers(Event),
        event_loop(Stream,StopEvent).

event_loop(_,_).
