:- module(errhandle, [error_protect/1, handle_error/2], [assertions]).

:- use_module(library(system)).
:- use_module(library(rtchecks/rtchecks_utils)).

:- meta_predicate(error_protect(goal)).

error_protect(Goal) :-
	RTError = rtcheck(_Type, _Pred, _Prop, _Valid, _Poss),
	catch(catch(Goal, RTError, handle_rtcheck_error(RTError)),
	    error(Error, Where), handle_error(Error, Where)).

handle_rtcheck_error(RTError) :-
	handle_rtcheck(RTError),
	abort.

% display_errno :-
% 	c_errno(Errno),
% 	Errno \== 0 ->
% 	display('Last errno was '),
% 	display(Errno            ),
% 	display(': '             ),
% 	c_strerror(StrError),
% 	display(StrError)
%     ;
% 	true.

handle_error(Error, Where) :-
	get_error_message(Error, Where, Message),
	display(user_error, '{'),
	error(Message),
% 	display('{ERROR: '),
% 	display_where(Where),
% 	display(' - '),
% 	display_error(Error),
% 	display(' - '),
% 	display_errno,
% 	display('}'),
% 	nl,
	fail.

get_error_message(Error, Where, Message0) :-
	get_where(Where, Message0, Message),
	get_error(Error, Message, ['}']).

get_where(unknown/ -1, T, T) :- !.
get_where(P/N-A, [P, '/', N, ', arg ', A, ' - '|T], T) :- !.
get_where(P/N,   [P, '/', N, ' - '|T],              T) :- !.
get_where(W,     [W, ' - '|T],                      T).

% OGRAMA: OLD VERSION ---------------------------------------------------
% display_error(instantiation_error) :- !,
% 	display('instantiation error').
% display_error(existence_error(procedure, _P)) :- !,
% 	display('undefined predicate').
% display_error(existence_error(source_sink, S)) :- !,
% 	display_list(['file ', S, ' not found']).
% display_error(permission_error(open, source_sink, S)) :- !,
% 	display_list(['cannot open file ', S]).
% display_error(permission_error(input, stream, S)) :- !,
% 	display_list(['no input permission from ', S]).
% display_error(permission_error(output, stream, S)) :- !,
% 	display_list(['no output permission to ', S]).
% display_error(permission_error(input, past_end_of_stream, S)) :- !,
% 	display_list(['attempt to read past end of stream ', S]).
% OGRAMA: END OLD VERSION -----------------------------------------------
% error code=128

get_error(system_error, ['system error: ', SER|T], T) :- !,
	system_error_report(SER).
%get_error(no_access_permission) :- !,
%	display('no access permission').
get_error(syntax_error,   ['syntax error'|T],              T) :- !.
get_error(resource_error(Type), [Resource_error|T],            T) :- !, 
	translate_resource_error(Type, Resource_error).
get_error(user_error,     ['user error (C interface?)'|T], T) :- !.
get_error(evaluation_error(Type, Culprit),
	    ['evaluation error ', Type, ' in ', Culprit|T], T) :- !.
get_error(representation_error(Type),
	    ['representation error ', Type|T],              T) :- !.
% File does not exist 
get_error(permission_error(open, _Stream, Culprit),
	    ['could not open: ', Culprit|T], T) :- !.
% Permision error
get_error(permission_error(Operation, Object, Culprit),
	    ['permission error: ', Operation,
		' not allowed on ', Object,
		' with value ', Culprit|T], T) :- !.
get_error(existence_error(Type, Culprit), ['existence error: ', Type,
		':', ~~(Culprit), ' does not exist'|T], T) :- !.
get_error(type_error(Type, Culprit),      ['expected ', Readable,
		', found ', Culprit|T],                 T) :- !,
	translate_ball(Type, Readable).
get_error(instantiation_error,
	        ['argument is not sufficiently instantiated'|T], T):-!.
get_error(uninstantiation_error(Culprit), 
	        ['expected an (unbound) variable, found ', Culprit|T], T):-!.
% Value of correct type but outside allowed range
get_error(domain_error(Domain, Culprit), ['expected ', Readable,
		', found ', Culprit|T], T) :- !,
	translate_ball(Domain, Readable).
get_error(syntax_error([L0, L1, Msg, ErrorLoc]),
	    ['syntax error '|Message], T) :- !,
	add_lines(L0, L1, ['\n', [](Msg), ':', ErrorLoc|T], Message).
get_error(X, [X|T], T).

translate_ball(atom,      'a non-numeric atom') :- !.
translate_ball(atomic,    'an atom (including a number)') :- !.
translate_ball(byte,      'a byte') :- !.
translate_ball(callable,  'a callable structure (i.e., a goal)') :- !.
translate_ball(character, 'a character') :- !.
translate_ball(compound,  'a non-atomic term (a structure)') :- !.
translate_ball(evaluable,
	    'an arithmetically evaluable expression') :- !.
translate_ball(in_byte,             'a byte') :- !.
translate_ball(in_character,        'a character') :- !.
translate_ball(integer,             'an integer') :- !.
translate_ball(list,                'a list') :- !.
translate_ball(number,              'a number') :- !.
translate_ball(predicate_indicator, 'a predicate indicator') :- !.
% RH : remove in ISO corregendum 2.
%translate_ball(variable,            'an (unbound) variable') :- !.

translate_ball(flag_value, 'a valid flag value') :- !.

translate_ball(Ball, Ball) :- !. % Not yet a readable form


translate_resource_error(heap, 'heap overflow') :- !.
translate_resource_error(_, 'resource error').
