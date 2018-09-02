:- module(_, [], [assertions, regtypes, isomodes, hiord]).

:- doc(title, "Call goals with reified IO and (exit) ports").
:- doc(author, "Jose F. Morales").

:- doc(module, "Predicates to delay the execution of the (exit) port
   of a goal and capture IO side-effects (to strings).").

:- doc(bug, "An interface similar to @pred{process_call/3} would be
   simpler").

:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(port_reify), [once_port_reify/2]).
:- use_module(library(io_alias_redirection), [set_stream/3]).

:- export(io_once_port_reify/3).
:- pred io_once_port_reify(Goal, Port, OutString)
	=> (string(OutString))
        #"Executes @var{Goal}, @var{Port} is the state when the
          predicate finishes (true, fail). Its stdout is
          stored in @var{OutString}.".
:- meta_predicate io_once_port_reify(goal, ?, ?).
io_once_port_reify(Goal, Port, OutString) :-
	mktemp_in_tmp('outtextXXXXXX', OutN),
	open(OutN, write, OutS),
	current_output(OutS0),
	set_output(OutS),
	once_port_reify(Goal, Port0),
	set_output(OutS0),
	close(OutS),
	file_to_string(OutN, OutString),
	del_file_nofail(OutN),
	Port = Port0.

% TODO: move somewhere else? duplicated!
:- export(io_once_port_reify/4).
:- pred io_once_port_reify(Goal, Port, OutString, ErrString)
	=> (string(OutString), string(ErrString))
        #"Executes @var{Goal}, @var{Port} is the state when the
          predicate finishes (true, fail). Its stdout and stderr are
          stored in @var{OutString} and @var{ErrString} respectively.".
:- meta_predicate io_once_port_reify(goal, ?, ?, ?).
io_once_port_reify(Goal, Port, OutString, ErrString) :-
	mktemp_in_tmp('outtextXXXXXX', OutN),
	mktemp_in_tmp('outerrXXXXXX', ErrN),
	open(OutN, write, OutS),
	open(ErrN, write, ErrS),
	current_output(OutS0),
	set_output(OutS),
	set_stream(user_error, ErrS, ErrS0),
	once_port_reify(Goal, Port0),
	set_output(OutS0),
	set_stream(user_error, ErrS0, _),
	close(OutS),
	close(ErrS),
	file_to_string(OutN, OutString),
	file_to_string(ErrN, ErrString),
	del_file_nofail(OutN),
	del_file_nofail(ErrN),
	Port = Port0.

