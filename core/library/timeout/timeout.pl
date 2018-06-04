:- module(timeout, [
	call_with_time_limit/2, 
	call_with_time_limit/3
   ], [foreign_interface]).

:- doc(title, "Call with timeout").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Jos@'{e} F. Morales (minor fixes)").

:- doc(module, "@begin{alert}Experimental@end{alert}").

:- doc(bug, "Implement without threads").
:- doc(bug, "See limitations of @pred{call_with_time_limit/3}.").

:- pred call_with_time_limit(+Goal, +Time, +Handler) ::
     callable * int * callable 
   # "Succeed if @var{Goal} completes within @var{Time}
      milliseconds. @var{Goal} is executed as in @pred{once/1}. If
      @var{Goal} doesn't complete within @var{Time} milliseconds
      (wall time), exit and call @var{Handler}.".

:- doc(call_with_time_limit/3, "Please note that this predicate uses
   @pred{control_c} exception and therefore is not capable to break
   out of long running goals such as @pred{sleep/1}, blocking I/O or
   other long-running (foreign) predicates. Blocking I/O can be
   handled using the timeout option of @pred{read_term/3}. Moreover it
   can accidently catch a @pred{control_c}").

:- meta_predicate(call_with_time_limit(+, :, :)).
call_with_time_limit(Time, Call, Handler):- 
	Time > 0, !,
	init_alarm(Time, Id),
	catch(begin(Id, Call),
	      E,
	      on_exception(E, Id, Handler)).
call_with_time_limit(_Time, _Call, Handler):- 
	call(Handler).

:- meta_predicate(begin(+, :)).
begin(Id, Call):- 
	start_alarm(Id),
	% TODO: 'control_c' is a signal; any safe way of throwing exception directly?
	( intercept(Call, control_c, throw(time_limit_exceeded)) -> OK = yes
	; OK = no
	),
	stop_alarm(Id),
	OK = yes.

:- meta_predicate(on_exception(+, +, :)).
on_exception(E, Id, Handler) :-
	stop_alarm(Id),
	( E = time_limit_exceeded,
	  alarm_stat(Id, _, true, true) ->
	    % garbage,
	    call(Handler)
	; % garbage,
	  throw(E)
	).

:- set_prolog_flag(multi_arity_warnings,off).
:- pred call_with_time_limit(Call, Time) :: callable * int # " equivalente to 
	@pred(call_with_time_limit(Call, Time, throw(time_limit_exceeded))).".
   
:- meta_predicate(call_with_time_limit(+, :)).
call_with_time_limit(Time, Call):-
	call_with_time_limit(Time, Call, throw(time_limit_exceeded)).
:- set_prolog_flag(multi_arity_warnings,on).

% ---------------------------------------------------------------------------
% C Interface for alarm

:- use_module(library(foreign_interface/foreign_interface_properties), [null/1]).
:- use_module(library(global_vars), [setval/2, getval/2]).

init_alarm(Time, Id):-
	global_vars:getval(last, Last),
	( var(Last) -> 
	    foreign_interface_properties:null(Last)
	; true
	),
	init_alarm_c(Time, Last, Id), 
	( foreign_interface_properties:null(Id)->
	    throw(init_alarm_fail)
	; global_vars:setval(last, Id)
	).

:- true pred init_alarm_c(in(Time), in(Lats), go(Id)) :: c_int *  address * address 
	+ (returns(Id), foreign(init_alarm)). 

start_alarm(Id):-
%	garbage, 
	start_alarm_c(Id, 1).

:- true pred start_alarm_c(in(Id), go(Status)) :: address * c_int
	+ (returns(Status), foreign(start_alarm)).

stop_alarm(Id):-
%	garbage, 
	stop_alarm_c(Id).

:- true pred stop_alarm_c(in(Id)) :: address
	+ (foreign(stop_alarm)).

alarm_stat(Id, State, Send, IsOldest):-
%	garbage, 
	alarm_stat_c(Id, Bits), 
	StateBits is Bits /\ 3, 
	( StateBits = 0 -> State = initialized
	; StateBits = 1 -> State = running
	; StateBits = 2 -> State = terminated
	; State = killed
	),
	SendBits is Bits /\ 4,
	( SendBits = 4 -> Send = true
	; Send = false
	),
	IsOldestBits is Bits /\ 8, 
	( IsOldestBits = 8 -> IsOldest = true
	; IsOldest = true
	).

:- true pred alarm_stat_c(in(ID), go(State)) :: address * c_int
	+ (returns(State),  foreign(alarm_stat)). 

% garbage:- 
% 	global_vars:getval(last, Id), 
% 	(
% 	    nonvar(Id) ->
% 	    true
% 	;
% 	    garbage_c(Id)
% 	).
% 
% :- true pred garbage_c(in(Last)) :: address 
% 	+ (foreign(force_garbage)).

:- use_foreign_source('alarm.c').

