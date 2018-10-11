:- module(actmod_rt, [], [assertions, regtypes, hiord, fsyntax, datafacts]).

% :- compilation_fact(trace_actmod). % Uncomment to enable tracing (for debugging)


:- include(library(actmod/actmod_hooks)).
:- include(library(fibers/fibers_hooks)).

:- use_module(engine(io_basic)).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(fibers/fibers_rt)).
:- use_module(library(actmod/actmod_dist)).
:- use_module(library(lists), [member/2, append/3]).

% ---------------------------------------------------------------------------
:- doc(section, "The mailbox").

% Mailboxes are the data structures that used to send and receive
% messages between active module instances (identified by ActRef).
%
% actI_mbox(SelfRef, Msg): mailbox for SelfRef where Msg is:
% 
% - rch(QProt,Request,ResponseChn):
%     Call request (r(QProt,Request,ActRef)) from ActRef to SelfRef, where
%     ResponseChn is the channel to send the response back (to ActRef).
%
% - s(QProt,Request):
%     Cast request from an unknown ActRef to SelfRef.
%
% - response(CalleeRef, Response):
%     Response from CalleeRef to a previous request from SelfRef.

% TODO: make it concurrent so that messages can be sent from different
%   workers (for that we require a version that blocks; this may not
%   work fine with the current stream_watchdog)

:- data actI_mbox/2.

:- export(actref_send/2).
actref_send(ActRef, Msg) :-
	assertz_fact(actI_mbox(ActRef,Msg)).

% Messages from mailboxes (for fiber receive)
'$current_msg'(FID, Msg, Ref) :-
	current_fact(actI_mbox(ActRef,Msg), Ref),
	actref_to_fid(ActRef, FID).

% ---------------------------------------------------------------------------
:- doc(section, "Initialization").

:- export('$actmod_start_main'/1).
% '$actmod_start_main'(G): start io_sched calling G and terminating on success
'$actmod_start_main'(G) :- !,
	actmod_start_common(main(G)).

:- export('$actmod_start_nohalt'/1).
% '$actmod_start_nohalt'(G0): start io_sched and wait forever
'$actmod_start_nohalt'(G0) :-
	actmod_start_common(nohalt(G0)).

:- import(actmod_dist, [dist_init_args/3]). % TODO: weak dep

% TODO: better merge
actmod_start_common(nohalt(G0)) :- !,
	io_sched_init,
	( G0 = dist_init_args(RegProtocol, Mod, Args, G) ->
	    dist_init_args(RegProtocol, Mod, Args),
	    ( G = true -> true
	    ; % Trick to set initial goal
	      %% actmod_cast(G) 
	      get_dmod(G, GMod),
	      get_qprot(G, QProt),
	      actI_send_cast(GMod, QProt, G)
	    ) 
	; throw(unknown_g0(G0)) % TODO: fix it
	),
	io_sched(_).
actmod_start_common(main(G)) :-
	io_sched_init,
	get_dmod(G, DMod),
	ActRef = DMod, % TODO:T253 see [new-actref]
	actref_to_fid(ActRef, FID),
	actI_init_named(DMod, ActRef),
	set_fid(FID),
	'$actmod_call'(G). % (This calls io_sched_nested/2)

% ---------------------------------------------------------------------------
:- doc(section, "Actmod instances").

% TODO:T253 [new-actref] fix map between ActRef (really the instance) and DMod in all cases

:- export(named_actRef/2).
named_actRef(ActRef, DMod) :-
	'$static_named_actRef'(ActRef, DMod0),
	!,
	DMod = DMod0.
%%named_actRef(ActRef, DMod) :-
%%	display(user_error, 'NOT_NAMED_ACTREF'(ActRef)), nl(user_error),
%%	abort,
%%	% TODO:T253 do it only for some!
%%	DMod = ActRef. % TODO:T253 see [new-actref]

:- export(singleton_actRef/2).
% Obtain an ActRef for DMod, assumming that there can exist at most one instance 
singleton_actRef(DMod, ActRef) :-
	DMod = ActRef. % [new-actref-2]

:- export(actI_init_named/2).
% (just initializes)
actI_init_named(_DMod, ActRef) :-
	FID = ActRef, % TODO:T253 actref_to_fid? actref_new_fid?
	fiber_set_condsusp(FID, queryloop).

% ---------------------------------------------------------------------------
:- doc(section, "The query loop").

% waiting for queries
:- impl(gsusp, queryloop).
(queryloop as gsusp).guard(_, rch(_,_,_), n).
(queryloop as gsusp).guard(_, s(_,_), n).
%
(queryloop as gsusp).run(Msg) :-
	do_query(Msg).

% (Reused later)
do_query(s(QProt,Request)) :- !,
	setup_query(QProt, Request, QProt2, Request2),
	trace_mbox_Rcast(Request2),
	% TODO: exceptions should be part of the response (at least for some QProt)
	catch((QProt2 as qprot).collect(Request2, _Response2), % TODO: Add version for 'cast'? (no response)
	      E,
	      local_answer_error(QProt2, E)).
        % TODO: errors if _Response2 above (cast) contains a suspension
do_query(rch(QProt,Request,ResponseChn)) :-
	setup_query(QProt, Request, QProt2, Request2),
	CallerRef = ~chn_actref(ResponseChn),
	trace_mbox_Rcall(CallerRef, Request2),
	% TODO: exceptions should be part of the response (at least for some QProt)
	catch((QProt2 as qprot).collect(Request2, Response2),
	      E,
	      local_answer_error(QProt2, E)),
	% TODO: Make sure that ActRef does not change after qprot.collect/2 is called
	trace_mbox_Sresp(CallerRef, Response2),
	(QProt as qprot).enc(Response2, Response),
	get_fid(FID),
	fid_to_actref(FID, ActRef),
	actchn_send(ResponseChn, response(ActRef, Response)).

:- use_module(library(fibers/fibers_data), [t_data_restore/1]). % TODO: ad-hoc

% Decode request and restore fiber state
setup_query(QProt, Request, QProt2, Request2) :-
	(QProt as qprot).dec(Request, QProt2, Request1),
	( Request1 = '$fiberSusp'(FiberData, G0) ->
	    FiberData = fiberData(DMod,State),
	    set_curr_actI_dmod(DMod),
	    t_data_restore(State),
	    Request2 = G0
	; Request2 = Request1
	).

% ---------------------------------------------------------------------------
:- doc(section, "actmod send call / receive response (low-level)").

% TODO:T253 cast to local not working; it needs unblocking a blocked select() -- see wait_streams(off) call

:- export(actI_send_cast/3).
% Send a cast to ActRef (do not watch for response)
actI_send_cast(ActRef, QProt, Request) :-
	trace_mbox_Scast(ActRef, Request),
	actchn_send(~actref_to_actchn(ActRef), s(QProt,Request)).

:- export(actI_send_call/3).
% Send a QProt-based Request to ActRef from SelfRef (and watch for a response)
actI_send_call(ActRef, QProt, Request) :-
	trace_mbox_Scall(ActRef, Request),
	actmod_get_self(SelfRef),
	actchn_send(~actref_to_actchn(ActRef), r(QProt,Request,SelfRef)).

% ---------------------------------------------------------------------------

:- export(actI_receive_response/2).
% Suspend until we get a response Response from CalleeRef
actI_receive_response(CalleeRef, Response) :-
	actmod_get_self(SelfRef),
	%
	CalleeChn = ~actref_to_actchn(CalleeRef),
	actchn_watch_response(CalleeChn, SelfRef),
	%
	% Wait until we get a response from CalleeRef (on SelfRef):
	%  - Non-matching messages are queued
	%  - Other ActRef may be processed
	%  - Other query on SelfRef may be processed with 'response_from_or_queryloop'
	%
	( CalleeChn = local(_) -> % TODO:T253 document; why decide on CalleeRef and not SelfRef? decide per pred instead?
	    ResponseSusp = response_from_or_queryloop(CalleeRef) % TODO: response_from_or_queryloop requires nested calls (ala "ghost" frames)
	; ResponseSusp = response_from(CalleeRef)
	),
	% Example of io_sched_nested:
	%   - A send a call to B and waits for a response
	%   - B calls A meanwhile and waits for a response
	%   - B receives answer from A, and then answers A
	%
	fiber_wait(ResponseSusp, got_response(Response)).

% NOTE: response_from/1 and response_from_or_queryloop/1 only accepts
%   response/2 messages if the scheduler exit continuation is the
%   handler for responses.

% TODO: avoid response_from_or_queryloop/1? use a temporary fiber to handler the response (or new queries).

% waiting for responses from CalleeRef
:- impl(gsusp, response_from/1).
(response_from(CalleeRef) as gsusp).guard(yes, response(CalleeRef,_), u).
%
(response_from(_) as gsusp).run(_) :- fail. % (not here see got_response/2)

% waiting for responses from CalleeRef or queries
:- impl(gsusp, response_from_or_queryloop/1).
(response_from_or_queryloop(_) as gsusp).guard(_, rch(_,_,_), n).
(response_from_or_queryloop(_) as gsusp).guard(_, s(_,_), n).
(response_from_or_queryloop(CalleeRef) as gsusp).guard(yes, response(CalleeRef,_), u).
%
(response_from_or_queryloop(_) as gsusp).run(Msg) :-
	do_query(Msg).

% TODO: add a suspendable(_) version? (no io_sched_nested/2)
got_response(Msg, Response) :-
	Msg = response(CalleeRef,Response),
	CalleeChn = ~actref_to_actchn(CalleeRef),
	actchn_unwatch_response(CalleeChn), % TODO: OK? unwatching later
	trace_mbox_Rresp(CalleeRef, Response).

% ---------------------------------------------------------------------------
:- doc(section, "actmod metacall/metacast").
% TODO: missing suspendable version

:- export(actmod_call/1).
% Metacall to an active module (from a non-susp context)
% NOTE: Requires being called from a sched context
actmod_call(DMod:P) :-
	'$actmod_call'(DMod:P).

:- export(actmod_cast/1).
% Metacast (like metacall but for cast) (both from susp and non-susp contexts)
% NOTE: It does not expect a result
% NOTE: It may not work as expected if a suspension is returned
actmod_cast(DMod:P) :-
	actI_send_cast(DMod, allsols, DMod:P). % TODO: add to outgoing queue and use a lock!

:- export('$actmod_call'/1).
% Run G and wait synchronously until all suspended goals are completed.
% TODO: merge with rpc_run at ciao-actmod.js
%
% Exceptions:
%   error(connection_error(actmod, Mod),'$actmod_call'/1):
%     Could not connect to active module Mod
%   error(existence_error(actmod, Mod),'$actmod_call'/1):
%     Could not find address to active module Mod

'$actmod_call'(G) :-
	var(G), !, throw(error(unbound_goal, '$actmod_call'/1)).
'$actmod_call'(G) :-
	% Try to run it here (get residue if we could not)
	Residue = [G],
	run_task(Residue, nostop, '$nomod', Residue2),
	% Throw an error if the residue is not empty
	% (this predicate must be called from a non-async pred)
	check_empty_residue(Residue2).
	
check_empty_residue(Residue) :-
	( Residue = [] -> true
	; throw(error(non_empty_residue, '$actmod_call'/1))
	).

% ---------------------------------------------------------------------------
:- doc(section, "Query protocols").

:- use_module(library(aggregates), [findall/3]).

% allsols: list of all solutions, no residue
:- impl(qprot, allsols).
(allsols as qprot).dec(R, allsols, R).
(allsols as qprot).enc(R, R).
%
(allsols as qprot).prepare_query(Mod:Goal, Request, Binder) :-
	Request = Mod:Goal, % TODO: data and vars
	Binder = Goal. % TODO: only vars
%
(allsols as qprot).join_answers(Binder, _DefTopMod, _ResponseTopMod, Response, Residue, Residue2) :-
	Goal = Binder,
	Answers = Response,
	ResidueCont = [],
        member(Goal, Answers), % TODO: nondet
	append(ResidueCont, Residue, Residue2).
%
(allsols as qprot).collect(Request, Response) :-
	Request = Mod:Goal,
	Response = Answers,
	'$actmod_meta'(Mod, Goal, Meta),
	% Collect all solutions
        findall(Goal,Meta,Answers).

% async: up to one solution, optional residue
:- impl(qprot, async).
(async as qprot).dec(R, async, R).
(async as qprot).enc(R, R).
%
(async as qprot).prepare_query(MG, Request, Binder) :-
	Request = MG, % TODO: data and vars
	Binder = none. % TODO: not needed in async?
%
(async as qprot).join_answers(_Binder, DefTopMod, ResponseTopMod, Response, Residue, Residue1) :-
	( DefTopMod = ResponseTopMod ->
	    GCont = Response
	; GCont = [with_actref(ResponseTopMod, Response)] % TODO: apply with_actref to every item?
	),
	append(GCont, Residue, Residue1).
%
% TODO: run just one iteration of run_task here -- the rest at on_query; try to merge 
(async as qprot).collect(Request, Response) :-
	Request = Query,
	Response = Residue,
	run_task([Query], noglobal, '$nomod', Residue).

'$actmod_meta'(Mod, Goal, Meta) :-
        ( '$actmod_exe'(Goal, Mod, Meta) -> true
        ; functor(Goal, F, N),
	  message(error, ['could not find active module predicate ', ''(Mod), ':', ''(F/N)]),
          Meta = fail % TODO: throw error instead?
        ).

local_answer_error(QProt, E) :-
	display(user_error, local_answer_error(QProt, E)),
	nl(user_error),
	fail. % TODO: Do not fail? do not use display?

% ---------------------------------------------------------------------------

% Run a task (conjunction of goals). The stop condition is:
%  - Stop=nostop: try to execute until the residue is empty
%  - Stop=noglobal: suspend execution on the first goal that is
%    whose code
%    
% The variable @var{Residue} is unified with the suspended execution.

% TODO: See @pred{once_port_reify/2}, etc.
% TODO: See rpc_run(), rpc_next() at ciao-actmod.js for the JavaScript equivalent

run_task([], _Stop, _, []) :- !.
run_task([with_actref(NewDefTopMod, Residue0)|Residue], Stop, DefTopMod, Residue2) :- !,
	run_task(Residue0, Stop, NewDefTopMod, Residue1),
	( Residue = [] -> Residue2 = []
	; append(Residue1, [with_actref(DefTopMod, Residue)], Residue2b),
	  run_task(Residue2b, Stop, NewDefTopMod, Residue2)
	).
run_task(Gs, Stop, _, Residue) :- suspend_task(Gs, Stop), !,
	% Suspend task execution at this point
	Residue = Gs.
run_task([G|Gs], Stop, DefTopMod, Residue) :-
	run_goal(G, DefTopMod, Gs, Gs2),
	run_task(Gs2, Stop, DefTopMod, Residue).

% Decide if the scheduler must suspend, depending on the Stop condition:
%  - Stop=nostop: do not stop
%  - Stop=noglobal: stop on external goals (\+ actI_in_process)
suspend_task(Gs, Stop) :-
	Gs = [G|_],
	( G = 'fibers_blt.yield_residue'(_) ->
	    % Force residue for this builtin % TODO: better way?
	    true
	; Stop = nostop ->
	    fail % Do not stop
	; % Stop = noglobal
	  get_dmod(G, GMod),
	  % Stop only if GMod not in this process
	  \+ mod_is_local(GMod) % TODO: use ActRef and actI_in_process/1
	).

get_dmod(G, GMod) :-
	( G = GMod0:_Goal -> GMod = GMod0
	; GMod0 = ~fnct_mod(G), fnct_property(G, async) -> GMod = GMod0
	; GMod = '$unknown_mod' % TODO: sure?
	).

mod_is_local(GMod) :-
	\+ GMod = '$unknown_mod',
	'$curr_mod'(GMod).

get_qprot(G, QProt) :-
	( G = (_:_) -> QProt = allsols
	; QProt = async
	).

% TODO: add 'delegate' mod protocol? (see proxy)

run_goal(G, DefTopMod, Residue, Residue1) :-
	get_dmod(G, GMod),
	get_qprot(G, QProt),
	(QProt as qprot).prepare_query(G, Request, Binder), % TODO: prepare it here or during residue queuing?
	( QProt = async, mod_is_local(GMod) ->
	    DefTopMod2 = GMod,
	    ActRef = GMod,
	    local_run_async_(Request, Response)
	; rewrite_request(QProt, Request, Request2, DefTopMod, ActRef),
	  DefTopMod2 = DefTopMod,
	  actI_send_call(ActRef, QProt, Request2),
	  actI_receive_response(ActRef, Response)
	),
	(QProt as qprot).join_answers(Binder, DefTopMod2, ActRef, Response, Residue, Residue1). % TODO: how can I handle nondet here? (for QProt=allsols is done; for async?)

% Rewrite using local proxy defs and guess TopMod
rewrite_request(allsols, Request, Request2, _DefTopMod, ActRef) :- !,
	Request = (Mod:_),
	Request2 = Request,
	ActRef = Mod. % TODO:T253 this may not be correct, allow proxy, etc? see [new-actref]
rewrite_request(async, MG0, MG2, _DefTopMod, ActRef) :-
	Mod0 = ~fnct_mod(MG0), fnct_property(MG0, async),
	!,
	( '$dmod_proxy'(Mod0, ProxyMod),
	  unconcat(MG0, Mod0, G) ->
	    ActRef = ProxyMod, % TODO:T253 see [new-actref]
	    mod_concat_head(ProxyMod, G, MG2)
	; ActRef = Mod0, % TODO:T253 this may not be correct; see [new-actref]
	  MG2 = MG0
	).
rewrite_request(async, MG0, MG2, DefTopMod, ActRef) :-
	% MG0 is unknown! Use DefTopMod (i.e., TopMod that originated this residue) % TODO: annotate individual MG?
	MG2 = MG0,
	ActRef = DefTopMod. % TODO:T253 see [new-actref]

% TODO: like module_split but using '.'
% unconcat(+MG, +Mod, -G): Remove 'Mod.' from MG functor name to obtain G
unconcat(MG, Mod, G) :-
	MG =.. [MN|As],
	atom_concat(Mod, '.', Mod_),
	atom_concat(Mod_, N, MN),
	!,
	G =.. [N|As].

local_run_async_('fibers_blt.fiber_meta_call'(G0), GCont) :- !,
	check_mod_concat_head(G0, G), % (expand at runtime)
	GCont = [G].
local_run_async_('fibers_blt.residue_reify'(G0, Residue), GCont) :- !,
	check_mod_concat_head(G0, G), % (expand at runtime)
	run_task([G], noglobal, '$nomod', Residue),
	GCont = [].
local_run_async_(G, GCont) :-
	% (note: currently async.run/1 must be det! get only one solution)
	% (note: G cannot be in a .stub)
	(G as async).run(GCont).

% TODO: Add a separate meta_exp_async goal?
check_mod_concat_head(M:G, G2) :- !,
	mod_concat_head(M, G, G2).
check_mod_concat_head(G, G). % Assume it is already expanded

% ---------------------------------------------------------------------------
% (duplicated)

mod_concat_head(M, H, MH) :-
	H =.. [N|As],
	mod_concat(M, N, MN),
	MH =.. [MN|As].

mod_concat(M, N, MN) :-
	atom_concat(M, '.', MN0),
	atom_concat(MN0, N, MN).

% ---------------------------------------------------------------------------

:- export(get_actI/2).
% ActRef for the given Request
get_actI(Request, ActRef) :-
	( Request = '$fiberSusp'(FiberData, _) ->
	    FiberData = fiberData(DMod, _)
	; get_dmod(Request, DMod)
	),
	ActRef = DMod. % TODO:T253 see [new-actref]

% ---------------------------------------------------------------------------

% TODO: ActRef must have global scope while FID can be local

fid_to_actref(FID, ActRef) :- ActRef = FID.
actref_to_fid(ActRef, FID) :- FID = ActRef.

:- export(force_set_actref/1).
% (needed by serve_http.pl; it should not be needed if ciao-serve main
% starts in a fiber)
force_set_actref(ActRef) :-
	% TODO: missing set actref?
	actref_to_fid(ActRef, FID),
	set_fid(FID),
	fiber_set_condsusp(FID, queryloop).

:- export(actmod_get_self/1).
actmod_get_self(ActRef) :-
	get_fid(FID),
	fid_to_actref(FID, ActRef).

% ---------------------------------------------------------------------------
% actI save/restore (for context switches)

:- data curr_actI_dmod/1.

set_curr_actI_dmod(DMod) :-
	retractall_fact(curr_actI_dmod(_)),
	assertz_fact(curr_actI_dmod(DMod)).

:- export(actmod_get_self_mod/1).
actmod_get_self_mod(DMod) :-
	curr_actI_dmod(DMod).

% ---------------------------------------------------------------------------
:- doc(section, "Tracing").

:- if(defined(trace_actmod)).

trace_mbox_common(Ev) :-
	actmod_get_self(SelfRef),
	display(user_error, '['),
	display(user_error, SelfRef),
	display(user_error, '] '),
	display(user_error, Ev),
	display(user_error, ' ').

trace_mbox_Rcall(CallerRef, Request) :-
	trace_mbox_common('R:call'),
	display(user_error, '<-'),
	display(user_error, CallerRef),
	display(user_error, ' - '),
	display(user_error, Request),
	nl(user_error).

trace_mbox_Rcast(Request) :-
	trace_mbox_common('R:cast'),
	display(user_error, ' - '),
	display(user_error, Request),
	nl(user_error).

trace_mbox_Scall(ActRef, Request) :-
	trace_mbox_common('S:call'),
	display(user_error, '->'),
	display(user_error, ActRef),
	display(user_error, ' - '),
	display(user_error, Request),
	nl(user_error).

trace_mbox_Scast(ActRef, Request) :-
	trace_mbox_common('S:cast'),
	display(user_error, '->'),
	display(user_error, ActRef),
	display(user_error, ' - '),
	display(user_error, Request),
	nl(user_error).

trace_mbox_Rresp(ActRef, Response) :-
	trace_mbox_common('R:resp'),
	display(user_error, '<-'),
	display(user_error, ActRef),
	display(user_error, ' - '),
	display(user_error, Response),
	nl(user_error).

trace_mbox_Sresp(CallerRef, Response) :-
	trace_mbox_common('S:resp'),
	display(user_error, '->'),
	display(user_error, CallerRef),
	display(user_error, ' - '),
	display(user_error, Response),
	nl(user_error).

:- else.

trace_mbox_Rcall(_CallerRef, _Request).
trace_mbox_Rcast(_Request).
trace_mbox_Scall(_ActRef, _Request).
trace_mbox_Scast(_ActRef, _Request).
trace_mbox_Rresp(_ActRef, _Response). 
trace_mbox_Sresp(_CallerRef, _Response). 

:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Logging").

:- export(dist_log/1).
dist_log(Msg) :-
	( get_fid_nothrow(FID) ->
	    fid_to_actref(FID, ActRef)
	; ActRef = '<main>' % no ActRef yet... not a problem for logging
	),
        message(inform, [ActRef, ': '|Msg]).


