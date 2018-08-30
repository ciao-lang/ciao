:- module(actmod_dist, [], [assertions, regtypes, fsyntax]).

:- doc(title, "Distribution protocol for active modules").
:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "This is the code that handles setting up nodes in a distributed
   environment, and sending and receiving messages (see mailbox)
   between active module instances.

   @begin{note}
   The current distribution protocol provide no means for
   security. The application must take care of this.
   @end{note}

   This includes:
   @begin{itemize}
   @item Low-level TCP/IP socket connection between nodes
   @item Publishing/locating ActRef names in a registry
   @item Sending/receiving messages
   @end{itemize}

   A node is an OS process (currently identified by its
   Hostname:Port). Each node may contain many active module
   instances. The distributed protocol requires a mapping between the
   active module instance identifiers and the precise location of the
   node on the network (its address).

   This mapping is maintained by a @em{registry} of addresses of
   active module instances. We allow differerent @concept{registry
   protocols} to publish and locate these addresses (see @ref{Registry
   protocols}).

   The current implementation for communication between active modules
   is based on TCP/IP sockets (thus, the address of an active module
   instance is an IP socket address in a particular machine).

   Requests to execute goals in a module are sent as messages through
   the mailbox. When the mailbox corresponds to a remote active module
   instance, the messages are send over TCP/IP socket connection
   between the local and remote nodes. Similary, response messages
   that encode the answers are retrieved through sockets into local
   mailboxes.

   @section{Distributed nodes}
   
   When using the @tt{dist_node} directive, the executable serves as a
   @emph{node} for distributed active modules. It accepts following
   command-line options:

@begin{verbatim}
-p Port                     Port number for listening connection
-a Address                  Address for listening connection
--reg-protocol RegProtocol  Registry protocol
--reg-addr Address Port     Registry address (optional)
--reg-dir Path              Registry directory (optional)
--name Name                 Publish name for the (default) active module instance
@end{verbatim}

   Some of them can be controlled through the following environment
   variables:

@begin{verbatim}
ACTMOD_HOSTNAME        Address for listening connection
@end{verbatim}

   Other declarations:

   @begin{itemize}
   @item @tt{:- dist_start(G)}: specify a default starting goal to be
     executed once the node is ready to accept connections.
   @item @tt{:- actmod_reg_protocol(<RegProtocol>)}: default registry
     protocol used in the context of the current active module
   @end{itemize}

   @section{Registry protocols}
   
   @begin{note}
   The implementation currently uses the experimental @lib{traits}
   package (similar to @tt{multifile} predicates but with better
   control on the hooks interface).
   @end{note}
   
   A registry protocol implements the predicates to publish and locate
   addresses of active module instances. By convention, the
   implementation of a registry protocol @tt{RP} is encoded in a module
   with name @tt{regp_<RP>}.
   
   The @em{publish} part of the protocol must implement the
   @tt{actmod_publish} trait, defining the @pred{save_addr/5}
   predicate. This predicate publishes the current active module address.
   
   The @em{locate} part of the protocol must implement the
   @tt{actmod_locate} trait, defining the @pred{remote_address/3}
   predicate. This predicate locates the address of the specified active
   module.
   
   For efficiency, the locate methods maintain a cache of addresses, so
   that the server information only needs to be read from the file system
   the first time the active module is accessed.
   
   The available implementations are:
   @begin{itemize}
   @item @lib{regp_filebased} (default)
   @item @lib{regp_platformbased}
   @item @lib{regp_webbased}
   @end{itemize}
").

:- use_module(engine(data_facts)).

:- use_module(library(system), [getenvstr/2]).
:- use_module(library(lists), [member/2]).

:- include(library(fibers/fibers_hooks)).
:- include(library(actmod/actmod_hooks)).
:- use_module(library(actmod/actmod_rt)).
:- use_module(library(fibers/stream_watchdog)).

% ----------------------------------------------------------------

% This ActRef is physically in this running OS process
actI_in_process(ActRef) :-
	DMod = ActRef, % TODO:T253 see [new-actref]
	'$local_actmod'(DMod).

% ----------------------------------------------------------------
:- doc(section, "Node state").

% Hostname:Port and socket for this node
:- data curr_addr/2.
:- data curr_socket/1.

% addr_stream(Hostname,Port,Stream):
%   Open socket connection (Stream) from this node to a(Hostname,Port)
:- data addr_stream/3.

reset_state :-
	retractall_fact(curr_addr(_,_)),
	retractall_fact(curr_socket(_)),
	retractall_fact(addr_stream(_,_,_)).

% ----------------------------------------------------------------
:- doc(section, "Initialization").

:- export(dist_init/3).
:- pred dist_init(RegProtocol, Mod, Opts) # "Initialize a node for
   distributed active modules (registry protocol @var{RegProtocol},
   initial module @var{Mod}, options @var{Opts}).".

dist_init(RegProtocol0, Mod, Opts) :-
	reset_state,
	% Get RegProtocol to publish ActRef instance
	( member(reg_protocol(RegProtocol), Opts) -> true
	; RegProtocol = RegProtocol0
	),
	% TODO:T253 see parse_args comment about --name; decide default, etc.
	% TODO:T253 fix named_actRef/2
	( member(name(ActRef), Opts) -> true
	; ActRef = Mod % TODO:T253 use singleton_actRef at least; see [new-actref]
	),
	% TODO:T253 this may not be the right place
	actI_init_named(Mod, ActRef),
	% Get (optional) Hostname, Port
	( member(hostname(Hostname), Opts) -> true
	; true % Leave Hostname unbound
	),
	( member(port(Port), Opts) -> true
	; true % Leave Port unbound
	),
	%
	dist_set_reg_protocol(RegProtocol),
	dist_bind(Hostname, Port),
	dist_publish_actmod(ActRef, Mod, Opts).

:- export(dist_init_args/3).
:- pred dist_init_args(RegProtocol, Mod, Args) # "Like @pred{dist_init/3}
   but parses options from command-line arguments @var{Args}.".

dist_init_args(RegProtocol, Mod, Args) :-
	parse_args(Args, _Rest, Opts),
        dist_init(RegProtocol, Mod, Opts).

% Parse server options from arguments
%
% Options for listening address
parse_args(['-p', PortAtm|Args], Rest, Opts) :- % Port for listening address
        atom_number(PortAtm, Port),
	!,
	Opts = [port(Port)|Opts0],
	parse_args(Args, Rest, Opts0).
parse_args(['-a', Hostname|Args], Rest, Opts) :- % Hostname for listening address
	!,
	Opts = [hostname(Hostname)|Opts0],
	parse_args(Args, Rest, Opts0).
% Options for registry
parse_args(['--reg-protocol', RegProtocol|Args], Rest, Opts) :- % Registry protocol
	!,
	Opts = [reg_protocol(RegProtocol)|Opts0],
	parse_args(Args, Rest, Opts0).
parse_args(['--reg-addr', Hostname, PortAtm|Args], Rest, Opts) :- % Registry address (e.g., for name server based)
	!,
        atom_number(PortAtm, Port),
	RegAddr = a(Hostname,Port),
	Opts = [reg_addr(RegAddr)|Opts0],
	parse_args(Args, Rest, Opts0).
parse_args(['--reg-dir', Path|Args], Rest, Opts) :- % Path for registry directory (e.g., for .addr files)
	!,
	Opts = [reg_dir(Path)|Opts0],
	parse_args(Args, Rest, Opts0).
%
% Options for default actmod instance
parse_args(['--name', Name|Args], Rest, Opts) :- % TODO: Name is ActRef; DMod is missing; add multiple (start many actmods)
	!,
	Opts = [name(Name)|Opts0],
	parse_args(Args, Rest, Opts0).
%
parse_args(Args, Args, []).

% ----------------------------------------------------------------
:- doc(section, "Socket connections").

:- use_module(library(system), [current_host/1, get_pid/1]).
:- use_module(library(sockets), [connect_to_socket/3, bind_socket/3]).

% set_addr_and_socket(+Hostname, +Port, +Socket): 
%   Set Hostname:Port and socket of node (socket)
set_addr_and_socket(Hostname, Port, Socket) :-
	retractall_fact(curr_addr(_,_)),
	retractall_fact(curr_socket(_)),
	assertz_fact(curr_addr(Hostname, Port)),
	assertz_fact(curr_socket(Socket)).

% :- export(dist_bind/2).
% dist_bind(?Hostname, ?Port):
%   Create a socket bound to address Hostname:Port.
%   Both Hostname and Port can be unbound.

dist_bind(Hostname, Port) :-
	( nonvar(Hostname) -> true
	; getenvstr('ACTMOD_HOSTNAME', Hostname0) ->
	    atom_codes(Hostname, Hostname0)
	; current_host(Hostname)
	),
        bind_socket(Port, 5, Socket),
	set_addr_and_socket(Hostname, Port, Socket),
	watch_socket(Socket, actmod_msg(any)).

% :- export(dist_disconnect/0). % TODO: untested
dist_disconnect :-
	( curr_socket(Socket) ->
	    retractall_fact(curr_socket(_)),
	    unwatch_socket(Socket)
	; true
	).

:- use_module(engine(stream_basic), [current_stream/3]).

:- export(get_addr_stream/2).
% Obtain a socket connection from this node to a(Hostname,Port)
% TODO: close on error?
get_addr_stream(a(Hostname,Port), Stream) :-
        current_fact(addr_stream(Port, Hostname, Stream)),
        current_stream(_N,socket,Stream), !.
get_addr_stream(a(Hostname,Port), Stream) :-
	connect_to_socket(Hostname, Port, Stream),
        retractall_fact(addr_stream(Port, Hostname, _)),
        assertz_fact(addr_stream(Port, Hostname, Stream)).

% ----------------------------------------------------------------
:- doc(section, "Publishing").

% :- export(dist_publish_actmod/3).
% TODO: publish node names instead of actmods too?
dist_publish_actmod(ActRef, Mod, Opts) :-
	curr_addr(Hostname, Port), Address = a(Hostname, Port),
	dist_get_reg_protocol(RegProtocol),
	get_pid(Pid),
	(RegProtocol as actmod_publish).save_addr(ActRef, Mod, Address, Pid, Opts),
        dist_log(['published ', ~~(Mod), ' node [pid=', Pid, ', addr=', ~~(Address), '] reg_protocol=', ~~(RegProtocol)]).

% ----------------------------------------------------------------
:- doc(section, "Sockets send/receive").

:- use_module(library(sockets/sockets_io),
	[socket_send_term/2, socket_recv_term/2]).

:- export(dist_send/2).
% TODO:T253 (!) use actchn
dist_send(addr(Addr), X) :-
        get_addr_stream(Addr, Stream),
        socket_send_term(Stream, X).
dist_send(stream(Stream), X) :-
	socket_send_term(Stream, X).

% :- export(dist_recv/2).
% TODO:T253 (!) use actchn
dist_recv(stream(Stream), X) :-
	socket_recv_term(Stream, X).

% ---------------------------------------------------------------------------
% Registry protocol (for ActRef names)

% TODO: currently only for register/lookup ActRef addresses
:- data dist_reg_protocol/1.

:- export(dist_get_reg_protocol/1).
dist_get_reg_protocol(RegProtocol) :-
	current_fact(dist_reg_protocol(RegProtocol)).

:- export(dist_set_reg_protocol/1).
dist_set_reg_protocol(RegProtocol) :-
	retractall_fact(dist_reg_protocol(_)),
	asserta_fact(dist_reg_protocol(RegProtocol)).

% Preferred RegProtocol for locating the remote address for DMod active module instances
dmod_get_reg_protocol(DMod, RegProtocol) :-
	( '$dmod_reg_protocol'(DMod, RegProtocol0) ->
	    RegProtocol = RegProtocol0
	; dist_reg_protocol(RegProtocol0) ->
	    RegProtocol = RegProtocol0
	; throw(error(no_reg_protocol_for_dmod(DMod), dmod_get_reg_protocol/2))
	).

:- export(actI_alloc_named/2).
% actI_alloc_named(+DMod, +ActRef): Reserve the identifier ActRef
%   for DMod in the registry (if needed)
%   (address is not assigned yet)
actI_alloc_named(DMod, ActRef) :-
	dmod_get_reg_protocol(DMod, RegProtocol),
	(RegProtocol as actmod_locate).cleanup_actI(ActRef).

% ---------------------------------------------------------------------------

:- export(actref/1).
:- regtype actref(X) # "Reference to an active module instance".
actref(_).

:- export(actchn/1).
:- regtype actchn(X) # "Channel for active module instance communication".
actchn(null). % Null channel
actchn(local(ActRef)) :- % local (same process)
	actref(ActRef).
actchn(addr(_, ActRef)) :- % some a/2 address
	actref(ActRef).
actchn(stream(_Stream, ActRef)) :- % specific stream (e.g., for bidirectional socket)
	actref(ActRef).
actchn(response_chn(ActChn, CalleeRef)) :- % a channel specific to response/2 messages
	actchn(ActChn),
	actref(CalleeRef).

:- export(chn_actref/2).
% (currently only for tracing)
chn_actref(null, '$unknown').
chn_actref(local(ActRef), ActRef).
chn_actref(addr(_, ActRef), ActRef).
chn_actref(stream(_, ActRef), ActRef).
chn_actref(response_chn(ActChn, _), ActRef) :- chn_actref(ActChn, ActRef).

% ---------------------------------------------------------------------------
% Database of node address for each ActRef.
%
% NOTE: currently limited to singleton instances (ActRef=x corresponds
%   to a singleton instance of module 'x')

% TODO:T253 ActRef vs names (aliases)
% TODO:T253 Use DMod from actI_dmod/2? (at least for consistency)

% actI_dmod(ActRef, DMod):
%   ActRef is an instance of DMod.
:- data actI_dmod/2.
% TODO: impl_dmod instead? merge with traits?

% actI_addr(ActRef, Address):
%   ActRef has address Address
%   Note that two ActRef could have the same node Address.
:- data actI_addr/2.

% Ask remote address
actI_get_addr(ActRef, Addr) :-
	current_fact(actI_addr(ActRef, Addr0)), !,
	Addr = Addr0.
actI_get_addr(ActRef, Addr) :-
	named_actRef(ActRef, DMod), % TODO: [new-actref]
	dmod_get_reg_protocol(DMod, RegProtocol),
	catch((RegProtocol as actmod_locate).remote_address(ActRef, DMod0, Addr),
	      E, address_error(E, ActRef)),
	!, % TODO: really det?
	( DMod == DMod0 -> true
	; % DMod0 at remote is different than the expected DMod
	  throw(error(mismatch_dmod(DMod, DMod0), actI_get_addr/2))
	),
	assertz_fact(actI_dmod(ActRef, DMod)),
	assertz_fact(actI_addr(ActRef, Addr)).
actI_get_addr(ActRef, _Addr) :-
	throw(error(address_not_found(ActRef), actI_get_addr/2)).

address_error(error(existence_error(source_sink,_),_), ActRef) :- !,
	throw(error(existence_error(actmod, ActRef), actI_get_addr/2)).
address_error(E, _) :- throw(E).

% % TODO: unused!
% % DMod for ActRef
% % (get from static actRef defs or from db)
% actI_get_dmod(ActRef, DMod) :-
% 	named_actRef(ActRef, DMod0), !, % (declared static actRef)
% 	DMod = DMod0.
% actI_get_dmod(ActRef, DMod) :-
% 	% (registered)
% 	current_fact(actI_dmod(ActRef, DMod0)), !,
% 	DMod = DMod0.
% actI_get_dmod(_ActRef, _DMod) :-
% 	throw(error(unknown_actRef, actI_get_dmod/2)).

% Reset memorized dmod
actI_reset_dmod(ActRef) :-
	retractall_fact(actI_dmod(ActRef, _)).

% Reset memorized address
actI_reset_addr(ActRef) :-
	retractall_fact(actI_addr(ActRef, _)).

% ---------------------------------------------------------------------------
:- doc(section, "actmod send message").

:- export(actchn_send/2).
% Send a message through ActChn
% TODO:T253 (!) use actchn for assertion; better a trait?
actchn_send(ActChn, r(QProt,Request,CallerRef)) :- ActChn = local(_), !,
	ResponseChn = local(CallerRef),
	Msg = rch(QProt,Request,ResponseChn),
	actchn_send(ActChn, Msg).
actchn_send(response_chn(ActChn, CalleeRef), Msg) :- !, 
	( Msg = response(CalleeRef, Response) ->
	    true
	; % this channel is exclusive for response/2 messages
	  throw(error(invalid_message, actchn_send/2))
	),
	actchn_send(ActChn, Response).
%
actchn_send(local(ActRef), Msg) :- !,
	actref_send(ActRef, Msg).
actchn_send(addr(Addr, ActRef), Msg) :- !,
	catch(dist_send(addr(Addr), Msg), E, send_error(E, ActRef)).
actchn_send(stream(Stream, _ActRef), Msg) :- !,
	dist_send(stream(Stream), Msg). % TODO: catch errors
actchn_send(ActChn, _Msg) :-
	throw(unsupported_actchn(ActChn)).

:- export(actref_to_actchn/2).
% Guess channel for ActRef (local or remote)
actref_to_actchn(ActRef) := ActChn :-
	( actI_in_process(ActRef) ->
	    ActChn = local(ActRef)
	; actI_get_addr(ActRef, Addr),
	  ActChn = addr(Addr, ActRef)
	).

% ---------------------------------------------------------------------------

% TODO: actchn error? try to recover?
send_error(error(system_error, 'sockets:connect_to_socket_type'/4-1), ActRef) :- !,
	actI_reset_dmod(ActRef), % Reset memoized dmod
	actI_reset_addr(ActRef), % Reset memoized address
	throw(error(connection_error(actmod, ActRef), actI_send_call/3)).
send_error(E, _) :- throw(E).

% TODO: improve, failure is not the right thing
recv_error(E) :-
	dist_log(['socket error: ', ''(E)]),
	fail.

% ---------------------------------------------------------------------------

:- export(actchn_watch_response/2).
% (if needed) register stream response
actchn_watch_response(ActChn, CallerRef) :-
	( ActChn = local(_) ->
	    true
	; ActChn = addr(Addr, ActRef) ->
	    get_addr_stream(Addr, Stream),
	    watch_stream(Stream, actmod_msg(recv_response(CallerRef,ActRef))) % TODO: use ActRef or some kind of NodeId
	; throw(unsupported_actchn(ActChn))
	).

:- export(actchn_unwatch_response/1).
% (if needed) unregister stream watch
actchn_unwatch_response(ActChn) :-
	( ActChn = local(_) ->
	    true
	; ActChn = addr(Addr, _) ->
	    get_addr_stream(Addr, Stream),
	    unwatch_stream(Stream)
	; throw(unsupported_actchn(ActChn))
	).

% ---------------------------------------------------------------------------
% Handler for stream data
%
% `actmod_msg(MsgTy)` is the handler for data from watched streams, where
% `MsgTy` indicates the partial information that is not sent through the
% stream and that will be used to reconstruct the final term:
%
%    - any: any message
%    - recv_response(_,_): a response through a response channel

:- use_module(engine(stream_basic), [close/1]).

% (fibers_hooks)
'$handle_stream'(actmod_msg(MsgTy), Stream) :- !,
	catch(dist_recv(stream(Stream), X),E,recv_error(E)),
	( X = end_of_file -> % end of file, that is, a broken connection
	    unwatch_stream(Stream),
	    close(Stream)
	; msg_decode(MsgTy, X, Stream, ActChn, Msg),
	  actchn_send(ActChn, Msg)
	).

msg_decode(any, r(QProt,Request,CallerRef), Stream, ActChn, Msg) :- !, % call
	get_actI(Request, ActRef),
	CallerChn = stream(Stream, CallerRef),
	ResponseChn = response_chn(CallerChn, ActRef),
	ActChn = local(ActRef),
	Msg = rch(QProt,Request,ResponseChn).
%
msg_decode(any, s(QProt,Request), _Stream, ActChn, Msg) :- !, % cast
	get_actI(Request, ActRef),
	ActChn = local(ActRef),
	Msg = s(QProt,Request).
%
msg_decode(recv_response(ActRef,CalleeRef), X, _Stream, ActChn, Msg) :- !, % response
	% (a response through a response channel)
	% unwatch_stream(Stream), % TODO: remove if OK, unwatching later
	ActChn = local(ActRef),
	Msg = response(CalleeRef,X).

% ---------------------------------------------------------------------------
% (move somewhere)

% TODO: format:format_to_string/3 implementation is not complete
:- use_module(library(format_to_string), [format_to_string/3]).
% :- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(library(read_from_string), [read_from_atom/2]).

:- export(term_to_atom/2).
term_to_atom(X, A) :-
	format_to_string("~q", [X], Str),
	atom_codes(A, Str).

% TODO: read_from_atom/2 is buggy (uses a pipe), read_from_string_atmvars/2 does not support quotes '...' or "..."; use fast_read when possible
:- export(atom_to_term/2).
atom_to_term(Atom, Term) :-
%	( atom_codes(Atom, Str),
%	  read_from_string_atmvars(Str, Term0) ->
%	    Term = Term0
%	; read_from_atom(Atom, Term)
%	).
	read_from_atom(Atom, Term).

% ---------------------------------------------------------------------------

:- doc(bug, "allow other implementations (it should be possible)").

:- doc(bug, "merge with eng_call(serve_socket) like in javasock.pl").

:- doc(bug, "merge with ciaod (blackboard)").

:- doc(bug, "merge with andprolog_d").


