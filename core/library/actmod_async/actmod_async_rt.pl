:- module(actmod_async_rt, [], [assertions, fsyntax, dcg]).

:- doc(title, "Async active modules (using JSON protocol)").
% TODO: pickle/unpickle?

:- doc(module, "
   Experimental asynchronous active modules.

   Predicates are defined in an (explicit) continuation-passing style
   (see bug entries for limitations). The continuation (resolvent) is
   a list of goals (defined in the current or other active modules).

   The execution is driven by the master active module (see
   @pred{async_call/2}).

   Each active module has direct access to its own data. Some data is
   shared across queries (see @lib{actmod_transient_state}).

   The current implementation of the master active module depends on:
   @begin{itemize}
   @item @lib{ciao-actmod.js}: JavaScript interface to Ciao active
     modules, and driver for a client-side master active module
   @item @lib{actmod_http.pl}: bridge for HTTP protocol and active module calls
   @item @apl{ciao-serve}: HTTP server with dynamic loading of active modules
   @end{itemize}

   @begin{alert}
   @begin{itemize}
   @item This is an experimental library
   @item Server side continuations must be the last in the list;
   client side does not support async yet (but it would be trivial to
   fix)
   @end{itemize}
   @end{alert}
").

:- doc(bug, "use html2terms/2 and string(_) to pass HTML as strings in json terms?").

:- doc(bug, "A more high-level interface may be much better (using
   Ciao concurrency primitives and a communication thread, or a
   package to properly implement continuation-passing style).").

:- use_module(library(lists), [length/2]).
:- use_module(library(pillow/json)).

% ---------------------------------------------------------------------------

:- include(.(actmod_async_hooks)).

% ---------------------------------------------------------------------------

% (nondet)
:- export(get_async_prop/2). 
% TODO: use stub_prop if prop is not defined; do it better?
get_async_prop(Action, Prop) :-
	( 'async.prop'(Action, Prop)
	; 'async.stub_prop'(Action, Prop)
	).

% ---------------------------------------------------------------------------
:- doc(section, "Encode/decode individual values").

encode_arg(json, X, X).
encode_arg(string, X, string(X)).
encode_arg(const, X, Y) :- atomic_to_json_str(X, Y). % TODO: remove?
encode_arg(num, X, Y) :- atomic_to_json_str(X, Y).
encode_arg(atm, X, Y) :- atomic_to_json_str(X, Y).
encode_arg(tagval_list, X, Y) :- js_tagval_list(X, Y).

% TODO: not here, declare data encoders
js_tagval_list([]) := [].
js_tagval_list([V=X|Vs]) := [Y|Ys] :-
	Y = ~js_tagval(V, X),
	js_tagval_list(Vs, Ys).

js_tagval(Tag, Value) := R :-
	R = json([tag = ~atomic_to_json_str(Tag),
	          value = Value]).

decode_arg(json, X, X).
decode_arg(string, string(X), X).
decode_arg(const, X, Y) :- json_as_atm(X, Y). % TODO: remove?
decode_arg(num, X, Y) :- json_as_num(X, Y).
decode_arg(atm, X, Y) :- json_as_atm(X, Y).
%decode_arg(tagval, X, Y) :- ...
%decode_arg(tagval_list, X, Y) :- ...

% ---------------------------------------------------------------------------
:- doc(section, "Encode/decode arguments and contextual data").
% (given FTypes)

decode_args([], _Args, _Data, _Blobs) --> [].
decode_args([FType|FTypes], Args, Data, Blobs) -->
	get_arg(FType, Args, Args2, Data, Blobs),
	decode_args(FTypes, Args2, Data, Blobs).

% (from data)
get_arg(data(Ty, Var), Args, Args, Data, _Blobs) --> !,
	{ json_get(Data, Var, Val0) -> try_decode_arg(Ty, Val0, Val) ; throw(no_data(Var)) },
	[Val].
get_arg(blob(Ty, Var), Args, Args, _Data, Blobs) --> !,
	{ json_get(Blobs, Var, Val0) -> try_decode_arg(Ty, Val0, Val) ; throw(no_data(Var)) },
	[Val].
% (from positional args)
get_arg(Ty, [X|Args], Args, _Data, _Blobs) --> !,
	{ try_decode_arg(Ty, X, Y) },
	[Y].

try_decode_arg(Ty, X, Y) :- 
	( decode_arg(Ty, X, Y0) -> Y = Y0
	; throw(cannot_decode(Ty))
	).

encode_args([], _, [], []).
encode_args([data(Ty,Var)|Tys], [X|Xs], Args, Data) :- !,
	encode_arg(Ty, X, Y),
	Data = [Var = Y|Data0],
	encode_args(Tys, Xs, Args, Data0).
encode_args([blob(Ty,Var)|Tys], [X|Xs], Args, Data) :- !,
	encode_arg(Ty, X, Y),
	Data = [Var = Y|Data0],
	encode_args(Tys, Xs, Args, Data0).
encode_args([Ty|Tys], [X|Xs], Args, Data) :- !,
	encode_arg(Ty, X, Y),
	Args = [Y|Args0],
	encode_args(Tys, Xs, Args0, Data).

% ---------------------------------------------------------------------------
:- doc(section, "Encode/decode reponses and requests"). % TODO: resolvent? residue?
% Notes on the current model:
%  - responses are lists of goals (resolvents) to be executed
%    on other actmods
%  - requests are goals to be executed in this actmod

:- export(encode_response/2).
% Translate from a list of goals (continuation, resolvent) into an async_response
% (encodes positional arguments and contextual data)
encode_response(Cont) := async_response(R) :-
	R = json([cont = ~encode_response_(Cont)]).

encode_response_([]) := [].
encode_response_([X|Xs]) := [R|Rs] :-
	R = ~encode_response_lit(X),
	Rs = ~encode_response_(Xs).

% Encode a goal into a JSON term, adding 'cmd', 'args' (positional
% arguments) and 'data' (contextual data)

% TODO: data need to be passed explicitly as arguments
%   (we may use 'transient' data but we must make sure that it is
%    retreived at the right place; be careful when continuations
%    contains more than one goal)

encode_response_lit(Query) := R :-
	functor(Query, N, A),
	functor(QueryTy, N, A),
	( 'async.ftypes'(QueryTy, FTypes) -> true
	; FTypes = []
	),
	Query =.. [Cmd|Args],
	% Encode positional arguments and contextual data
	encode_args(FTypes, Args, Args2, Data0),
	Data = json([args = Args2|Data0]),
	%
	functor(QueryD, N, A),
	( 'async.decl'(QueryD) -> % TODO: strange
	    % Command on Ciao active module
	    ( get_async_prop(Cmd, cached_step) -> RPCMethod = 'cached_step'
	    ; get_async_prop(Cmd, nosideff_step) -> RPCMethod = 'nosideff_step'
	    ; RPCMethod = 'normal_step'
	    ),
	    R = ~js_cmd_remote(Cmd, Data, RPCMethod)
	; % Command on a foreign JS active module
	  R = ~js_cmd(Cmd, Data)
	).

% Cmd with data Data (for local execution)
js_cmd(Cmd, Data) := R :-
	R = json([cmd = ~atomic_to_json_str(Cmd), data = Data]).

% Cmd with data Data (for remote execution)
js_cmd_remote(Cmd, Data, RPCMethod) := R :-
	R = json([cmd = ~atomic_to_json_str(Cmd), data = Data,
	          remote = ~atomic_to_json_str(RPCMethod)]).

:- export(decode_request/2).
% Translate from async_request to a goal for async.run
% (decodes positional arguments and contextual data)
% NOTE: see async.run note (arity differ)
decode_request(async_request(_ActMod, Cmd, Data, Blobs), Query2) :-
	( json_get(Data, 'args', Args) -> true ; Args = [] ), % positional args
	length(Args, N),
	functor(CmdD, Cmd, N), % take number of positional args as arity
	% Reconstruct goal
	( 'async.decl'(CmdD) -> true
	; throw(unknown_cmd(CmdD))
	),
	functor(CmdTy, Cmd, N), % take number of positional args as arity
	( 'async.ftypes'(CmdTy, FTypes) -> true
	; FTypes = []
	),
	decode_args(FTypes, Args, Data, Blobs, Args2, []),
	Query2 =.. [Cmd|Args2].

% ---------------------------------------------------------------------------

% TODO: currently 'async.run' must be deterministic! (get only one solution)

:- export(async_call/2).
% Treat one async request (decode + run + encode)
async_call(AsyncRequest, AsyncResponse) :-
	decode_request(AsyncRequest, Query2),
	%
	catch('async.run'(Query2, Cont),
	      X,
	      (display(uncaught_exception(X)),nl)), % TODO: Do not use display/1
	encode_response(Cont, AsyncResponse),
	!.

% ---------------------------------------------------------------------------
% Some custom responses

:- export(async_error/3).
async_error(not_ready, Msg) := async_response(json([
      'not_ready' = string(Msg)
    ])).

