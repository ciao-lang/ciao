:- module(actmod_http, [], [assertions, fsyntax, hiord, dcg]).

:- doc(title, "Active modules over HTTP").

:- doc(stability, devel).
:- doc(module, "
   This module offers support for active modules over HTTP (for static
   HTML generation or interaction with JS code). It implements the
   encoding/decoding of messages as JSON (for requests and responses,
   including suspensions of active module instances) or HTML.

   Responses may include residues that can be partially executed on
   each node, producing both an answer and a residue (pending
   resolvent) composed of goals whose execution is delegated to the
   node of the caller active module. Suspensions may include
   fiber-local data (@lib{fibers_data}).

   See also:
   @begin{itemize}
   @item @lib{ciao-actmod.js}: JavaScript interface to Ciao active
     modules, and driver for a client-side master active module
   @item @lib{actmod_http.pl}: bridge for HTTP protocol and active module calls
   @item @lib{serve_http} and @apl{ciao-serve}: HTTP server with
     dynamic loading of active modules
   @end{itemize}
").

:- doc(bug, "use html2terms/2 and string(_) to pass HTML as strings in json terms?").
:- doc(bug, "client side continuations does not support fiberSusp yet (but it would be trivial to fix)").

:- doc(bug, "A more high-level interface may be much better (using
   Ciao concurrency primitives and a communication thread, or a
   package to properly implement continuation-passing style).").

:- use_module(library(http/http_forms)).
:- use_module(library(http/http_service_rt), [service_path/2]).
:- use_module(library(pillow/json)).
:- use_module(library(lists), [member/2, append/3, length/2]).
:- use_module(engine(messages_basic), [message/2]).

% ---------------------------------------------------------------------------
:- doc(section, "From HTTP request to actmod request").

% TODO: This is slowing polluting the atom table; try more efficient
%   serialization; add atom-GC.

% TODO: optimize all encoding/decoding part!
% TODO: add blob1,etc. directly from Javascript?

:- import(actmod_rt, [mod_concat_head/3]). % TODO: export or merge

:- export(actRequest_from_http/3).
actRequest_from_http(ServName, Request, ActRequest) :-
	http_parse_form(Request, Input),
	'form_data->fiberSusp'(ServName, Input, ActRequest).

% From HTTP form data to $fiberSusp
% (specialized for actmod_http)
'form_data->fiberSusp'(ServName, Input, ActRequest) :-
	DMod = ~form_data_dmod(ServName, Input),
	State = ~form_data_state(Input),
	G = ~form_data_goal(DMod, Input),
	ActRequest = '$fiberSusp'(fiberData(DMod,State), G).

form_data_dmod(ServName, Input) := DMod :-
	( get_form_value_atm(Input, 'actmod', DMod), \+ DMod = '' -> true
	; DMod = ServName % TODO: ServName as DMod; is it a good idea?
	).

form_data_state(Input) := State :-
	( member(s=State0, Input) ->
	    atom_to_term(State0, State)
	; State = []
	).

form_data_goal(_DMod, Input) := G :-
	get_form_value_atm(Input, 'cmd', Cmd),
	\+ Cmd = '',
	!,
	( get_form_value_json(Input, 'data', Data) -> true ; Data = json([]) ),
	( member(blob1=file(Filename, InputPrg), Input) ->
	    Blobs = json([blob1_filename=string(~atom_codes(Filename)),
	                  blob1=string(InputPrg)])
	; get_form_value_string(Input, blob1, Blob1) ->
	    Blobs = json([blob1=string(Blob1)])
	; Blobs = json([])
	),
	G = goal_as_json(Cmd, Data, Blobs).
form_data_goal(DMod, Input) := G :-
	( member(c=G0, Input) ->
	    atom_to_term(G0, G)
	; member(g=G0, Input) ->
	    % special case, encoded dumpHTML(_)
	    atom_to_term(G0, G1),
	    G = 'wui_html.dumpHTML'(G1)
	; % default, initialize
	  mod_concat_head(DMod, '__init__', G1),
	  G = 'wui_html.dumpHTML'(G1)
	).

get_form_value_json(Input, Name, Value) :-
	get_form_value_string(Input, Name, Value0),
	string_to_json(Value0, Value).

% ---------------------------------------------------------------------------
:- doc(section, "From actmod response to HTTP response").

:- use_module(library(pillow/json), [json_to_string/2]).
:- use_module(library(pillow/html), [html2terms/2]).

:- export(actResponse_to_http/2).
actResponse_to_http(actResponse_json(Output), Response) :- !,
	json_to_string(Output, Str),
	Response = json_string(Str).
actResponse_to_http(actResponse_html(Output), Response) :- !,
        html2terms(Str, Output),
	Response = html_string(Str).

% ---------------------------------------------------------------------------
:- doc(section, "From fiberSusp to HTTP query strings").

:- export('fiberSusp->query_str'/2).
% Query string (href) from $fiberSusp
% (see actmod_http:'form_data->fiberSusp'/3)
'fiberSusp->query_str'('$fiberSusp'(fiberData(DMod,State), G0), HRef) :-
	term_to_atom(State, Sa),
	( G0 = 'wui_html.dumpHTML'(G) ->
	    % (Special case)
	    GParam = (g= ~term_to_atom(G)) % TODO: avoid atoms!
	; GParam = (c= ~term_to_atom(G0)) % TODO: avoid atoms!
	),
	Params = [GParam, s=Sa],
	HRef = ~get_query_str(DMod, Params).

% TODO: avoid atoms!
% (see ciao-actmod.js)
get_query_str(DMod, Params, HRef) :-
	url_query_values(Str, Params),
	ServName = DMod, % TODO:T253 from DMod to service name; good idea?
	service_path(ServName, Path),
	append(Path, "?"||Str, HRef).

% ---------------------------------------------------------------------------

:- include(library(actmod/actmod_hooks)).
:- use_module(library(fibers/fibers_rt)).

% ---------------------------------------------------------------------------

:- impl(qprot, async_json).
%
(async_json as qprot).collect(_,_) :- fail. % (undefined)
(async_json as qprot).dec(ActRequest, Method2, Request2) :-
	Method2 = async,
	async_json_decode_request(ActRequest, Request2).
%
(async_json as qprot).enc(Response2, ActResponse) :-
	async_json_encode_response(Response2, ActResponse).
(async_json as qprot).prepare_query(_,_,_) :- fail. % (undefined)
(async_json as qprot).join_answers(_,_,_,_,_,_) :- fail. % (undefined)

% ---------------------------------------------------------------------------
:- doc(section, "Encode/decode individual values").

encode_arg(json, X, X).
encode_arg(string, X, string(X)).
encode_arg(const, X, Y) :- atomic_to_json_str(X, Y). % TODO: remove? 3->"3"->('3' or 3?) (similar for bigints)
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
decode_arg(const, X, Y) :- json_as_atm(X, Y). % TODO: remove? 3->"3"->('3' or 3?) (similar for bigints)
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

% TODO: do not distinguish; try Data then Blobs
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
:- doc(section, "Encode/decode responses and requests").

% Notes on the current model:
%  - responses are lists of goals (resolvents) to be executed
%    on other actmods
%  - requests are goals to be executed in this actmod

% TODO: related to '$fiber_susp'/2

%:- export(async_json_encode_response/2).
% Translate from a list of goals (continuation, resolvent) into an actResponse_json
% (encodes positional arguments and contextual data)
async_json_encode_response(Cont) := actResponse_html(Output) :- % TODO: ad-hoc case
	Cont = ['fibers_blt.yield_residue'(html(Output))], !.
async_json_encode_response(Cont) := actResponse_json(R) :-
	% TODO: missing t_data_dump! (in the cases where it is needed)
	R = json([cont = ~async_json_encode_response_(Cont)]).

async_json_encode_response_([]) := [].
async_json_encode_response_([X|Xs]) := [R|Rs] :-
	R = ~async_json_encode_response_lit(X),
	Rs = ~async_json_encode_response_(Xs).

% Encode a goal into a JSON term, adding 'cmd', 'args' (positional
% arguments) and 'data' (contextual data)

% TODO: data need to be passed explicitly as arguments
%   (we may use 'transient' data but we must make sure that it is
%    retreived at the right place; be careful when continuations
%    contains more than one goal)

async_json_encode_response_lit(Query) := R :-
	functor(Query, N, A),
	functor(QueryD, N, A), % TODO: is copy really needed?
	( fnct_mod(QueryD, _), fnct_property(QueryD, async) ->
	     get_async_ftypes(Query, FTypes),
	     Query =.. [Cmd|Args],
	     % Encode positional arguments and contextual data
	     encode_args(FTypes, Args, Args2, Data0),
	     Data = json([args = Args2|Data0]),
	     % 
	     ( fnct_stub_only(QueryD) ->
	         RPCMethod = none
	     ; % TODO: Introduce with_actref/2? (or similar)
	       ( fnct_property(QueryD, cached_step) -> RPCMethod = 'cached_step'
	       ; fnct_property(QueryD, nosideff_step) -> RPCMethod = 'nosideff_step'
	       ; RPCMethod = 'normal_step'
	       )
	     )
	; message(error, ['undefined active module (async) predicate ', ''(N/A)]),
          Cmd = fail, % TODO: encode exception, throw error
	  Data = json([args = []]),
	  RPCMethod = none 
	),
	R = ~js_cmd(Cmd, Data, RPCMethod).

% Cmd with data Data. RPCMethod is:
%  - none: for exec on browser
%  - other for exec on server
js_cmd(Cmd, Data, none) := R :- !,
	R = json([cmd = ~atomic_to_json_str(Cmd), data = Data]).
js_cmd(Cmd, Data, RPCMethod) := R :-
	R = json([cmd = ~atomic_to_json_str(Cmd), data = Data,
	          remote = ~atomic_to_json_str(RPCMethod)]).

% ---------------------------------------------------------------------------

% Decode a goal for async.run/1 (translates from goal_as_json/3 to goal)
async_json_decode_request('$fiberSusp'(FiberData, G0), '$fiberSusp'(FiberData, G)) :-
	async_json_decode_request_(G0, G).

% NOTE: see async.run/1 note (arity differ)
async_json_decode_request_(goal_as_json(Cmd, Data, Blobs), G2) :- !,
	( json_get(Data, 'args', Args) -> true ; Args = [] ), % positional args
	length(Args, N),
	functor(CmdD, Cmd, N), % take number of positional args as arity
	% Reconstruct goal
	( fnct_mod(CmdD, _), fnct_property(CmdD, async) -> true
	; throw(unknown_cmd(CmdD))
	),
	functor(CmdTy, Cmd, N), % take number of positional args as arity
	get_async_ftypes(CmdTy, FTypes),
	decode_args(FTypes, Args, Data, Blobs, Args2, []),
	G2 =.. [Cmd|Args2]. % (module qualified)
async_json_decode_request_(G, G).

% ---------------------------------------------------------------------------
% Some custom responses

:- export(async_json_error/3).
async_json_error(not_ready, Msg) := actResponse_json(json([
      'not_ready' = string(Msg)
    ])).

% ---------------------------------------------------------------------------

:- use_module(library(actmod/actmod_dist), [term_to_atom/2, atom_to_term/2]).

