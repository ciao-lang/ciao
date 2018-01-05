:- module(actmod_http, [], [assertions, fsyntax, hiord]).

:- doc(title, "HTTP interface to (async) active modules").

:- doc(module, "This module implements a encoding/decoding of Ciao
   (async) active modules (see @lib{actmod_async}) messages as HTTP
   messages. See @lib{serve_http}.

   Current client libraries:
   @begin{itemize}
   @item @lib{ciao-actmod.js}: interact with active modules from JavaScript
   @end{itemize}
").

:- doc(bug, "Document the protocol").
:- doc(bug, "Implement a faster websocket interface").

% ---------------------------------------------------------------------------
:- doc(section, "HTTP bridge (for serve_http)").

% TODO: allow non-deterministic code in async requests
% TODO: missing timeout
:- use_module(library(system), [system_error_report/1]).
:- use_module(library(actmod_async/actmod_dyn)).
:- use_module(library(actmod_async/actmod_async_rt)).

:- multifile http_bridge/5.
% http_bridge(Protocol, ServName, Request, Response, Status)

http_bridge(actmod, ServName, Request, Response, Status) :- !,
	% bridge between HTTP and async handler
	async_request_from_http(ServName, Request, AsyncRequest),
	try_async_call(ServName, AsyncRequest, AsyncResponse, Status),
	async_response_to_http(AsyncResponse, Response).

% Call the active module (and (re)start it if needed)
% (it supports in-process active modules)
try_async_call(ActMod, AsyncRequest, AsyncResponse, Status) :-
	% TODO: distinguish failure from error
	catch(actmod_call(ActMod, async_call(AsyncRequest, AsyncResponse0)),
	      _,
	      fail),
	!,
	Status = ok,
	AsyncResponse = AsyncResponse0.
try_async_call(_ActMod, _AsyncRequest, AsyncResponse, Status) :-
	% Error during call.
	% We send the 'not ready' message and mark Status as 'need_restart'
	% We not try the async call immediately again since the daemon
	% may need some time to start.
	Status = need_restart,
	not_ready_response(AsyncResponse).

not_ready_response(AsyncResponse) :-
        system_error_report(ErrorMsg0),
	atom_codes(ErrorMsg0, ErrorMsg),
	AsyncResponse = ~async_error(not_ready, ErrorMsg).

% ---------------------------------------------------------------------------
:- doc(section, "From HTTP request to async request").

:- use_module(library(http/http_forms)).
:- use_module(library(pillow/json), [string_to_json/2]).

% TODO: optimize all encoding/decoding part!
% TODO: add blob1,etc. directly from Javascript?

% :- export(async_request_from_http/3).
async_request_from_http(ServName, Request, AsyncRequest) :-
	http_parse_form(Request, Input),
	form_data_to_async_request(ServName, Input, AsyncRequest).

% Obtain async query request from http form data
% (specialized for actmod_http)
form_data_to_async_request(ServName, Input, async_request(ActMod, Cmd, Data, Blobs)) :-
	% display(user_error, i(Input)), nl(user_error),
	( get_form_value_atm(Input, 'actmod', ActMod) -> true ; ActMod = ServName ), % TODO: ServName as ActMod is a good idea?
	( get_form_value_atm(Input, 'cmd', Cmd) -> true ; Cmd = '' ),
	( get_form_value_json(Input, 'data', Data) -> true ; Data = json([]) ),
	( member(blob1=file(Filename, InputPrg), Input) ->
	    Blobs = json([blob1_filename=string(~atom_codes(Filename)),
	                  blob1=string(InputPrg)])
	; get_form_value_string(Input, blob1, Blob1) ->
	    Blobs = json([blob1=string(Blob1)])
	; Blobs = json([])
	).

get_form_value_json(Input, Name, Value) :-
	get_form_value_string(Input, Name, Value0),
	string_to_json(Value0, Value).

% ---------------------------------------------------------------------------
:- doc(section, "From async response to HTTP response").

:- use_module(library(pillow/json), [json_to_string/2]).

% :- export(async_response_to_http/2).
async_response_to_http(AsyncResponse, Response) :-
	async_response_to_json(AsyncResponse, Output),
	json_to_string(Output, Str),
	Response = json_string(Str).

async_response_to_json(async_response(X), X).

