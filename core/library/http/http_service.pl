:- package(http_service). % TODO: :- trait http_service.

% Trait for HTTP service (based on library(http/http_serve))
% 
% For compatibility, this can be used as a CGI when compiled as an
% executable (main/1 entry point).

% Requires:
%   :- pred service_name(Name)
%   :- pred treat_request(Request, Response)

% (interface as CGI process)

:- use_module(library(http/cgi), [
	cgi_read_request/1,
	cgi_write_response/1
]).

:- export(main/1).
main(_) :-
	cgi_read_request(Request),
	treat_request(Request, Response),
	cgi_write_response(Response).

% (interface as HTTP server)

:- use_module(library(http/http_service_rt)).

:- include(library(http/http_server_hooks)).

'httpserv.handle'(Path, Request, Response) :-
	split_query_str(Path, URI, _),
	service_name(Name),
	service_path(Name, URI),
	!,
	treat_request(Request, Response).
