:- module(http_service_rt, [], [assertions, basicmodes]).

:- use_module(library(lists), [append/3]).

% TODO: customize? see rpc_url_prefix in ciao-actmod.js

:- export(service_path/2).
:- pred service_path(?Name, ?URI) # "@var{URI} (without query string
   @tt{?...}) of a Ciao HTTP service with name @var{Name}.".

service_path(Name, URI) :-
	var(Name),
	!,
	service_name_(URI, NameStr),
	atom_codes(Name, NameStr).
service_path(Name, URI) :-
	atom_codes(Name, NameStr),
	service_name_(URI, NameStr).

% service_name_(URI, NameStr) :-
% 	URI = "/cgi-bin/"||NameStr0,
% 	append(NameStr, ".cgi", NameStr0),
% 	!.
service_name_(URI, NameStr) :-
	URI = "/"||NameStr,
	\+ member(0'/, NameStr).

:- export(split_query_str/3).
:- pred split_query_str(?Path, ?URI, ?QueryStr) # "Split the URI
   @var{Path} into @var{URI} and the query string @var{QueryStr} (if
   it has the form @tt{uri?query}) or return an empty query string (if
   it does not contain a query string)".

split_query_str(Path, URI, QueryStr0) :-
	append(URI0, "?"||QueryStr, Path),
	!,
	URI = URI0, QueryStr = QueryStr0.
split_query_str(Path, Path, "").
