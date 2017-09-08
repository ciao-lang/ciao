:- module(http_service_rt, [], [assertions, basicmodes]).

:- use_module(library(lists), [append/3]).

% TODO: diferent URI? (not really CGIs) We could use /api/... /q/... etc. or nothing

:- export(service_path/2).
:- pred service_path(?Name, ?Path) # "@var{Path} is the URI of a Ciao HTTP service".

service_path(Name, Path) :-
	append("/cgi-bin/", Path0, Path),
	append(NameStr, ".cgi"||Rest, Path0),
	( Rest = "" ; Rest = "?"||_ ),
	!,
	atom_codes(Name, NameStr).
