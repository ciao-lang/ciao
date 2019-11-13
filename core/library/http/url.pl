:- module(url, [], [assertions,isomodes,regtypes,dcg,doccomments]).

%! \title  URL encoding/decoding
%  \author The Ciao Development Team
%
%  \module
%  This module implements URL encoding/decoding predicates.

:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(http/http_grammar), [digit/3, parse_integer/3, loalpha/3, upalpha/3]).

% ----------------------------------------------------------------------------

:- export(url_term/1).
:- doc(url_term/1, "A term specifying an Internet Uniform Resource
   Locator. Currently only HTTP URLs are supported.  Example:
   @tt{http('ciao-lang.org',80,\"/ciao/\")}.  Defined as
   @includedef{url_term/1}").
:- regtype url_term(URL) # "@var{URL} specifies a URL.".

url_term(http(Hostname,Port,URIStr)) :-
    atm(Hostname),
    int(Port),
    string(URIStr).

% ----------------------------------------------------------------------------

:- export(url_info/2).
:- doc(url_info(URL,URLTerm), "Translates a URL @var{URL} to a
   Prolog structure @var{URLTerm} which details its various components,
   and vice-versa. For now non-HTTP URLs make the predicate fail.").

:- pred url_info(+atm, ?url_term).
:- pred url_info(+string, ?url_term).
:- pred url_info(-string, +url_term).

url_info(Url, Info) :-
    atom(Url), !,
    atom_codes(Url, UrlStr),
    url_to_info(UrlStr, Info).
url_info(Url, Info) :-
    instantiated_string(Url), !,
    url_to_info(Url, Info).
url_info(Url, Info) :-
    info_to_url(Info, Url).

url_to_info(Url, http(Hostname,Port,URIStr)) :-
    http_url(Hostname, Port, URIStr, Url, []), !.
% More protocols may be added here...

% TODO: Add 'https' at least
http_url(Hostname,Port,Doc) -->
    "http://",
    internet_host(Hostname),
    optional_port(Port),
    http_document(Doc).

internet_host(Hostname) -->
    internet_host_char(C),
    internet_host_char_rest(Cs),
    {
        atom_codes(Hostname, [C|Cs])
    }.

internet_host_char_rest([C|Cs]) -->
    internet_host_char(C),
    internet_host_char_rest(Cs).
internet_host_char_rest([]) --> "".

internet_host_char(C) --> digit(C), !.
internet_host_char(C) --> loupalpha(C), !.
internet_host_char(0'-) --> "-".
internet_host_char(0'.) --> ".".

loupalpha(C) --> loalpha(C), !.
loupalpha(C) --> upalpha(CU), { C is CU+0'a-0'A }.

optional_port(Port) -->
    ":", !,
    parse_integer(Port).
optional_port(80) --> "".

http_document([0'/|Doc]) -->
    "/", !,
    rest(Doc).
http_document("/") --> "".

rest(S, S, []).

instantiated_string(S) :- var(S), !, fail.
instantiated_string([]).
instantiated_string([C|Cs]) :-
    integer(C),
    instantiated_string(Cs).

info_to_url(http(Hostname,Port,URIStr), Info) :- !,
    atom(Hostname),
    integer(Port),
    atom_codes(Hostname, HostnameS),
    port_codes(Port, PortS),
    mappend(["http://", HostnameS, PortS, URIStr], Info).
% More protocols may be added here...

port_codes(80, "") :- !.
port_codes(Port, [0':|PortS]) :-
    number_codes(Port, PortS).

% ---------------------------------------------------------------------------

:- export(url_info_relative/3).
:- doc(url_info_relative(URL,BaseURLTerm,URLTerm), "Translates a
   relative URL @var{URL} which appears in the HTML page refered to by
   @var{BaseURLTerm} into @var{URLTerm}, a Prolog structure containing its
   absolute parameters. Absolute URLs are translated as with
   @pred{url_info/2}.  E.g.
@begin{verbatim}
url_info_relative(\"dadu.html\",
              http('www.foo.com',80,\"/bar/scoob.html\"), Info)
@end{verbatim}
   gives @tt{Info = http('www.foo.com',80,\"/bar/dadu.html\")}.").

:- pred url_info_relative(+atm,+url_term,?url_term).
:- pred url_info_relative(+string,+url_term,?url_term).

url_info_relative(URL, Base, Info) :-
    atom(URL), !,
    atom_codes(URL, URLStr),
    url_info_relative(URLStr, Base, Info).
url_info_relative(URL, _Base, Info) :-
    url_info(URL, Info), !.
url_info_relative(Path, http(Hostname,Port,_), http(Hostname,Port,Path)) :-
    Path = [0'/|_], !.
url_info_relative(File, http(Hostname,Port,BaseDoc), http(Hostname,Port,URIStr)) :-
    \+ member(0':, File), % Naive check to ensure it is not a valid URL
    append(BasePath, BaseFile, BaseDoc),
    \+ member(0'/, BaseFile), !,
    append(BasePath, File, URIStr).

% ---------------------------------------------------------------------------

% Concatenates a list of lists
mappend([], []).
mappend([S|Ss], R) :-
    append(S, R0, R),
    mappend(Ss, R0).
