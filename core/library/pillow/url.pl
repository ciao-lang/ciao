:- module(url, [
    url_term/1,
    url_info/2,
    url_info_relative/3],
   [assertions,isomodes,dcg]).

:- doc(title, "URL encoding/decoding").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module implements URL encoding/decoding predicates.").

:- use_module(library(lists), [append/3]).
:- use_module(library(pillow/pillow_aux), [digit/3, parse_integer/3, loupalpha/3]).

% ----------------------------------------------------------------------------

:- doc(url_term/1, "A term specifying an Internet Uniform Resource
   Locator. Currently only HTTP URLs are supported.  Example:
   @tt{http('www.clip.dia.fi.upm.es',80,\"/Software/Ciao/\")}.  Defined as
   @includedef{url_term/1}").
:- true prop url_term(URL) + regtype # "@var{URL} specifies a URL.".

url_term(http(Host,Port,Document)) :-
        atm(Host),
        int(Port),
        string(Document).

% ----------------------------------------------------------------------------

:- doc(url_info(URL,URLTerm), "Translates a URL @var{URL} to a
   Prolog structure @var{URLTerm} which details its various components,
   and vice-versa. For now non-HTTP URLs make the predicate fail.").

:- true pred url_info(+atm, ?url_term).
:- true pred url_info(+string, ?url_term).
:- true pred url_info(-string, +url_term).

url_info(Url, Info) :-
        atom(Url), !,
        atom_codes(Url, UrlStr),
        url_to_info(UrlStr, Info).
url_info(Url, Info) :-
        instantiated_string(Url), !,
        url_to_info(Url, Info).
url_info(Url, Info) :-
        info_to_url(Info, Url).

url_to_info(Url, http(Host,Port,Document)) :-
        http_url(Host, Port, Document, Url, []), !.
% More protocols may be added here...

http_url(Host,Port,Doc) -->
        "http://",
        internet_host(Host),
        optional_port(Port),
        http_document(Doc).

internet_host(Host) -->
        internet_host_char(C),
        internet_host_char_rest(Cs),
        {
            atom_codes(Host, [C|Cs])
        }.

internet_host_char_rest([C|Cs]) -->
        internet_host_char(C),
        internet_host_char_rest(Cs).
internet_host_char_rest([]) --> "".

internet_host_char(C) --> digit(C), !.
internet_host_char(C) --> loupalpha(C), !.
internet_host_char(0'-) --> "-".
internet_host_char(0'.) --> ".".

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

info_to_url(http(Host,Port,Document), Info) :- !,
        atom(Host),
        integer(Port),
        atom_codes(Host, HostS),
        port_codes(Port, PortS),
        mappend(["http://", HostS, PortS, Document], Info).
% More protocols may be added here...

port_codes(80, "") :- !.
port_codes(Port, [0':|PortS]) :-
        number_codes(Port, PortS).

% ---------------------------------------------------------------------------

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

:- true pred url_info_relative(+atm,+url_term,?url_term).
:- true pred url_info_relative(+string,+url_term,?url_term).

url_info_relative(URL, Base, Info) :-
        atom(URL), !,
        atom_codes(URL, URLStr),
        url_info_relative(URLStr, Base, Info).
url_info_relative(URL, _Base, Info) :-
        url_info(URL, Info), !.
url_info_relative(Path, http(Host,Port,_), http(Host,Port,Path)) :-
        Path = [0'/|_], !.
url_info_relative(File, http(Host,Port,BaseDoc), http(Host,Port,Document)) :-
        \+ member(0':, File), % Naive check to ensure it is not a valid URL
        append(BasePath, BaseFile, BaseDoc),
        \+ member(0'/, BaseFile), !,
        append(BasePath, File, Document).

% ---------------------------------------------------------------------------

% Concatenates a list of lists
mappend([], []).
mappend([S|Ss], R) :-
        append(S, R0, R),
        mappend(Ss, R0).
