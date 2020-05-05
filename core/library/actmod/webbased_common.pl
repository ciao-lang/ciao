:- module(webbased_common, [], []).

:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(http/http_client)).

% ---------------------------------------------------------------------------

% TODO: define proper addresses!
server_public_address(
  % URL
  http('www.clip.dia.fi.upm.es',80, "/actmod_servers/webbased_server.addr"),
  % Site path (on the filesystem)
  '/home/clip/public_html/actmod_servers/webbased_server.addr'
).

:- export(common_url/1).
% URL of script which knows the address of the server
common_url(URL):- server_public_address(URL,_).

:- export(common_path/1).
% PATH for the above script
common_path(PATH):- server_public_address(_,PATH).

% ---------------------------------------------------------------------------

:- export(fetch_server_addr/2).
fetch_server_addr(URL, NSAddr) :-
    ( fetch_url(URL,[],Response) -> true
    ; Response = []
    ),
    ( member(content(Bytes),Response) -> true
    ; Bytes = Response
    ),
    String=Bytes, % TODO: use string_bytes(String, Bytes),
    ( append(String0,[_],String) -> true
    ; String0 = String
    ),
    ( read_from_string_atmvars(String0,server(NSAddr,_)) ->
        true
    ; name(NSAddr,String0),
      throw(wrong_webserver_remote_address(NSAddr))
    ).
