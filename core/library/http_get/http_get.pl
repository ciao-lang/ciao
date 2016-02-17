:- module(http_get, [http_get/2], [assertions]).

:- doc(title, "Simple HTTP Download").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module provides a simple interface for
   downloading files from the Web. It supports HTTP, HTTPS, and FTP
   protocols.

   As backend the module uses the command line tool @tt{wget} or
   @tt{curl}, depending on the one currently available on the system.
   Proxy are supported through the environment variables,
   @tt{http_proxy}, @tt{https_proxy}, @tt{ftp_proxy}, and
   @tt{no_proxy}. Refer to the manual page of @tt{wget} or @tt{curl}
   for more information about how setting those variables.").

:- use_module(library(process)).
:- use_module(library(process/process_channel)).

:- pred http_get(URL, OutputChanel) : (atom(URL), process_channel(Output))  #
     "downloads form the internet the file at the address @var{URL} and
      output it in the channel @var{Output}.".

http_get(URL, Output):-
	detect_command(wget), !, wget(URL, Output).
http_get(URL, Output):-
	detect_command(curl), !, curl(URL, Output).
http_get(_, _) :- 
	throw(error(http_get/2, neither_wget_nor_curl_found)).

% Silently fail if the command cannot be found in the path or does not
% answer successfully to '--version' option.
detect_command(Cmd):-
	catch(detect_command_(Cmd), _E, fail).

detect_command_(Cmd):-
	process_call(path(Cmd), ['--version'], [stdout(null), stderr(null), status(0)]).

wget(URL, Output):-
	process_call(path(wget), ['-q', URL, '-O', '-'], [stdout(Output)]).

curl(URL, Output):-
	% '-L' is needed to follow HTTP redirects
	process_call(path(curl), ['-s', '-L', URL], [stdout(Output)]).

