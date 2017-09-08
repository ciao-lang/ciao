:- use_package([assertions,doccomments]).
:- doc(nodoc,assertions).

%! \title HTTP client/server libraries
%  \author Ciao Development Team
%
%  \module
%
%    This is a collection of modules that implement a client and
%    server for the @concept{HTTP} protocol, and related technology
%    like @concept{CGI} programming.
% 
%    @bf{NOTE}: Parts of this code are based on the original PiLLoW
%    library @cite{pillow-ws-dist-short}), which can be downloaded from
%    @href{http://clip.dia.fi.upm.es/Software/pillow/pillow.html}.

:- doc(bug, "This code implemented HTTP/1.0 (RFC 1945)
   originally. Complete support for HTTP/1.1").

