% (included)

% :- trait httpserv.

% 'httpserv.handle'(Path, Request, Response)
%   (if fail, it uses 'httpserv.file_path'/2)
:- discontiguous 'httpserv.handle'/3.
:- multifile 'httpserv.handle'/3.

% 'httpserv.file_path'(Dir, LocalDir):
%   URI files under Dir are accessible from LocalDir in the local filesystem
:- discontiguous 'httpserv.file_path'/2.
:- multifile 'httpserv.file_path'/2.

