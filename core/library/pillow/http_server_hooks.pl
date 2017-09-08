% (included)

% http_handle(Path, Response)
%   (if fail, it uses http_file_path/2)
:- discontiguous http_handle/2.
:- multifile http_handle/2.

% http_file_path(Dir, LocalDir):
%   URI files under Dir are accessible from LocalDir in the local filesystem
:- discontiguous http_file_path/2.
:- multifile http_file_path/2.

