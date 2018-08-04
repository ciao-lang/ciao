:- package(nortchecks).
:- use_package(assertions).

:- new_declaration(nortchecked/0).

:- nortchecked.

% A module that mark that run-time checks have been reported not to
% work with this code.

% TODO: That means that either the assertions here, or the code that
%   implements the run-time checks are erroneous. Except for a few
%   exceptions (rtchecks runtime itself) this is probably a bug.
