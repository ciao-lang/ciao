:- package(nortchecks).
:- use_package(assertions).

:- new_declaration(nortchecked/0).

:- nortchecked.

% Author: Edison Mera 
% 
% Jose Morales: I changed the message a bit so that it looks nicer in
% the documentation

:- doc(bug, "Run-time checks have been reported not to work with
   this code. That means that either the assertions here, or the code
   that implements the run-time checks are erroneous.").
