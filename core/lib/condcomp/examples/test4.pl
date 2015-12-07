% Tests for package priorities.
% Jose F. Morales

:- module(_, [main/0], [argnames, condcomp]).

% Without package priorities, compilation stops with this message:
%   ERROR (argnames_trans): (lns 7-9) incompatible argnames declaration foo(a,b,d)

:- argnames foo(a,b,c).
:- if(fail).
% If condcomp is executed before argnames expansion, this erroneous
% clause will not be ever seen.
:- argnames foo(a,b,d).
:- endif.

main.
