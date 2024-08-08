% (included file)
%! # Low level global variables

% Currently, the global variables are reserved statically:
%
%   [optim_comp only]
%   1 - debugger
%   2 - compiler/frontend.pl ('compile' part)
%   4 - compiler/frontend.pl ('split' part)
%   3 - Errs object during analysis
%   5 - unused, see compiler/dynload.pl
%   6 - absmach
%
%   [all Ciao]
%   7 - exceptions.pl (catch/throw)
%   8 - exceptions.pl (intercept/send_signal)
%   10 - CHR package (chr/hprolog.pl)
%   11 - global_vars module
% ---------------------------------------------------------------------------

% NOTE: It is possible to replace '$setarg'/4 by attributed varibles.
%   Remember to use update_attribute instead of deteach/attach
%   sequences, which creates arbitrarily long dereference chains
%   (identified by Remy & Jose).

% global variables based on setarg
:- export('$global_vars_set'/2).
'$global_vars_set'(I, X) :-
    '$global_vars_get_root'(R),
    '$setarg'(I, R, X, on).
:- export('$global_vars_get'/2).
'$global_vars_get'(I, X) :-
    '$global_vars_get_root'(R),
    arg(I, R, X).

:- if(defined(optim_comp)).
:- '$props'('$global_vars_get_root'/1, [impnat=cbool(prolog_global_vars_get_root)]).
:- else.
:- impl_defined('$global_vars_get_root'/1).
:- impl_defined('$global_vars_set_root'/1). % TODO: deprecate
:- endif.

% naive implementation of mutable variables based on setarg
% TODO: I could place mutable type info here
% :- export('$mutvar_init'/2).
% '$mutvar_init'(Val, X) :-
%   X = '$mut'(Val).
% :- export('$mutvar_assign'/2).
% '$mutvar_assign'(X, Val) :- nonvar(X), X = '$mut'(_), 
%   '$setarg'(1, X, Val, on).
% :- export('$mutvar_get'/2).
% '$mutvar_get'(X, Val) :- nonvar(X), X = '$mut'(Val).

