% ===========================================================================
% :- doc(title, "Low level global variables.").

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
