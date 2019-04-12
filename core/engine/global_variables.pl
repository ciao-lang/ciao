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
%   [rest of Ciao]
%   10 - CHR package (chr/hprolog.pl)
%   11 - global_vars module
% ---------------------------------------------------------------------------

% global variables based on setarg
:- export('$global_vars_set'/2).
'$global_vars_set'(I, X) :-
	'$global_vars_get_root'(R),
	'$setarg'(I, R, X, on).
:- export('$global_vars_get'/2).
'$global_vars_get'(I, X) :-
	'$global_vars_get_root'(R),
	arg(I, R, X).

% TODO: Bad implementation! This creates long dereference chains
%       Use update_attribute instead of deteach/attach sequences
%       (detected in the main branch by Remy & Jose)
%
% TODO: it may worth implementing the new attributes
%
% % global variables based on cva variables
% :- use_module(engine(attributes)).
% :- use_module(engine(term_typing)).
% :- export('$global_vars_set'/2).
% '$global_vars_set'(I, X) :-
% 	'$global_vars_get_root'(R),
% 	arg(I, R, V),
% 	( type(V, attv) ->
% 	    detach_attribute(V)
% 	; true
% 	),
% 	attach_attribute(V, v(X)).
% :- export('$global_vars_get'/2).
% '$global_vars_get'(I, X) :-
% 	'$global_vars_get_root'(R),
% 	arg(I, R, V),
% 	( type(V, attv) ->
% 	    get_attribute(V, v(X))
% 	; attach_attribute(V, v(X))
% 	).

% NOTE: This has to be done before any choicepoint is created
'$global_vars_init' :-
	% % cva version
	% % F = '$glb'(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
	% %            _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), % 32 global vars
	% setarg version	
	F = '$glb'(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), % 32 global vars
	'$global_vars_set_root'(F).

