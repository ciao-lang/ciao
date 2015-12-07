:- use_module(engine(hiord_rt)).

%% These are the modes used in the commons library. They are similar to the ones used in the ISO standard 
%% and in (ref to Jan et als paper) 

:- op(200, fy, [(?),(@),(:)]).

%% Basic ISO-modes
:- modedef '+'(A) : nonvar(A).
:- modedef '-'(A) : var(A). 
%% The standard says that this should be:
% :- modedef '-'(A) : var(A) => nonvar(A).
%% but then it says that the only error possible is for not 
%% meeting the : var... what to do?
:- modedef '?'(_).
:- modedef '@'(A) + not_further_inst(A).
%% Only in older versions of standard? It is obsolete now.
%% :- modedef '*'(_).
%% Meta arg. Needs to be defined further?
:- modedef ':'(A) : nonvar(A).


%% Parametric versions of above
:- push_prolog_flag(read_hiord,on).
:- modedef +(A,X) :  X(A).
:- modedef -(A,X) :  var(A) => X(A).
%% Version in standard supports this simple interpretation:
% :- modedef ?(A,X) :: X(A).
%% but all builtins conform to:
:- modedef ?(A,X) :: X(A) => X(A).
%% ..what to do??
:- modedef @(A,X) :  X(A) => X(A) + not_further_inst(A).
%% Only in older versions of standard? It is obsolete now.
%% :- modedef *(A,X) :: X(A).
%% Meta arg. Needs to be defined further?
:- modedef ':'(A,X) : X(A).
:- pop_prolog_flag(read_hiord).
