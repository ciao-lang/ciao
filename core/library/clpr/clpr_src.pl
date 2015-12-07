% RH: It seems to be a pretty bad name !
:- package(clpr_src).

:- use_module(library(clpr/eval_r)).
:- use_module(library(clpqr/arith_extra)).

:- load_compilation_module(library(clpr/expand_r)). % May be expand_real or expand_rational
:- add_goal_trans(expand_r:expand/2, 750). % TODO: Right priority?

% (Experimental) A better way is the following --EMM:
% :- use_package(inliner).
% % :- inline_module(library(apply), [maplist/3]).
% :- use_module(library(clpr/eval_r), [arith_eval/2]).

% :- unfold arith_eps(yes).
% :- unfold arith_eval(yes).
% :- unfold arith_eval(yes, no).
% :- unfold ieee754_eps(yes).
% :- inline_module(library(clpr/eval_r),
% 	    [arith_eps/1, arith_zero/1, arith_eval/2, arith_eval/1]).
