% RH: It seems to be a pretty bad name !
:- package(clpq_src).

:- use_module(library(clpq/eval_q)).
:- use_module(library(clpqr/arith_extra)).

:- load_compilation_module(library(clpq/expand_q)).
:- add_goal_trans(expand_q:expand/2, 750). % TODO: Right priority?

% (Experimental) A better way is the following --EMM:
% :- use_package(inliner).
% % :- inline_module(library(apply), [maplist/3]).
% :- use_module(library(clpq/eval_q), [arith_eval/2]).

% :- unfold arith_eps(yes).
% :- unfold arith_eval(yes).
% :- unfold arith_eval(yes, no).
% :- unfold ieee754_eps(yes).
% :- inline_module(library(clpq/eval_q),
% 	    [arith_eps/1, arith_zero/1, arith_eval/2, arith_eval/1]).
