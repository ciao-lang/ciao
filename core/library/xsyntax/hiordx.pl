:- package(hiordx).

% Enhanced hiord ({...} anonymous predicates)

:- use_package(hiord).
:- use_package(xsyntax/'_xsyntax').
:- use_package(xsyntax/'_xcontrol'). % (needed for shared-by-default)
:- fun_eval(hiord(true)).

% ---------------------------------------------------------------------------
% Clause block syntax

% Enable "{ Sent. Sent. ... }" syntax
:- set_prolog_flag(read_curly_blocks, on).

% TODO: preprocessing to merge variable by name in blocks, do it automatically?
:- load_compilation_module(library(xsyntax/blk_tr)).
:- add_sentence_trans(blk_tr:sentence_tr/4,250).

% TODO: alternative syntax? "{ Sent | Sent | ... }"
% :- op(1190, fx,[':-']).
% :- op(1190, xfx,[':-']).
% :- op(1195, xfy,['|']).

