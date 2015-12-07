:- package(det_hook).
:- use_module(library(det_hook/det_hook_rt)).

% note: after persdb_mysql, etc.
:- load_compilation_module(library(det_hook/det_hook_tr)).
:- add_sentence_trans(det_hook_tr:det_hook_trans/2, 1150).
