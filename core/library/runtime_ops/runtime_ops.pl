:- package(runtime_ops).

:- if(defined('SHELL')).
% (nothing, op/3 is treated by the toplevel)
:- else.
% Not in a toplevel

:- use_module(library(operators), [op/3]).

:- load_compilation_module(library(runtime_ops/runtime_ops_tr)).
:- add_sentence_trans(runtime_ops_tr:runtime_op/2, 210).

:- endif. % defined('SHELL')