:- package(fsyntax).
:- include(library(fsyntax/ops)).
:- if(defined(optim_comp)).
:- '$pragma'(functional_expand).
% TODO: integrated fsyntax, main incompatibilities and differences:
%   - fun_return is not allowed
%   - fun_eval does not allow fun_eval syntax
%   - simpler translation for functional if-then-else (emit (_->_;_)
%     directly, because it is correctly optimized by later
%     optimizations)
:- else.
:- load_compilation_module(library(fsyntax/functionstr)).
:- add_sentence_trans(functionstr:defunc/3, 610).
:- add_goal_trans(functionstr:defunc_goal/2, 610).
:- endif.

