:- package(xsyntax).
% Base translation for fsyntax and more

:- if(defined(optim_comp)).
% TODO: integrated, main incompatibilities and differences:
%   - missing fsyntaxplus extensions
%   - fun_return is not allowed
%   - fun_eval does not allow fun_eval syntax
%   - simpler translation for functional if-then-else (emit (_->_;_)
%     directly, because it is correctly optimized by later
%     optimizations)
:- else.
:- load_compilation_module(library(xsyntax/xsyntax_tr)).
:- add_sentence_trans(xsyntax_tr:defunc/3, 610).
:- add_goal_trans(xsyntax_tr:defunc_goal/2, 610).
:- endif.
