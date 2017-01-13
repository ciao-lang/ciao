:- package(pp).
% Enable preprocessing
% TODO: this implementation is a hack
:- load_compilation_module(library(assertions/pp/pp_tr)).
:- add_sentence_trans(pp_tr:pp_sent/3, 170). % (just after condcomp, doccomments)
