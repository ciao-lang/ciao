:- package(metatypes).

% note: priority like regtypes-2
:- load_compilation_module(library(metatypes/metatypes_tr)).
:- add_sentence_trans(metatypes_tr:expand_metatypes/2, 208). % TODO: Right priority?
