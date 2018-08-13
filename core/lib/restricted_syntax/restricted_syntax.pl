:- package(restricted_syntax).

% This package implements a restricted syntax mode for module/2, where
% most Ciao extensions are disabled, specially the use of
% packages. Use module/3 with the 'classic' package if your module
% requires extending 'classic'.

:- load_compilation_module(library(restricted_syntax/restricted_syntax_tr)).
:- add_sentence_trans(restricted_syntax_tr:tr_sentence/3, 200).
