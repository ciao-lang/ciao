:- package(regexp).
:- op(200, fy, (=~)).   % Like unary '+'

:- load_compilation_module(library(regexp/regexp_trans)).
:- add_sentence_trans(regexp_trans:pattern_unification/2, 640).

:- use_module(library(regexp/regexp_code)).

