:- package(foreign_interface).
:- use_package([assertions,basicmodes,regtypes]).

:- use_module(library(foreign_interface/foreign_interface_properties)).

:- new_declaration(use_foreign_source/1,on).
:- new_declaration(use_foreign_source/2,on).

:- new_declaration(use_foreign_library/1,on).
:- new_declaration(use_foreign_library/2,on).

:- new_declaration(use_compiler/1, on).
:- new_declaration(use_compiler/2, on).

:- new_declaration(extra_compiler_opts/1,on).
:- new_declaration(extra_compiler_opts/2,on).

:- new_declaration(extra_linker_opts/1,on).
:- new_declaration(extra_linker_opts/2,on).

:- new_declaration(use_linker/1, on).
:- new_declaration(use_linker/2, on).

:- new_declaration(ttr_def/2,on).
:- new_declaration(ttr_match/2,on).

:- new_declaration(foreign_inline/1,on).
:- new_declaration(foreign_inline/2,on).

:- use_package(library(foreign_interface/foreign_interface_ttrs)).
:- use_package(library(foreign_interface/foreign_interface_ops)).

:- load_compilation_module(library(foreign_interface/foreign_inliner_tr)).
:- add_sentence_trans(foreign_inliner_tr:foreign_inliner_tr/3, 1010).

:- load_compilation_module(library(foreign_interface/foreign_interface_tr)).
:- add_sentence_trans(foreign_interface_tr:foreign_interface_tr/3, 1020).
