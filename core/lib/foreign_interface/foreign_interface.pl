:- package(foreign_interface).
:- use_package([assertions,basicmodes,regtypes]).

:- if(defined(optim_comp)).
:- '$pragma'(treat_assertions).
:- '$pragma'(gluecode_options).
:- '$native_weak_inline'(include('ciao/ciao_gluecode.h')).
:- endif.

:- use_module(library(foreign_interface/foreign_interface_properties)).

% TODO: optim_comp does not require 'on' here, fix core?

:- new_declaration(use_foreign_source/1, on).

:- new_declaration(use_foreign_gluecode_header/1, on).

:- new_declaration(use_foreign_library/1, on).
:- new_declaration(use_foreign_library/2, on).

:- new_declaration(use_compiler/1, on).
:- new_declaration(use_compiler/2, on).

:- new_declaration(extra_compiler_opts/1, on).
:- new_declaration(extra_compiler_opts/2, on).

:- new_declaration(extra_linker_opts/1, on).
:- new_declaration(extra_linker_opts/2, on).

:- new_declaration(use_linker/1, on).
:- new_declaration(use_linker/2, on).

:- new_declaration(ttr_def/2, on).
:- new_declaration(ttr_match/2, on).

:- load_compilation_module(library(foreign_interface/foreign_interface_tr)).
:- add_sentence_trans(foreign_interface_tr:foreign_interface_tr/3, 1020).

:- use_package(engine(foreign_types)).
