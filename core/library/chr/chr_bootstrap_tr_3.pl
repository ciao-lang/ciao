:- module(_ ,[ chr_compile_module/3 ], ['chr/chr_bootstrap_2b']).

:- chr_compiler_message("Doing CHR bootstrapping phase 3.").
:- include(library(chr/chr_common_tr)).
:- use_module(library(chr/guard_entailment_3)).
:- include(library(chr/chr_translate)).
:- include(chr_compiler_options).
