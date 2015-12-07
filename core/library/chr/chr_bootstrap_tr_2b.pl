:- module(_ ,[ chr_compile_module/3 ], ['chr/chr_bootstrap_2a']).

:- chr_compiler_message("Doing CHR bootstrapping phase 2b.").
:- include(library(chr/chr_common_tr)).
:- include(library(chr/chr_translate_bootstrap_2)).
