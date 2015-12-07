:- module(_ ,[ chr_compile_module/3 ], ['chr/chr_bootstrap_0']).

:- chr_compiler_message("Doing CHR bootstrapping phase 1a.").
:- include(library(chr/chr_common_tr)).
:- include(library(chr/chr_translate_bootstrap_1)).
