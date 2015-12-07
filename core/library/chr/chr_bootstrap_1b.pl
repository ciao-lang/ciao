:- package(chr_bootstrap_1b).

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.
:- load_compilation_module(library(chr/chr_bootstrap_tr_1b)).
:- add_sentence_trans(chr_bootstrap_tr_1b:chr_compile_module/3, 340).

:- include(library(chr/chr_pkg_common)).


