:- module(itf_base_db, [],[assertions, datafacts]).

% TODO: no doc

:- export(defines/3).
:- data defines/3.
:- export(imports/4).
:- data imports/4.
:- export(exports/2).
:- data exports/2.
:- export(multifile/2).
:- data multifile/2.
:- export(meta/2).
:- data meta/2.
:- export(dynamic/1).
:- data dynamic/1.
%
:- export(curr_module/1).
:- data curr_module/1.
:- export(curr_file/2).
:- data curr_file/2.
:- export(impl_defines/2).
:- data impl_defines/2.
:- export(defines_module/2).
:- data defines_module/2.
:- export(defines_module_rev_idx/2).
:- data defines_module_rev_idx/2. % reverse index (IG)
