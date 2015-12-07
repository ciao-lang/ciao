:- module(dirs, []).
:- use_package([]).
:- multifile library_directory/1.
:- dynamic library_directory/1.
library_directory('.').
