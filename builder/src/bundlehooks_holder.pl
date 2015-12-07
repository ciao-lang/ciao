:- module(bundlehooks_holder,
	[do_use_module/1,
	 do_unload/1],
	[]).

% Use this module to load bundlehook modules.
%
% This module isolates the effects of meta-programming to just one
% module, without interfering with the rest of code.
%
% (similar to optim_comp's translation_module_holder)
%
% Author: Jose F. Morales

:- use_module(library(compiler), [use_module/1, unload/1]).
:- use_module(library(compiler), [make_po/1]).

do_use_module(A) :-
	use_module(A).
do_unload(A) :-
	unload(A).

