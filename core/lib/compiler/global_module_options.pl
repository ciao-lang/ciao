:- module(global_module_options, [], [assertions, define_flag]).

:- doc(author, "Jose F. Morales"). % TODO: Merge with older optim_comp version
:- doc(author, "Edison Mera"). % (original version for Ciao, just for packages)

:- doc(title, "Global Module Options").

:- doc(module, "This module provides the compiler interface for the
   specification of global module options. These options can modify
   the compilation and translation phases of several modules without
   modifying the source code.

   This can be useful for tools that perform program transformations
   that do not change the semantics of modules but add certain
   functionality. Examples are dynamic monitors (such as the
   profiler), run-time assertion checkers, optimization packages that
   perform unfolding or specialization, etc.

   @bf{Limitations}: currently, the only valid options are a list of
   additional packages that are included for the specified
   modules. Since packages do not support @emph{parameters}, this is
   quite limited in some cases. We plan to extend the list of options
   with compiler flags, engine options, or definitions for conditional
   compilation.").

:- doc(bug, "Merge with options for the debugger (it was generalized
   in optim_comp)").

:- doc(bug, "Incremental compilation does not detect changes in global
   module options.  Fix: include them in the .itf files and compare
   them to check if some module needs to be recompiled.").

:- doc(bug, "Specify different naming patterns for .po and .itf files
   based on some options. This will avoid many recompilations when
   switching between two (or more) fixed sets of options.").

% ---------------------------------------------------------------------------
% Global module options

% (using define_flag package)
define_flag(use_global_module_options, [yes, no],  yes).

:- pred glbmod_use_package(Module, Package) :: atm * term
   # "Use package @var{Package} in module @var{Module}.".
:- data glbmod_use_package/2.

% ---------------------------------------------------------------------------

% TODO: The types in the assertions are wrong. Both Modules and
%   Packages can be free variables (JFMC).
% TODO: The intended meaning of free variables here is 'forall'. This
%   is strange (JFMC).

:- export(glbmod_add_package/2).
:- pred glbmod_add_package(Modules, Packages) ::
	nlist(atm) * nlist(sourcename)
   # "Use @var{Packages} (a nested list) implicitly in modules
      @var{Modules} (a nested list).".

glbmod_add_package(Module, Package) :-
	var(Module),
	!,
	glbmod_add_package_m(Package, Module).
glbmod_add_package([],               _) :- !.
glbmod_add_package([Module|Modules], Package) :-
	!,
	glbmod_add_package(Module,  Package),
	glbmod_add_package(Modules, Package).
glbmod_add_package(Module, Package) :-
	glbmod_add_package_m(Package, Module).

glbmod_add_package_m([],                 _) :- !.
glbmod_add_package_m([Package|Packages], Module) :-
	!,
	glbmod_add_package_m(Package,  Module),
	glbmod_add_package_m(Packages, Module).
glbmod_add_package_m(P, Module) :-
	glbmod_add_package_mp(Module, P).

glbmod_add_package_mp(Module, Package) :-
	glbmod_use_package(Module, Package),
	!.
glbmod_add_package_mp(Module, Package) :-
	assertz_fact(glbmod_use_package(Module, Package)).

% ---------------------------------------------------------------------------
% TODO: Not used

:- export(glbmod_del_package/2).
:- pred glbmod_del_package(Modules, Packages) ::
	nlist(atm) * nlist(sourcename)
   # "Remove @var{Packages} (a nested list) from the list of
      implicitly used packages for modules @var{Modules} (a nested
      list).".

glbmod_del_package(Module, Package) :-
	var(Module),
	!,
	glbmod_del_package_m(Package, Module).
glbmod_del_package([],               _) :- !.
glbmod_del_package([Module|Modules], Package) :-
	!,
	glbmod_del_package(Module,  Package),
	glbmod_del_package(Modules, Package).
glbmod_del_package(Module, Package) :-
	glbmod_del_package_m(Package, Module).

glbmod_del_package_m(Package, Module) :-
	var(Package),
	!,
	glbmod_del_package_mp(Module, Package).
glbmod_del_package_m([],                 _) :- !.
glbmod_del_package_m([Package|Packages], Module) :-
	!,
	glbmod_del_package_m(Package,  Module),
	glbmod_del_package_m(Packages, Module).
glbmod_del_package_m(Package, Module) :-
	glbmod_del_package_mp(Module, Package).

glbmod_del_package_mp(Module, Package) :-
	retractall_fact(glbmod_use_package(Module, Package)).

:- export(glbmod_reset/0).
:- pred glbmod_reset/0 # "Remove all global options for all modules".

glbmod_reset :-
	retractall_fact(glbmod_use_package(_, _)).

% ---------------------------------------------------------------------------
% TODO: Remove ad-hoc code for rtchecks, find a better solution. The
%   current one mixes runtime flags with compile-time flags (JFMC)

:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates), [findall/3]).

:- export(glbmod_collect_packages/3).
:- pred glbmod_collect_packages(Module, Packages0, Packages)
   # "Extend @var{Packages0} list to @var{Packages}, including the
      packages implied by the global module options for @var{Module}".

glbmod_collect_packages(Module, Packages0, Packages) :-
	% Make sure that Packages0 is a list
	( var(Packages0) ->
	    % TODO: Packages0 should not be 'var', catch error before?
	    Packages1 = []
	; flatten(Packages0, Packages1)
	),
	glbmod_collect_packages_(Module, Packages1, Packages).

glbmod_collect_packages_(Module, Packages0, Packages) :-
	% Add packages due to glbmod_use_package
	% TODO: Packages may be duplicated
	( current_prolog_flag(use_global_module_options, yes) ->
	    findall(Package, glbmod_use_package(Module, Package), GPackages),
	    append(Packages0, GPackages, Packages1)
	; Packages1 = Packages0
	),
	% Fix packages for rtchecks
	rtchecks_fix_packages(Packages1, Packages).

% ---------------------------------------------------------------------------
% Fix packages for rtchecks

% TODO: I fixed this code, but it is still a kludge: it only captures
%   rtchecks when it appears in the third argument of module/3
%   declaration.

% Extend the list of packages with rtchecks if necessary
rtchecks_fix_packages(Packages0, Packages) :-
	rtcheck_packages(Packages0, RTPackages),
	append(Packages0, RTPackages, Packages).

rtcheck_packages(Packages0, RTPackages) :-
	member(nortchecks, Packages0), % TODO: ugly (use a compile-time flag instead)
	!,
	RTPackages = [].
rtcheck_packages(Packages0, RTPackages) :-
	% No rtchecks without assertions
	\+ member(assertions, Packages0), % TODO: show warning?
	!,
	RTPackages = [].
rtcheck_packages(Packages0, RTPackages) :-
	( member(rtchecks, Packages0) ->
	    RTPackages = RTPackages0 % 'rtcheck' already in list
	; current_prolog_flag(runtime_checks, yes) ->
	    RTPackages = [rtchecks|RTPackages0] % add 'rtchecks' package
	),
	!,
	curr_rtcheck_package(Package),
	RTPackages0 = [Package].
rtcheck_packages(_Packages0, RTPackages) :-
	RTPackages = [].

% TODO: rtcheck_inline must also be a global_module_option, not a prolog flag
% Package for 'rtcheck' (based on value of rtcheck_inline prolog flag)
curr_rtcheck_package(Pkg) :-
	( current_prolog_flag(rtchecks_inline, yes) ->
	    Pkg = library(rtchecks/rtchecks_rt_inline)
	; Pkg = library(rtchecks/rtchecks_rt_library)
	).

% ---------------------------------------------------------------------------
% (from llists.pl, not included here to avoid one extra module in the compiler)
% TODO: Check if llists is already included in the compiler

flatten(Xs, Ys) :-
	flatten_dif(Xs, Ys, []).

flatten_dif(X, [X|Ys], Ys) :-
	var(X),
	!.
flatten_dif([], Xs, Xs) :-
	!.
flatten_dif([X|Xs], Ys, Zs) :-
	!,
	flatten_dif(X, Ys, Ys1),
	flatten_dif(Xs, Ys1, Zs).
flatten_dif(X, [X|Xs], Xs).

