:- package(rtchecks_tr_inline).
:- use_package(inliner).

:- inline_module(library(hiordlib), [map/3]).
% We use append/3 and select/3 inlined to avoid problems if the list
% module is compiled with run-time checks
:- inline_module(library(lists), [append/3, reverse/2, reverse/3,
		select/3, difference/3, intersection/3]).

:- inline pre_error/2.
:- inline pre_fails/2.
:- inline diff_props_2/3.
:- inline compat_rtcheck/7.
:- inline pre_lit/2.

:- unfold collect_checks(no, yes, no).
:- unfold diff_props(yes, yes, no).
:- unfold pre_error(yes, yes).
:- unfold pre_fails(yes, yes).
:- unfold pre_lit(yes, yes).
:- unfold compat_rtcheck(yes, no, no, no, no, no, yes).
:- unfold collect_checks(yes, yes, yes).
:- inline_module(library(rtchecks/rtchecks_basic)).
:- inline_module(library(rtchecks/rtchecks_meta)).
