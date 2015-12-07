:- module(_, _, [assertions, nativeprops, rtchecks]).

:- use_package(expander).


:- pred disc1(A) : (A=a).
:- pred disc1(A) : (A=b).
:- pred disc1(A) : (A=c).

disc1(a).
disc1(b).
disc1(c).

:- push_prolog_flag(rtchecks_inline, yes).

:- pred disc2(A) : (A=a).
:- pred disc2(A) : (A=b).
:- pred disc2(A) : (A=c).

disc2(a).
disc2(b).
disc2(c).

:- pop_prolog_flag(rtchecks_inline).

:- pred disc3(A) : (A=a).
:- pred disc3(A) : (A=b).
:- pred disc3(A) : (A=c).

disc3(a).
disc3(b).
disc3(c).


