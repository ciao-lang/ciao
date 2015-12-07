:- module(_, _, [assertions, nativeprops, rtchecks]).

:- use_package(expander).

:- discontiguous(disc1/1).

:- pred disc1(A) : (A=a).
disc1(a).
:- pred disc1(A) : (A=b).
disc1(b).
:- pred disc1(A) : (A=c).
disc1(c).
