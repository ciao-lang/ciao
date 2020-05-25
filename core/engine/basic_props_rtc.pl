:- module(basic_props_rtc, [
    % instantiation checks
    rtc_int/1,
    rtc_nnegint/1,
    rtc_flt/1,
    rtc_num/1,
    rtc_atm/1,
    rtc_struct/1,
    rtc_gnd/1,
    % compatibility checks
    compat_int/1,
    compat_nnegint/1,
    compat_flt/1,
    compat_num/1,
    compat_atm/1,
    compat_struct/1
], [nativeprops]).

% ---------------------------------------------------------------------------
% (rtcheck implementation for basic_props.pl)
% ---------------------------------------------------------------------------

% ----------------------------------------------------------------------
% versions of basic_props:int/1

% instantiation
rtc_int(X) :- integer(X).

% compatibility
compat_int(X) :- var(X), !.
compat_int(X) :- rtc_int(X).

% % generation
% gen_int(0).
% gen_int(N) :- posint(I), give_sign(I, N).
%
% posint(1).
% posint(N) :- posint(N1), N is N1+1.
%
% give_sign(P, P).
% give_sign(P, N) :- N is -P.

% ----------------------------------------------------------------------
% versions of basic_props:nnegint/1

% instantiation
rtc_nnegint(X) :- integer(X), X >= 0.

% compatibility
compat_nnegint(X) :- var(X), !.
compat_nnegint(X) :- rtc_nnegint(X).

% % generation
% gen_nnegint(0).
% gen_nnegint(N) :- posint(N).

% ----------------------------------------------------------------------
% versions of basic_props:flt/1

% instantiation
rtc_flt(T) :- float(T).

% compatibility
compat_flt(T) :- var(T), !.
compat_flt(T) :- rtc_flt(T).

% % generation
% rtc_flt(T) :- int(N), T is N/10.

% ----------------------------------------------------------------------
% versions of basic_props:num/1

% instantiation
rtc_num(T) :- number(T).

% compatibility
compat_num(T) :- var(T), !.
compat_num(T) :- rtc_num(T).

% % generation
% gen_num(T) :- gen_int(T).
% gen_num(T) :- gen_flt(T).

% ----------------------------------------------------------------------
% versions of basic_props:atm/1

% instantiation
rtc_atm(T) :- atom(T).

% compatibility
compat_atm(T) :- var(T), !.
compat_atm(T) :- rtc_atm(T).

% % generation
% rtc_atm(a). % TODO: incomplete! it should be at least current_atom/1

% ----------------------------------------------------------------------
% versions of basic_props:struct/1

% instantiation
rtc_struct(T) :- nonvar(T), functor(T, _, A), A > 0. % compound(T).

% compatibility
compat_struct(T) :- var(T), !.
compat_struct(T) :- rtc_struct(T).

% generation

% ----------------------------------------------------------------------
% instantiation rtcheck version for basic_props:gnd/1

rtc_gnd(X)  :- var(X), !, fail.
rtc_gnd([]) :- !.
rtc_gnd(T)  :-
    functor(T, _, A),
    grnd_args(A, T).

grnd_args(0, _).
grnd_args(N, T) :-
    arg(N, T, A),
    rtc_gnd(A),
    N1 is N-1,
    grnd_args(N1, T).
