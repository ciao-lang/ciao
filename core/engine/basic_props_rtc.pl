:- module(basic_props_rtc,
        [
            rtc_int/1,
            rtc_nnegint/1,
            rtc_flt/1,
            rtc_num/1,
            rtc_atm/1,
            rtc_struct/1,
            rtc_gnd/1,
            rtc_compat/2,
            rtc_inst/2
        ],
        [nativeprops]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (rtcheck implementation for basic_props.pl)

:- set_prolog_flag(read_hiord, on).
:- import(hiord_rt, [call/2]).

:- use_module(library(terms_check), [instance/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------------------------------------
% rtcheck version for basic_props:int/1

rtc_int(X) :-
        nonvar(X), !,
        integer(X).

rtc_int(0).
rtc_int(N) :- posint(I), give_sign(I, N).

posint(1).
posint(N) :- posint(N1), N is N1+1.

give_sign(P, P).
give_sign(P, N) :- N is -P.

% ----------------------------------------------------------------------
% rtcheck version for basic_props:nnegint/1

rtc_nnegint(X) :-
        nonvar(X), !,
        integer(X),
	X >= 0.

rtc_nnegint(0).
rtc_nnegint(N) :- posint(N).

% ----------------------------------------------------------------------
% rtcheck version for basic_props:flt/1

rtc_flt(T) :-
        nonvar(T), !,
        float(T).
rtc_flt(T) :-
        int(N), T is N/10.

% ----------------------------------------------------------------------
% rtcheck version for basic_props:num/1

rtc_num(T) :-
        number(T), !.

rtc_num(T) :- rtc_int(T).
rtc_num(T) :- rtc_flt(T).

% ----------------------------------------------------------------------
% rtcheck version for basic_props:atm/1

rtc_atm(T) :- atom(T), !.
rtc_atm(a). % TODO: incomplete! it should be at least current_atom/1

% ----------------------------------------------------------------------
% rtcheck version for basic_props:struct/1

rtc_struct([_|_]):- !.
rtc_struct(T) :- functor(T, _, A), A>0. % compound(T).

% ----------------------------------------------------------------------
% rtcheck version for basic_props:gnd/1

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

% ----------------------------------------------------------------------
% rtcheck version for basic_props:compat/2

rtc_compat(T, P) :- \+ \+ P(T).

% ----------------------------------------------------------------------
% rtcheck version for basic_props:inst/2

:- meta_predicate rtc_inst(?,pred(1)).

rtc_inst( X , Prop ) :-
	A = Prop( X ),
	copy_term( A , AC ),
	AC,
	instance( A , AC ).
