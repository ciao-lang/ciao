:- module(_, [], [assertions, nativeprops]).

:- doc(title, "Tests for atomic_basic.pl").

:- use_module(engine(atomic_basic)).

:- test atom_codes(A,B): (A='año') => (B = "año") + (not_fails, is_det).

%atom_codes(A,B) :- atomic_basic:atom_codes(A,B).

:- export(flt_base/1).
flt_base(10).

:- test number_codes(A, B, C) : ( A = 0.0, flt_base(B) ) => C = "0.0".
:- test number_codes(A, B, C) : ( flt_base(B), C="0.0" ) => A = 0.0.
:- test number_codes(A, B, C) : ( A = 1.0, flt_base(B) ) => C = "1.0".
:- test number_codes(A, B, C) : ( flt_base(B), C="1.0" ) => A = 1.0.
:- test number_codes(A, B, C) : ( A = 0.Inf, flt_base(B) ) => C = "0.Inf".
:- test number_codes(A, B, C) : ( flt_base(B), C="0.Inf" ) => A = 0.Inf.
:- test number_codes(A, B, C) : ( A = -1.0, flt_base(B) ) => C = "-1.0".
:- test number_codes(A, B, C) : ( flt_base(B), C="-1.0" ) => A = -1.0.
:- test number_codes(A, B, C) : ( A = -1.0, flt_base(B) ) => C = "-1.0".
:- test number_codes(A, B, C) : ( flt_base(B), C = "-1.0" ) => A = -1.0.
:- test number_codes(A, B, C) : ( A = -0.Inf, flt_base(B) ) => C = "-0.Inf".
:- test number_codes(A, B, C) : ( flt_base(B), C = "-0.Inf" ) => A = -0.Inf.
:- test number_codes(A, B, C) : ( A = 0.Nan, flt_base(B) ) => C = "0.Nan".
:- test number_codes(A, B, C) : ( flt_base(B), C = "0.Nan" ) => A = 0.Nan.

%% (temporarily disabled -- this is not a test but a test generator, it should
%%  not be here since it is not deterministic and it may not appear)
%%
%% % TODO: number_codes_/3 should not be needed. Fix rtchecks.
%% :- prop number_codes_/3 # "We defined number_codes_/3 as a property to
%%         use it in the tests.".
%% number_codes_(A,B,C) :- number_codes(A,B,C).
%%
%% % TODO: strange test (head should not be number_codes, e.g., like if testing that "reverse o reverse = id")
%% :- test number_codes(A, B, C) :
%%      (
%%          float_random(A),
%%          flt_base(B)
%%      ) =>
%%      call((
%%          number_codes_(A1, B, C),
%%          near(A1, A, 0.0000000001)
%%      )) + times(50) # "Reversibility test 1".
%%
%% :- test number_codes(A, B, C) :
%%      (
%%          float_random(A0),
%%          flt_base(B),
%%          number_codes_(A0, B, C)
%%      ) =>
%%      (
%%          near(A, A0, 0.0000000001)
%%      ) + times(50) # "Reversibility test 2".
%%
%% :- impl_defined(float_random/1).
%% :- impl_defined(near/3).

:- test number_codes(A, B, C) : ( A = 19.26, B = 10 ) => C = "19.26".
:- test number_codes(A, B, C) : ( B = 10, C = "19.26" ) => A = 19.26.

% :- export(valid_base/1).
% :- prop valid_base/1 + regtype # "Valid numeric base to convert
%    numbers to strings or atoms".
% 
% valid_base(2).  valid_base(3).  valid_base(4).  valid_base(5).  valid_base(6).
% valid_base(7).  valid_base(8).  valid_base(9).  valid_base(10). valid_base(11).
% valid_base(12). valid_base(13). valid_base(14). valid_base(15). valid_base(16).
% valid_base(17). valid_base(18). valid_base(19). valid_base(20). valid_base(21).
% valid_base(22). valid_base(23). valid_base(24). valid_base(25). valid_base(26).
% valid_base(27). valid_base(28). valid_base(29). valid_base(30). valid_base(31).
% valid_base(32). valid_base(33). valid_base(34). valid_base(35). valid_base(36).

%number_codes(A,B,C) :- atomic_basic:number_codes(A,B,C).

:- test atom_number(A, B) : ( B = 0.0 ) => ( A = '0.0' ).
:- test atom_number(A, B) : ( A = '0.0' ) => ( B = 0.0 ).
:- test atom_number(A, B) : ( B = -0.0 ) => ( A = '-0.0' ).
:- test atom_number(A, B) : ( A = '-0.0' ) => ( B0 = -0.0, B = B0 ).
:- test atom_number(A, B) : ( B = 1.0 ) => ( A = '1.0' ).
:- test atom_number(A, B) : ( A = '1.0' ) => ( B = 1.0 ).
:- test atom_number(A, B) : ( B = 0.Inf ) => ( A = '0.Inf' ).
:- test atom_number(A, B) : ( A = '0.Inf' ) => ( B = 0.Inf ).
:- test atom_number(A, B) : ( B =  -1.0 ) => ( A = '-1.0' ).
:- test atom_number(A, B) : ( A = '-1.0' ) => ( B = -1.0 ).
:- test atom_number(A, B) : ( B =  -1.0 ) => ( A = '-1.0' ).
:- test atom_number(A, B) : ( A = '-1.0' ) => ( B = -1.0 ).
:- test atom_number(A, B) : ( B =  -0.Inf ) => ( A = '-0.Inf' ).
:- test atom_number(A, B) : ( A = '-0.Inf' ) => ( B = -0.Inf ).
:- test atom_number(A, B) : ( B =  -0.Inf ) => ( A = '-0.Inf' ).
:- test atom_number(A, B) : ( B = 0.Nan ) => ( A = '0.Nan' ).
:- test atom_number(A, B) : ( A = '0.Nan' ) => ( B = 0.Nan ).

%atom_number(A,B) :- atomic_basic:atom_number(A,B).
