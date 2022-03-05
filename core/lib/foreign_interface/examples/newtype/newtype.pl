:- module(newtype, [], [foreign_interface]).

% This example shows how to define a new type (called "newtype"), and
% their conversions from/to C.
%
% Example:
%   ?- newtype_mk(3,4,X), newtype_show(X).
%   [from C] newtype(3,4)
%   
%   X = '$newtype'(3,4) ? 
%   
%   yes

:- export(newtype/1).
:- regtype newtype(X) # "@var{X} an instance of C @tt{newtype_c}.".
newtype('$newtype'(A,B)) :-
    int(A),
    int(B).

% Type-translation rules
%
% This new C type is a structure passed by value. NOTE: this could be
% combined with other compatible type that has "pass by ref" rules.
%
:- ttr_match(in_newtype, (newtype, ground, ground)).
:- ttr_match(go_newtype, (newtype, term, ground)).
:- ttr_def(in_newtype, [
    ctype_decl  = newtype_t,
    ctype_call  = newtype_t,
    check       = ciao_is_newtype,
    exception   = usage_fault("foreign interface: newtype conversion received ill argument (needed $newtype/2)"),
    to_c        = ciao_term_to_newtype ]).
:- ttr_def(go_newtype, [
    ctype_decl  = newtype_t,
    ctype_res   = newtype_t,
    ctype_call  = pointer(newtype_t),
    call_cref   = yes,
    from_c      = ciao_newtype_to_term ]).

% A constructor
:- export(newtype_mk/3).
:- trust pred newtype_mk(in(A), in(B), go(X)) :: c_int * c_int * newtype + (foreign,returns(X)).

% Some operation on the type
:- export(newtype_show/1).
:- trust pred newtype_show(in(X)) :: newtype + foreign.

:- use_foreign_source(newtype_c).
:- use_foreign_gluecode_header(local('newtype_c.h')). % add definitions to the gluecode
% (note: this can be used to enforce inlining of operations) 
:- extra_compiler_opts(['-O2']).
