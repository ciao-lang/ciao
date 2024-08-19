% (included from internals.pl -- Prolog part)
%! \title Error codes for Prolog exceptions
%
%  Error codes (ISO Prolog and some extensions) are represented as
%  (most of the time) compound Prolog terms. In order to throw errors
%  from C code easily, these definitions provide bidirectional
%  encoding between those terms and integer representation.
%
%  See core/lib/errhandle.pl to learn how these work before updating
%  anything here.
%
%  This file provides the Prolog-side definition for error codes.
%  Check eng_errcodes.pl for the ImProlog side (generate C defs).
%  Check eng_errcodes.h for the C header side.
%
%  **NOTE**: Keep all files synchronized!

% TODO: alternative: great ground (non-backtrackable) terms on the heap, keep global pointers

% TODO: optimize (divide by range to get section)
in_range(Type, Code, WhichWithinType):-
    range_per_error(Range),
    error_start(Type, Section),
    Start is Section * Range,
    Code >= Start,
    Code < Start + Range,
    WhichWithinType is Code - Start.

error_term(1, _, instantiation_error) :- !.
error_term(2, Culprit, uninstantiation_error(Culprit)) :- !.
error_term(Code, _, system_error) :- in_range(system, Code, _), !.
error_term(Code, _, syntax_error) :- in_range(syntax, Code, _), !.
error_term(N, _, resource_error(Res)) :- in_range(res, N, Code), !, 
    resource_code(Code, Res).
error_term(Code, _, user_error) :- in_range(user, Code, _), !.
error_term(N, _Culprit, evaluation_error(Type)) :- in_range(eval, N, Code), !,
    evaluation_code(Code, Type).
error_term(N, _Culprit, representation_error(Type)) :- in_range(repres, N, Code), !,
    representation_code(Code, Type).
error_term(N, Culprit, type_error(Type, Culprit)) :- in_range(type, N, Code), !,
    type_code(Code, Type).
error_term(N, Culprit, domain_error(Type, Culprit)) :- in_range(dom, N, Code), !,
    domain_code(Code, Type).
error_term(N, Culprit, existence_error(Type, Culprit)) :- in_range(exist, N, Code), !,
    existence_code(Code, Type).
error_term(N, Culprit, permission_error(Permission, Object, Culprit)) :- in_range(perm, N, Code), !,
    get_obj_perm(Code,Obj,Per),
    permission_type_code(Per, Permission),
    permission_object_code(Obj, Object).

%% Check error type and return get Code for every class of error.  This should
%% be made more modularly (i.e., with an C interface - but is it worth?)

 %% is_evaluation_error(N,Code) :-     N>120, N<126, Code is N-121.
 %% 
 %% is_representation_error(N,Code) :- N>114, N<121, Code is N-115.
 %% 
 %% is_type_error(N,Code) :-           N>1, N<15, Code is N-2.
 %% 
 %% is_domain_error(N,Code) :-         N>14, N<32, Code is N-15.
 %% 
 %% is_existence_error(N,Code) :-      N>31, N<35, Code is N-32.
 %% 
 %% is_permission_error(N,Code) :-     N>34, N<115, Code is N-35.

get_obj_perm(Code, Obj, Perm) :-
    Obj is Code mod 10,
    Perm is Code // 10.

 %% culprit_stream([], S) :- !, current_input(S).
 %% culprit_stream(S,S).

range_per_error(100).

error_start(inst,   0).
error_start(type,   1).
error_start(dom,    2).
error_start(exist,  3).
error_start(perm,   4).
error_start(repres, 5).
error_start(eval,   6).
error_start(res,    7).
error_start(syntax, 8).
error_start(system, 9).
error_start(foreign, 10).
error_start(user,   11).

type_code(0, atom).
type_code(1, atomic).
type_code(2, byte).
type_code(3, character).
type_code(4, compound).
type_code(5, evaluable).
type_code(6, in_byte).
type_code(7, integer).
type_code(8, list).
type_code(9, number).
type_code(10, predicate_indicator).
% type_code(11, variable). % (deprecated, corr2, use uninstantiation)
type_code(12, callable).

domain_code(0, character_code_list). % TODO:[JF] not ISO, remove
domain_code(1, source_sink).
domain_code(2, stream).
domain_code(3, io_mode).
domain_code(4, non_empty_list).
domain_code(5, not_less_than_zero).
domain_code(6, operator_priority).
domain_code(7, prolog_flag).
domain_code(8, read_option).
domain_code(9, flag_value).
domain_code(10, close_option).
domain_code(11, stream_option).
domain_code(12, stream_or_alias).
domain_code(13, stream_position).
domain_code(14, stream_property).
domain_code(15, write_option).
domain_code(16, operator_specifier).

existence_code(0, procedure).
existence_code(1, source_sink).
existence_code(2, stream).

permission_type_code(0, access).
permission_type_code(1, create).
permission_type_code(2, input).
permission_type_code(3, modify).
permission_type_code(4, open).
permission_type_code(5, output).
permission_type_code(6, reposition).

permission_object_code(0, binary_stream).
permission_object_code(1, source_sink).
permission_object_code(2, stream).
permission_object_code(3, text_stream).
permission_object_code(4, flag).
permission_object_code(5, operator).
permission_object_code(6, past_end_of_stream).
permission_object_code(7, private_procedure).
permission_object_code(8, static_procedure).

representation_code(0, character_code_list). % TODO:[JF] not ISO, remove
representation_code(1, in_character_code).
representation_code(2, max_arity).
representation_code(3, character).
representation_code(4, max_integer).
representation_code(5, min_integer).
representation_code(6, character_code).
representation_code(7, nan_or_inf_to_integer).
representation_code(8, max_atom_length).

evaluation_code(0, float_overflow).
evaluation_code(1, int_overflow).
evaluation_code(2, undefined).
evaluation_code(3, underflow).
evaluation_code(4, zero_divisor).

resource_code(0, undefined).
resource_code(1, heap).
