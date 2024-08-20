% (included from internals.pl -- Prolog part)
%! \title Error codes for Prolog exceptions
%
%  Error codes (ISO Prolog and some extensions) are represented as
%  (most of the time) compound Prolog terms. In order to throw errors
%  from C code easily, these definitions provide bidirectional
%  encoding between those terms and integer representation. This file
%  provides the Prolog-side decoding of error codes.
%
%  See core/lib/errhandle.pl to learn how these work before updating
%  anything here.
%
%  Check eng_errcodes.pl for the ImProlog side (generate C defs).
%  Check eng_errcodes.h for the C header side (encoder).
%
%  **NOTE**: Keep all files synchronized!

% TODO: alternative: great ground (non-backtrackable) terms on the heap, keep global pointers

% (exported)
% decode_errcode(+Code, +Culprit, -ErrTerm)
decode_errcode(1, _, ErrTerm) :- !, ErrTerm = instantiation_error.
decode_errcode(2, Culprit, ErrTerm) :- !, ErrTerm = uninstantiation_error(Culprit).
decode_errcode(Code, Culprit, ErrTerm) :-
    split_errcode(Code, ErrType, ErrArg),
    decode_errcode_(ErrType, ErrArg, Culprit, ErrTerm).

decode_errcode_(foreign, _, Culprit, foreign_error(Culprit)).
decode_errcode_(system, _, _, system_error).
decode_errcode_(syntax, _, _, syntax_error).
decode_errcode_(res, Arg, _, resource_error(Res)) :- resource_code(Arg, Res).
decode_errcode_(user, _, _, user_error).
decode_errcode_(eval, Arg, _, evaluation_error(Type)) :- evaluation_code(Arg, Type).
decode_errcode_(repres, Arg, _, representation_error(Type)) :- representation_code(Arg, Type).
decode_errcode_(type, Arg, Culprit, type_error(Type, Culprit)) :- type_code(Arg, Type).
decode_errcode_(dom, Arg, Culprit, domain_error(Type, Culprit)) :- domain_code(Arg, Type).
decode_errcode_(exist, Arg, Culprit, existence_error(Type, Culprit)) :- existence_code(Arg, Type).
decode_errcode_(perm, Arg, Culprit, permission_error(Perm, Obj, Culprit)) :-
    split_perm_obj(Arg, Obj0, Perm0),
    permission_type_code(Perm0, Perm),
    permission_object_code(Obj0, Obj).

% split_errcode(+Code, -ErrType, -ErrArg)
split_errcode(Code, ErrType, ErrArg) :-
    ErrType0 is Code // 0x100,
    ErrArg is Code mod 0x100,
    error_start(ErrType0, ErrType).

split_perm_obj(PermObj, Obj, Perm) :-
    Obj is PermObj mod 0x10,
    Perm is PermObj // 0x10.

error_start(0, inst).
error_start(1, type).
error_start(2, dom).
error_start(3, exist).
error_start(4, perm).
error_start(5, repres).
error_start(6, eval).
error_start(7, res).
error_start(8, syntax).
error_start(9, system).
error_start(10, foreign).
error_start(11, user).

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
