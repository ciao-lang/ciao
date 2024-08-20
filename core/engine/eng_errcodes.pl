% (included from absmach_def.pl -- ImProlog part)
%! \title Error codes for Prolog exceptions
%
% (see eng_errcodes_p.pl for documentation)

:- lowinclude(postdef_h, engine(eng_errcodes)).

% Encoding of error terms as integers.

% TODO: Simplify, share with eng_errcodes_p tables, reuse tagged box/unbox machinery?

% TODO: ~'$ccons'('A0',unknown) is a horrible hack! ImProlog compiler
%   generates '(...)' around macro params and this is incorrect for '##'

% Errors identifiers cannot be zero (as 0 = -0)
:- pred err_instantiation_error/1 + lowentrymacrocons(intmach, 'ERR_instantiation_error').
err_instantiation_error := 1.
:- pred err_uninstantiation_error/1 + lowentrymacrocons(intmach, 'ERR_uninstantiation_error').
err_uninstantiation_error := 2.
:- pred err_type_error/2 + lowentrymacrofun([iany], intmach, 'ERR_type_error').
err_type_error(D) := ~enc_errcode(type, ~type_err(~'$ccons'('A0',unknown))).
:- pred err_domain_error/2 + lowentrymacrofun([iany], intmach, 'ERR_domain_error').
err_domain_error(D) := ~enc_errcode(dom, ~domain_err(~'$ccons'('A0',unknown))).
:- pred err_existence_error/2 + lowentrymacrofun([iany], intmach, 'ERR_existence_error').
err_existence_error(D) := ~enc_errcode(exist, ~existence_err(~'$ccons'('A0',unknown))).
:- pred err_permission_error/3 + lowentrymacrofun([iany, iany], intmach, 'ERR_permission_error').
err_permission_error(D, F) := ~enc_errcode(perm, ~permission_perm(~'$ccons'('A0',unknown))*0x10 + ~permission_obj(~'$ccons'('A1',unknown))).
:- pred err_representation_error/2 + lowentrymacrofun([iany], intmach, 'ERR_representation_error').
err_representation_error(D) := ~enc_errcode(repres, ~representation_err(~'$ccons'('A0',unknown))).
:- pred err_evaluation_error/2 + lowentrymacrofun([iany], intmach, 'ERR_evaluation_error').
err_evaluation_error(D) := ~enc_errcode(eval, ~evaluation_err(~'$ccons'('A0',unknown))).
:- pred err_resource_error/2 + lowentrymacrofun([iany], intmach, 'ERR_resource_error').
err_resource_error(D) := ~enc_errcode(res, ~resource_err(~'$ccons'('A0',unknown))).
:- pred err_syntax_error/1 + lowentrymacrocons(intmach, 'ERR_syntax_error').
err_syntax_error := ~enc_errcode(syntax, 0).
:- pred err_system_error/1 + lowentrymacrocons(intmach, 'ERR_system_error').
err_system_error := ~enc_errcode(system, 0).
:- pred err_foreign_error/1 + lowentrymacrocons(intmach, 'ERR_foreign_error').
err_foreign_error := ~enc_errcode(foreign, 0).
:- pred err_user_exception/1 + lowentrymacrocons(intmach, 'ERR_user_exception').
err_user_exception := ~enc_errcode(user, 0).

%:- pred enc_errcode/3 + lowentrymacrofun([iany, intmach], intmach, '_enc_errcode').
:- pred enc_errcode/3 + prop(unfold).
enc_errcode(Type,Arg) := ~err_range * ~err_base(~'$ccons'(Type, unknown)) + Arg.

% Enough number of errors
%:- pred err_range/1 + lowentrymacrocons(intmach, 'ERR_RANGE').
:- pred err_range/1 + prop(unfold).
err_range := 0x100.

% :- pred err_base/2 + prop(unfold).
:- pred err_base/2 + lowentrymacrofuncons([iany], intmach, '_err_base').
err_base(X) := ~'$keytable'(X, [
    case(inst, 0),
    case(type, 1),
    case(dom, 2),
    case(exist, 3),
    case(perm, 4),
    case(repres, 5),
    case(eval, 6),
    case(res, 7),
    case(syntax, 8),
    case(system, 9),
    case(foreign, 10),
    case(user, 11)
]).

:- pred type_err/2 + lowentrymacrofuncons([iany], intmach, '_type_err').
type_err(X) := ~'$keytable'(X, [
    case(atom, 0),
    case(atomic, 1),
    case(byte, 2),
    case(character, 3),
    case(compound, 4),
    case(evaluable, 5),
    case(in_byte, 6),
    case(integer, 7),
    case(list, 8),
    case(number, 9),
    case(predicate_indicator, 10),
    % case(variable, 11), % (deprecated, corr2, use uninstantiation)
    case(callable, 12)
]).

:- pred domain_err/2 + lowentrymacrofuncons([iany], intmach, '_domain_err').
domain_err(X) := ~'$keytable'(X, [
    case(character_code_list, 0), % TODO:[JF] not ISO, remove
    case(source_sink, 1),
    case(stream, 2),
    case(io_mode, 3),
    case(non_empty_list, 4),
    case(not_less_than_zero, 5),
    case(operator_priority, 6),
    case(prolog_flag, 7),
    case(read_option, 8),
    case(flag_value, 9),
    case(close_option, 10),
    case(stream_option, 11),
    case(stream_or_alias, 12),
    case(stream_position, 13),
    case(stream_property, 14),
    case(write_option, 15),
    case(operator_specifier, 16)
]).

:- pred existence_err/2 + lowentrymacrofuncons([iany], intmach, '_existence_err').
existence_err(X) := ~'$keytable'(X, [
    case(procedure, 0),
    case(source_sink, 1),
    case(stream, 2)
]).

% PERMISION_ERRORS: composed of action + object on which the action is defined

:- pred permission_perm/2 + lowentrymacrofuncons([iany], intmach, '_permission_perm').
permission_perm(X) := ~'$keytable'(X, [
    case(access, 0),
    case(create, 1),
    case(input, 2),
    case(modify, 3),
    case(open, 4),
    case(output, 5),
    case(reposition, 6)
]).

:- pred permission_obj/2 + lowentrymacrofuncons([iany], intmach, '_permission_obj').
permission_obj(X) := ~'$keytable'(X, [
    case(binary_stream, 0),
    case(source_sink, 1),
    case(stream, 2),
    case(text_stream, 3),
    case(flag, 4),
    case(operator, 5),
    case(past_end_of_stream, 6),
    case(private_procedure, 7),
    case(static_procedure, 8)
]).

:- pred representation_err/2 + lowentrymacrofuncons([iany], intmach, '_representation_err').
representation_err(X) := ~'$keytable'(X, [
    case(character_code_list, 0), % TODO:[JF] not ISO, remove
    case(in_character_code, 1),
    case(max_arity, 2),
    case(character, 3),
    case(max_integer, 4),
    case(min_integer, 5),
    case(character_code, 6),
    case(nan_or_inf_to_integer, 7),
    case(max_atom_length, 8)
]).

:- pred evaluation_err/2 + lowentrymacrofuncons([iany], intmach, '_evaluation_err').
evaluation_err(X) := ~'$keytable'(X, [
    case(float_overflow, 0),
    case(int_overflow, 1),
    case(e_undefined, 2),
    case(e_underflow, 3),
    case(zero_divisor, 4)
]).

:- pred resource_err/2 + lowentrymacrofuncons([iany], intmach, '_resource_err').
resource_err(X) := ~'$keytable'(X, [
    case(r_undefined, 0),
    case(r_stack, 1)
]).

