% (included from absmach_def.pl -- ImProlog part)
%! \title Error code classification (ISO Prolog)
%
% (see eng_errhandle_p.pl for documentation)

:- lowinclude(postdef_h, engine(eng_errhandle)).

% Encoding of error terms as integers.
% TODO:[oc-merge] use ERR_ macros instead, define them here?

% TODO: This code is too complicated. Use the box/unbox machinery used
%   to represent tagged words to simplify it?

% Errors identifiers cannot be zero (as 0 = -0)
:- pred instantiation_error/1 + lowentrymacrocons(intmach, 'INSTANTIATION_ERROR').
instantiation_error := 1.
:- pred uninstantiation_error/1 + lowentrymacrocons(intmach, 'UNINSTANTIATION_ERROR').
uninstantiation_error := 2.
:- pred type_error/2 + lowentrymacrofun([intmach], intmach, 'TYPE_ERROR').
type_error(D) := ~range_per_error * ~error_start(~'$ccons'(type, unknown)) + D.
:- pred domain_error/2 + lowentrymacrofun([intmach], intmach, 'DOMAIN_ERROR').
domain_error(D) := ~range_per_error * ~error_start(~'$ccons'(dom, unknown)) + D.
:- pred existence_error/2 + lowentrymacrofun([intmach], intmach, 'EXISTENCE_ERROR').
existence_error(D) := ~range_per_error * ~error_start(~'$ccons'(exist, unknown)) + D.
:- pred permission_error/3 + lowentrymacrofun([intmach, intmach], intmach, 'PERMISSION_ERROR').
permission_error(D, F) := ~range_per_error * ~error_start(~'$ccons'(perm, unknown)) + D*10 + F.
:- pred representation_error/2 + lowentrymacrofun([intmach], intmach, 'REPRESENTATION_ERROR').
representation_error(D) := ~range_per_error * ~error_start(~'$ccons'(repres, unknown)) + D.
:- pred evaluation_error/2 + lowentrymacrofun([intmach], intmach, 'EVALUATION_ERROR').
evaluation_error(D) := ~range_per_error * ~error_start(~'$ccons'(eval, unknown)) + D.
:- pred resource_error/2 + lowentrymacrofun([intmach], intmach, 'RESOURCE_ERROR').
resource_error(D) := ~range_per_error * ~error_start(~'$ccons'(res, unknown)) + D.
:- pred syntax_error/1 + lowentrymacrocons(intmach, 'SYNTAX_ERROR').
syntax_error := ~range_per_error * ~error_start(~'$ccons'(syntax, unknown)).
%:- pred system_error :: intmach + lowentrymacrocons('SYSTEM_ERROR').
:- pred system_error/1 + lowentrymacrocons(intmach, 'SYSTEM_ERROR').
system_error := ~range_per_error * ~error_start(~'$ccons'(system, unknown)).
:- pred foreign_error/1 + lowentrymacrocons(intmach, 'FOREIGN_ERROR').
foreign_error := ~range_per_error * ~error_start(~'$ccons'(foreign, unknown)).
:- pred user_exception/1 + lowentrymacrocons(intmach, 'USER_EXCEPTION').
user_exception := ~range_per_error * ~error_start(~'$ccons'(user, unknown)).

% Enough number of errors
:- pred range_per_error/1 + lowentrymacrocons(intmach, 'RANGE_PER_ERROR').
range_per_error := 100.

:- pred error_start/2 + lowentrymacrofuncons([iany], intmach, 'error_start').
error_start(X) := ~'$keytable'(X, [
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

:- pred type_errors/2 + lowentrymacrofuncons([iany], intmach, 'TYPE_ERRORS').
type_errors(X) := ~'$keytable'(X, [
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

:- pred domain_errors/2 + lowentrymacrofuncons([iany], intmach, 'DOMAIN_ERRORS').
domain_errors(X) := ~'$keytable'(X, [
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

:- pred existence_errors/2 + lowentrymacrofuncons([iany], intmach, 'EXISTENCE_ERRORS').
existence_errors(X) := ~'$keytable'(X, [
    case(procedure, 0),
    case(source_sink, 1),
    case(stream, 2)
]).

% PERMISION_ERRORS: composed of type of action + object on which the action
% is defined

:- pred permission_types/2 + lowentrymacrofuncons([iany], intmach, 'PERMISSION_TYPES').
permission_types(X) := ~'$keytable'(X, [
    case(access, 0),
    case(create, 1),
    case(input, 2),
    case(modify, 3),
    case(open, 4),
    case(output, 5),
    case(reposition, 6)
]).

:- pred permission_objects/2 + lowentrymacrofuncons([iany], intmach, 'PERMISSION_OBJECTS').
permission_objects(X) := ~'$keytable'(X, [
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

:- pred representation_errors/2 + lowentrymacrofuncons([iany], intmach, 'REPRESENTATION_ERRORS').
representation_errors(X) := ~'$keytable'(X, [
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

:- pred evaluation_errors/2 + lowentrymacrofuncons([iany], intmach, 'EVALUATION_ERRORS').
evaluation_errors(X) := ~'$keytable'(X, [
    case(float_overflow, 0),
    case(int_overflow, 1),
    case(e_undefined, 2),
    case(e_underflow, 3),
    case(zero_divisor, 4)
]).

:- pred resource_errors/2 + lowentrymacrofuncons([iany], intmach, 'RESOURCE_ERRORS').
resource_errors(X) := ~'$keytable'(X, [
    case(r_undefined, 0),
    case(r_stack, 1)
]).

