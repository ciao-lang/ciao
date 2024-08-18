/*
 *  eng_errhandle.h
 *
 *  Read eng_errhandle_p.pl for documentation.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_ERRHANDLE_H
#define _CIAO_ENG_ERRHANDLE_H

#if defined(OPTIM_COMP)
/* (generated from eng_errhandle.pl) */
#else
/* Errors identifiers cannot be zero (as 0 = -0) */
#define INSTANTIATION_ERROR     1
#define UNINSTANTIATION_ERROR   2
#define TYPE_ERROR(D)           (RANGE_PER_ERROR*error_start(type)+D)
#define DOMAIN_ERROR(D)         (RANGE_PER_ERROR*error_start(dom)+D)
#define EXISTENCE_ERROR(D)      (RANGE_PER_ERROR*error_start(exist)+D)
#define PERMISSION_ERROR(D,F)   (RANGE_PER_ERROR*error_start(perm)+D*10+F)
#define REPRESENTATION_ERROR(D) (RANGE_PER_ERROR*error_start(repres)+D)
#define EVALUATION_ERROR(D)     (RANGE_PER_ERROR*error_start(eval)+D)
#define RESOURCE_ERROR(D)       (RANGE_PER_ERROR*error_start(res)+D)
#define SYNTAX_ERROR            (RANGE_PER_ERROR*error_start(syntax))
#define SYSTEM_ERROR            (RANGE_PER_ERROR*error_start(system))
#define FOREIGN_ERROR           (RANGE_PER_ERROR*error_start(foreign))
#define USER_EXCEPTION          (RANGE_PER_ERROR*error_start(user))

#define RANGE_PER_ERROR 100                    /* Enough number of errors */
#define error_start(KEY) error_start__##KEY
#define error_start__inst    0
#define error_start__type    1
#define error_start__dom     2
#define error_start__exist   3
#define error_start__perm    4
#define error_start__repres  5
#define error_start__eval    6
#define error_start__res     7
#define error_start__syntax  8
#define error_start__system  9
#define error_start__foreign 10
#define error_start__user    11

/* TYPE_ERRORS */
#define TYPE_ERRORS(KEY) TYPE_ERRORS__##KEY
#define TYPE_ERRORS__atom 0
#define TYPE_ERRORS__atomic 1
#define TYPE_ERRORS__byte 2
#define TYPE_ERRORS__character 3
#define TYPE_ERRORS__compound 4
#define TYPE_ERRORS__evaluable 5
#define TYPE_ERRORS__in_byte 6
#define TYPE_ERRORS__integer 7
#define TYPE_ERRORS__list 8
#define TYPE_ERRORS__number 9
#define TYPE_ERRORS__predicate_indicator 10
// #define TYPE_ERRORS__variable 11 // (deprecated, corr2, use uninstantiation)
#define TYPE_ERRORS__callable 12

/* DOMAIN_ERRORS */
#define DOMAIN_ERRORS(KEY) DOMAIN_ERRORS__##KEY
#define DOMAIN_ERRORS__character_code_list 0
#define DOMAIN_ERRORS__source_sink 1
#define DOMAIN_ERRORS__stream 2
#define DOMAIN_ERRORS__io_mode 3
#define DOMAIN_ERRORS__non_empty_list 4
#define DOMAIN_ERRORS__not_less_than_zero 5
#define DOMAIN_ERRORS__operator_priority 6
#define DOMAIN_ERRORS__prolog_flag 7
#define DOMAIN_ERRORS__read_option 8
#define DOMAIN_ERRORS__flag_value 9
#define DOMAIN_ERRORS__close_option 10
#define DOMAIN_ERRORS__stream_option 11
#define DOMAIN_ERRORS__stream_or_alias 12
#define DOMAIN_ERRORS__stream_position 13
#define DOMAIN_ERRORS__stream_property 14
#define DOMAIN_ERRORS__write_option 15
#define DOMAIN_ERRORS__operator_specifier 16

/* EXISTENCE_ERRORS */
#define EXISTENCE_ERRORS(KEY) EXISTENCE_ERRORS__##KEY
#define EXISTENCE_ERRORS__procedure 0
#define EXISTENCE_ERRORS__source_sink 1
#define EXISTENCE_ERRORS__stream 2

/* SOURCE_SINK and STREAM already defined */
/* #define SOURCE_SINK EXISTENCE_ERRORS(source_sink) */
/* #define STREAM EXISTENCE_ERRORS(stream) */

/* PERMISION_ERRORS: composed of type of action + object on which the action
   is defined */

/* PERMISSION_TYPE */
#define PERMISSION_TYPES(KEY) PERMISSION_TYPES__##KEY
#define PERMISSION_TYPES__access 0
#define PERMISSION_TYPES__create 1
#define PERMISSION_TYPES__input 2
#define PERMISSION_TYPES__modify 3
#define PERMISSION_TYPES__open 4
#define PERMISSION_TYPES__output 5
#define PERMISSION_TYPES__reposition 6

/* OBJECTS */
#define PERMISSION_OBJECTS(KEY) PERMISSION_OBJECTS__##KEY
#define PERMISSION_OBJECTS__binary_stream 0
#define PERMISSION_OBJECTS__source_sink 1
#define PERMISSION_OBJECTS__stream 2
#define PERMISSION_OBJECTS__text_stream 3
#define PERMISSION_OBJECTS__flag 4
#define PERMISSION_OBJECTS__operator 5
#define PERMISSION_OBJECTS__past_end_of_stream 6
#define PERMISSION_OBJECTS__private_procedure 7
#define PERMISSION_OBJECTS__static_procedure 8
/* REPRESENTATION_ERROR */
/* CHARACTER_CODE_LIST already defined */
#define REPRESENTATION_ERRORS(KEY) REPRESENTATION_ERRORS__##KEY
#define REPRESENTATION_ERRORS__character_code_list 0
#define REPRESENTATION_ERRORS__in_character_code 1
#define REPRESENTATION_ERRORS__max_arity 2
#define REPRESENTATION_ERRORS__character 3
#define REPRESENTATION_ERRORS__max_integer 4
#define REPRESENTATION_ERRORS__min_integer 5
#define REPRESENTATION_ERRORS__character_code 6
#define REPRESENTATION_ERRORS__nan_or_inf_to_integer 7
#define REPRESENTATION_ERRORS__max_atom_length 8

/* EVALUATION_ERROR */
#define EVALUATION_ERRORS(KEY) EVALUATION_ERRORS__##KEY
#define EVALUATION_ERRORS__float_overflow 0
#define EVALUATION_ERRORS__int_overflow 1
#define EVALUATION_ERRORS__e_undefined 2
#define EVALUATION_ERRORS__e_underflow 3
#define EVALUATION_ERRORS__zero_divisor 4

/* TODO:[oc-merge] new */
/* RESOURCE_ERROR */
#define RESOURCE_ERRORS(KEY) RESOURCE_ERRORS__##KEY
#define RESOURCE_ERRORS__r_undefined 0
#define RESOURCE_ERRORS__r_stack 1

#endif /* !defined(OPTIM_COMP) */ 

/* --------------------------------------------------------------------------- */

/* TODO:[oc-merge] unfold definitions, use keys in TYPE_ERROR(D),
   etc. so that we do not have conflicts! e.g, ErrCode_type(atom) */

#define STRICT_ATOM TYPE_ERRORS(atom)
#define ATOMIC TYPE_ERRORS(atomic)
#define TY_BYTE TYPE_ERRORS(byte)
#if !defined(OPTIM_COMP)
/* TODO:[oc-merge] used? */
#define CHARACTER TYPE_ERRORS(character)
#endif
#define COMPOUND TYPE_ERRORS(compound)
#define EVALUABLE TYPE_ERRORS(evaluable)
#define IN_BYTE TYPE_ERRORS(in_byte)
#define INTEGER TYPE_ERRORS(integer)
#define LIST TYPE_ERRORS(list)
#define NUMBER TYPE_ERRORS(number)
#define PREDICATE_INDICATOR TYPE_ERRORS(predicate_indicator)
#define CALLABLE TYPE_ERRORS(callable)

#if !defined(OPTIM_COMP)
/* TODO:[oc-merge] not in optim-comp */
#define CHARACTER_CODE_LIST     DOMAIN_ERRORS(character_code_list)
#endif
#define SOURCE_SINK             DOMAIN_ERRORS(source_sink)
#if !defined(OPTIM_COMP)
#define STREAM                  DOMAIN_ERRORS(stream)
#endif
#define IO_MODE DOMAIN_ERRORS(io_mode)
/* TODO:[oc-merge] not renamed in optim-comp (fixed?) */
#define NON_EMPTY_LIST DOMAIN_ERRORS(non_empty_list)
#define NOT_LESS_THAN_ZERO DOMAIN_ERRORS(not_less_than_zero)
#define OPERATOR_PRIORITY DOMAIN_ERRORS(operator_priority)
#define PROLOG_FLAG DOMAIN_ERRORS(prolog_flag)
#define READ_OPTION DOMAIN_ERRORS(read_option)
#define FLAG_VALUE DOMAIN_ERRORS(flag_value)
#define CLOSE_OPTION DOMAIN_ERRORS(close_option)
#define STREAM_OPTION DOMAIN_ERRORS(stream_option)
#define STREAM_OR_ALIAS DOMAIN_ERRORS(stream_or_alias)
#define STREAM_POSITION DOMAIN_ERRORS(stream_position)
#define STREAM_PROPERTY DOMAIN_ERRORS(stream_property)
#define WRITE_OPTION DOMAIN_ERRORS(write_option)
#define OPERATOR_SPECIFIER DOMAIN_ERRORS(operator_specifier)

#define PROCEDURE EXISTENCE_ERRORS(procedure)
#if defined(OPTIM_COMP)
#define STREAM EXISTENCE_ERRORS(stream)
#endif

#define ACCESS PERMISSION_TYPES(access)
#define CREATE PERMISSION_TYPES(create)
#define INPUT PERMISSION_TYPES(input)
#define MODIFY PERMISSION_TYPES(modify)
#define OPEN PERMISSION_TYPES(open)
#define OUTPUT PERMISSION_TYPES(output)
#define REPOSITION PERMISSION_TYPES(reposition)

#define BINARY_STREAM PERMISSION_OBJECTS(binary_stream)
/* SOURCE_SINK and STREAM already defined */
/* #define SOURCE_SINK PERMISSION_OBJECTS(binary_stream) */
/* #define STREAM PERMISSION_OBJECTS(binary_stream) */
#define TEXT_STREAM PERMISSION_OBJECTS(text_stream)
#define FLAG PERMISSION_OBJECTS(flag)
#define OPERATOR PERMISSION_OBJECTS(operator)
#define PAST_END_OF_STREAM PERMISSION_OBJECTS(past_end_of_stream)
#define PRIVATE_PROCEDURE PERMISSION_OBJECTS(private_procedure)
#define STATIC_PROCEDURE PERMISSION_OBJECTS(static_procedure)

#define IN_CHARACTER_CODE REPRESENTATION_ERRORS(in_character_code)
#define MAX_ARITY REPRESENTATION_ERRORS(max_arity)
#define MAX_INTEGER REPRESENTATION_ERRORS(max_integer)
#define MIN_INTEGER REPRESENTATION_ERRORS(min_integer)
#define CHARACTER_CODE REPRESENTATION_ERRORS(character_code)
#define NAN_OR_INF_TO_INTEGER REPRESENTATION_ERRORS(nan_or_inf_to_integer)
#define MAX_ATOM_LENGTH REPRESENTATION_ERRORS(max_atom_length)  /* Unneeded with dynamic atom sizes */

#define FLOAT_OVERFLOW EVALUATION_ERRORS(float_overflow)
#define INT_OVERFLOW EVALUATION_ERRORS(int_overflow)
#define E_UNDEFINED EVALUATION_ERRORS(e_undefined)
#define E_UNDERFLOW EVALUATION_ERRORS(e_underflow)
#define ZERO_DIVISOR EVALUATION_ERRORS(zero_divisor)

#define R_UNDEFINED RESOURCE_ERRORS(r_undefined)
#define R_STACK RESOURCE_ERRORS(r_stack)

#endif /* _CIAO_ENG_ERRHANDLE_H */
