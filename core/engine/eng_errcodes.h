/*
 *  eng_errcodes.h
 *
 *  Error codes for Prolog exceptions (see eng_errcodes_p.pl for documentation).
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_ERRHANDLE_H
#define _CIAO_ENG_ERRHANDLE_H

/* NOTE: Errors identifiers cannot be zero (as 0 = -0) */

#define ERR_instantiation_error INSTANTIATION_ERROR
#define ERR_uninstantiation_error UNINSTANTIATION_ERROR
#define ERR_type_error(X) TYPE_ERROR(TYPE_ERRORS(X))
#define ERR_representation_error(X) REPRESENTATION_ERROR(REPRESENTATION_ERRORS(X))
#define ERR_existence_error(X) EXISTENCE_ERROR(EXISTENCE_ERRORS(X))
#define ERR_permission_error(X,Y) PERMISSION_ERROR(PERMISSION_TYPES(X), PERMISSION_OBJECTS(Y))
#define ERR_domain_error(X) DOMAIN_ERROR(DOMAIN_ERRORS(X))
#define ERR_evaluation_error(X) EVALUATION_ERROR(EVALUATION_ERRORS(X))
#define ERR_resource_error(X) RESOURCE_ERROR(RESOURCE_ERRORS(X))
#define ERR_syntax_error SYNTAX_ERROR
#define ERR_system_error SYSTEM_ERROR
#define ERR_foreign_error FOREIGN_ERROR
#define ERR_user_exception USER_EXCEPTION

/* NOTE: Do not use directly, use ERR_ macros */
#if defined(OPTIM_COMP)
/* (generated from eng_errcodes.pl) */
#else
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
#define DOMAIN_ERRORS__character_code_list 0 // TODO:[JF] not ISO, remove
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

/* PERMISION_ERRORS (permission type + permission object) */
#define PERMISSION_TYPES(KEY) PERMISSION_TYPES__##KEY
#define PERMISSION_TYPES__access 0
#define PERMISSION_TYPES__create 1
#define PERMISSION_TYPES__input 2
#define PERMISSION_TYPES__modify 3
#define PERMISSION_TYPES__open 4
#define PERMISSION_TYPES__output 5
#define PERMISSION_TYPES__reposition 6
// Note: cannot be >=10!
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
#define REPRESENTATION_ERRORS(KEY) REPRESENTATION_ERRORS__##KEY
#define REPRESENTATION_ERRORS__character_code_list 0 // TODO:[JF] not ISO, remove
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

/* RESOURCE_ERROR */
#define RESOURCE_ERRORS(KEY) RESOURCE_ERRORS__##KEY
#define RESOURCE_ERRORS__r_undefined 0
#define RESOURCE_ERRORS__r_stack 1

#endif /* !defined(OPTIM_COMP) */ 

#endif /* _CIAO_ENG_ERRHANDLE_H */
