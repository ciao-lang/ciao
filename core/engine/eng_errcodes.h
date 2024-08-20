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

#if defined(OPTIM_COMP)
/* (generated from eng_errcodes.pl) */
#else
#define ERR_instantiation_error 1
#define ERR_uninstantiation_error 2
#define ERR_type_error(D) _enc_error(type,_type_err(D))
#define ERR_domain_error(D) _enc_error(dom,_domain_err(D))
#define ERR_existence_error(D) _enc_error(exist,_existence_err(D))
#define ERR_permission_error(D,F) _enc_error(perm, _permission_perm(D)*0x10+_permission_obj(F))
#define ERR_representation_error(D) _enc_error(repres, _representation_err(D))
#define ERR_evaluation_error(D) _enc_error(eval,_evaluation_err(D))
#define ERR_resource_error(D) _enc_error(res,_resource_err(D))
#define ERR_syntax_error _enc_error(syntax,0)
#define ERR_system_error _enc_error(system,0)
#define ERR_foreign_error _enc_error(foreign,0)
#define ERR_user_exception _enc_error(user,0)

#define _enc_error(Type,Arg) (_err_range*_err_base(Type)+Arg)

#define _err_range 0x100 /* 8-bit arg per error */

#define _err_base(KEY) _err_base__##KEY
#define _err_base__inst    0
#define _err_base__type    1
#define _err_base__dom     2
#define _err_base__exist   3
#define _err_base__perm    4
#define _err_base__repres  5
#define _err_base__eval    6
#define _err_base__res     7
#define _err_base__syntax  8
#define _err_base__system  9
#define _err_base__foreign 10
#define _err_base__user    11

#define _type_err(KEY) _type_err__##KEY
#define _type_err__atom 0
#define _type_err__atomic 1
#define _type_err__byte 2
#define _type_err__character 3
#define _type_err__compound 4
#define _type_err__evaluable 5
#define _type_err__in_byte 6
#define _type_err__integer 7
#define _type_err__list 8
#define _type_err__number 9
#define _type_err__predicate_indicator 10
// #define _type_err__variable 11 // (deprecated, corr2, use uninstantiation)
#define _type_err__callable 12

#define _domain_err(KEY) _domain_err__##KEY
#define _domain_err__character_code_list 0 // TODO:[JF] not ISO, remove
#define _domain_err__source_sink 1
#define _domain_err__stream 2
#define _domain_err__io_mode 3
#define _domain_err__non_empty_list 4
#define _domain_err__not_less_than_zero 5
#define _domain_err__operator_priority 6
#define _domain_err__prolog_flag 7
#define _domain_err__read_option 8
#define _domain_err__flag_value 9
#define _domain_err__close_option 10
#define _domain_err__stream_option 11
#define _domain_err__stream_or_alias 12
#define _domain_err__stream_position 13
#define _domain_err__stream_property 14
#define _domain_err__write_option 15
#define _domain_err__operator_specifier 16

#define _existence_err(KEY) _existence_err__##KEY
#define _existence_err__procedure 0
#define _existence_err__source_sink 1
#define _existence_err__stream 2

#define _permission_perm(KEY) _permission_perm__##KEY
#define _permission_perm__access 0
#define _permission_perm__create 1
#define _permission_perm__input 2
#define _permission_perm__modify 3
#define _permission_perm__open 4
#define _permission_perm__output 5
#define _permission_perm__reposition 6

#define _permission_obj(KEY) _permission_obj__##KEY
#define _permission_obj__binary_stream 0
#define _permission_obj__source_sink 1
#define _permission_obj__stream 2
#define _permission_obj__text_stream 3
#define _permission_obj__flag 4
#define _permission_obj__operator 5
#define _permission_obj__past_end_of_stream 6
#define _permission_obj__private_procedure 7
#define _permission_obj__static_procedure 8

#define _representation_err(KEY) _representation_err__##KEY
#define _representation_err__character_code_list 0 // TODO:[JF] not ISO, remove
#define _representation_err__in_character_code 1
#define _representation_err__max_arity 2
#define _representation_err__character 3
#define _representation_err__max_integer 4
#define _representation_err__min_integer 5
#define _representation_err__character_code 6
#define _representation_err__nan_or_inf_to_integer 7
#define _representation_err__max_atom_length 8

#define _evaluation_err(KEY) _evaluation_err__##KEY
#define _evaluation_err__float_overflow 0
#define _evaluation_err__int_overflow 1
#define _evaluation_err__e_undefined 2
#define _evaluation_err__e_underflow 3
#define _evaluation_err__zero_divisor 4

#define _resource_err(KEY) _resource_err__##KEY
#define _resource_err__r_undefined 0
#define _resource_err__r_stack 1

#endif /* !defined(OPTIM_COMP) */ 

#endif /* _CIAO_ENG_ERRHANDLE_H */
