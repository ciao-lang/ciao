:- package(foreign_interface_ttrs).
:- ttr_match(in_int, (int, ground, ground)).
:- ttr_match(go_int, (int, term, ground)).
:- ttr_match(in_num, (num, ground, ground)).
:- ttr_match(go_num, (num, term, ground)).
:- ttr_match(in_atm, (atm, ground, ground)).
:- ttr_match(go_atm, (atm, term, ground)).
:- ttr_match(in_string, (string, ground, ground)).
:- ttr_match(go_string, (string, term, ground)).
:- ttr_match(in_byte_list, (byte_list, ground, ground)).
:- ttr_match(go_byte_list, (byte_list, term, ground)).
:- ttr_match(in_int_list, (int_list, ground, ground)).
:- ttr_match(go_int_list, (int_list, term, ground)).
:- ttr_match(in_double_list, (double_list, ground, ground)).
:- ttr_match(go_double_list, (double_list, term, ground)).
:- ttr_match(in_address, (address, ground, ground)).
:- ttr_match(go_address, (address, term, ground)).
:- ttr_match(go_any_term, (any_term, term, ground)).

:- ttr_def(in_int, [
	ctype_call  = long,
	ctype_decl  = long,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_to_integer_s ]).

:- ttr_def(go_int, [
	ctype_res   = long,
	ctype_call  = pointer(long),
	ctype_decl  = long,
	call_cref   = yes,
	from_c      = ciao_integer_s ]).

:- ttr_def(in_num, [
	ctype_call  = double,
	ctype_decl  = double,
	check       = ciao_is_number_s,
	exception   = error_in_arg('NUMBER'),
	to_c        = ciao_to_float_s ]).

:- ttr_def(go_num, [
	ctype_res   = double,
	ctype_call  = pointer(double),
	ctype_decl  = double,
	call_cref   = yes,
	from_c      = ciao_float_s ]).

:- ttr_def(in_atm, [
	ctype_call  = pointer(char),
	ctype_decl  = pointer(char),
	check       = ciao_is_atom_s,
	exception   = error_in_arg('STRICT_ATOM'),
	to_c        = ciao_atom_name_dup_s,
	free        = free ]).

:- ttr_def(go_atm, [
	ctype_res   = pointer(char),
	ctype_call  = pointer(pointer(char)),
	ctype_decl  = pointer(char),
	call_cref   = yes,
	from_c      = ciao_atom_s,
	free        = free ]).

:- ttr_def(in_string, [
	ctype_call  = pointer(char),
	ctype_decl  = pointer(char),
	check       = ciao_is_char_code_list,
	exception   = error_in_arg('CHARACTER_CODE_LIST'),
	to_c        = ciao_list_to_str,
	free        = free ]).

:- ttr_def(go_string, [
	ctype_res   = pointer(char),
	ctype_call  = pointer(pointer(char)),
	ctype_decl  = pointer(char),
	call_cref   = yes,
	from_c      = ciao_str_to_list,
	free        = free ]).

:- ttr_def(in_address, [
	ctype_call  = pointer(void),
	ctype_decl  = pointer(void),
	check       = ciao_is_address,
	exception   = usage_fault("foreign interface: pointer conversion received ill argument (needed $address/1)"),
	to_c        = ciao_address_to_pointer ]).

:- ttr_def(go_address, [
	ctype_res   = pointer(void),
	ctype_call  = pointer(pointer(void)),
	ctype_decl  = pointer(void),
	call_cref   = yes,
	from_c      = ciao_pointer_to_address ]).

:- ttr_def(in_byte_list, [
	ctype_call  = pointer(unsigned+char),
	ctype_decl  = pointer(unsigned+char),
	check       = ciao_is_char_code_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_list_to_byte_array,
	compound    = yes ]).

:- ttr_def(go_byte_list, [
	ctype_res   = pointer(unsigned+char),
	ctype_call  = pointer(pointer(unsigned+char)),
	ctype_decl  = pointer(unsigned+char),
	call_cref   = yes,
	from_c      = ciao_byte_listn,
	compound    = yes ]).

:- ttr_def(in_int_list, [
	ctype_call  = pointer(int),
	ctype_decl  = pointer(int),
	check       = ciao_is_int_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_list_to_int_array,
	compound    = yes ]).

:- ttr_def(go_int_list, [
	ctype_res   = pointer(int),
	ctype_call  = pointer(pointer(int)),
	ctype_decl  = pointer(int),
	call_cref   = yes,
	from_c      = ciao_int_listn,
	compound    = yes ]).

:- ttr_def(in_double_list, [
	ctype_call  = pointer(double),
	ctype_decl  = pointer(double),
	check       = ciao_is_double_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_list_to_double_array,
	compound    = yes ]).

:- ttr_def(go_double_list, [
	ctype_res   = pointer(double),
	ctype_call  = pointer(pointer(double)),
	ctype_decl  = pointer(double),
	call_cref   = yes,
	from_c      = ciao_double_listn,
	compound    = yes ]).

:- ttr_def(go_any_term, [
	ctype_res   = ciao_term,
	ctype_call  = pointer(ciao_term),
	ctype_decl  = ciao_term,
	call_cref   = yes,
	from_c      = '=' ]).
