:- package(foreign_interface_ttrs).


:- ttr_match(in_c_short, (c_short, ground, ground)).
:- ttr_match(go_c_short, (c_short, term, ground)).
:- ttr_def(in_c_short, [
	ctype_call  = short,
	ctype_decl  = short,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_short_s ]).
:- ttr_def(go_c_short, [
	ctype_res   = short,
	ctype_call  = pointer(short),
	ctype_decl  = short,
	call_cref   = yes,
	from_c      = ciao_mk_c_short_s ]).

:- ttr_match(in_c_int,   (c_int, ground, ground)).
:- ttr_match(go_c_int,   (c_int, term, ground)).
:- ttr_def(in_c_int, [
	ctype_call  = int,
	ctype_decl  = int,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_int_s ]).
:- ttr_def(go_c_int, [
	ctype_res   = int,
	ctype_call  = pointer(int),
	ctype_decl  = int,
	call_cref   = yes,
	from_c      = ciao_mk_c_int_s ]).

:- ttr_match(in_c_long,  (c_long, ground, ground)).
:- ttr_match(go_c_long,  (c_long, term, ground)).
:- ttr_def(in_c_long, [
	ctype_call  = long,
	ctype_decl  = long,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_long_s ]).
:- ttr_def(go_c_long, [
	ctype_res   = long,
	ctype_call  = pointer(long),
	ctype_decl  = long,
	call_cref   = yes,
	from_c      = ciao_mk_c_long_s ]).

:- ttr_match(in_c_ushort, (c_ushort, ground, ground)).
:- ttr_match(go_c_ushort, (c_ushort, term, ground)).
:- ttr_def(in_c_ushort, [
	ctype_call  = unsigned+short,
	ctype_decl  = unsigned+short,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_ushort_s ]).
:- ttr_def(go_c_ushort, [
	ctype_res   = unsigned+short,
	ctype_call  = pointer(unsigned+short),
	ctype_decl  = unsigned+short,
	call_cref   = yes,
	from_c      = ciao_mk_c_ushort_s ]).

:- ttr_match(in_c_uint,   (c_uint, ground, ground)).
:- ttr_match(go_c_uint,   (c_uint, term, ground)).
:- ttr_def(in_c_uint, [
	ctype_call  = unsigned+int,
	ctype_decl  = unsigned+int,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_uint_s ]).
:- ttr_def(go_c_uint, [
	ctype_res   = unsigned+int,
	ctype_call  = pointer(unsigned+int),
	ctype_decl  = unsigned+int,
	call_cref   = yes,
	from_c      = ciao_mk_c_uint_s ]).

:- ttr_match(in_c_ulong,  (c_ulong, ground, ground)).
:- ttr_match(go_c_ulong,  (c_ulong, term, ground)).
:- ttr_def(in_c_ulong, [
	ctype_call  = unsigned+long,
	ctype_decl  = unsigned+long,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_ulong_s ]).
:- ttr_def(go_c_ulong, [
	ctype_res   = unsigned+long,
	ctype_call  = pointer(unsigned+long),
	ctype_decl  = unsigned+long,
	call_cref   = yes,
	from_c      = ciao_mk_c_ulong_s ]).

:- ttr_match(in_c_float, (c_float, ground, ground)).
:- ttr_match(go_c_float, (c_float, term, ground)).
:- ttr_def(in_c_float, [
	ctype_call  = float,
	ctype_decl  = float,
	check       = ciao_is_number_s,
	exception   = error_in_arg('NUMBER'),
	to_c        = ciao_get_c_float_s ]).
:- ttr_def(go_c_float, [
	ctype_res   = float,
	ctype_call  = pointer(float),
	ctype_decl  = float,
	call_cref   = yes,
	from_c      = ciao_mk_c_float_s ]).

:- ttr_match(in_c_double, (c_double, ground, ground)).
:- ttr_match(go_c_double, (c_double, term, ground)).
:- ttr_def(in_c_double, [
	ctype_call  = double,
	ctype_decl  = double,
	check       = ciao_is_number_s,
	exception   = error_in_arg('NUMBER'),
	to_c        = ciao_get_c_double_s ]).
:- ttr_def(go_c_double, [
	ctype_res   = double,
	ctype_call  = pointer(double),
	ctype_decl  = double,
	call_cref   = yes,
	from_c      = ciao_mk_c_double_s ]).

:- ttr_match(in_c_uintptr, (c_uintptr, ground, ground)).
:- ttr_match(go_c_uintptr, (c_uintptr, term, ground)).
:- ttr_def(in_c_uintptr, [
	ctype_call  = uintptr_t,
	ctype_decl  = uintptr_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_uintptr_s ]).
:- ttr_def(go_c_uintptr, [
	ctype_res   = uintptr_t,
	ctype_call  = pointer(uintptr_t),
	ctype_decl  = uintptr_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_uintptr_s ]).

:- ttr_match(in_c_size, (c_size, ground, ground)).
:- ttr_match(go_c_size, (c_size, term, ground)).
:- ttr_def(in_c_size, [
	ctype_call  = size_t,
	ctype_decl  = size_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_size_s ]).
:- ttr_def(go_c_size, [
	ctype_res   = size_t,
	ctype_call  = pointer(size_t),
	ctype_decl  = size_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_size_s ]).

:- ttr_match(in_c_int8,  (c_int8, ground, ground)).
:- ttr_match(go_c_int8,  (c_int8, term, ground)).
:- ttr_def(in_c_int8, [
	ctype_call  = int8_t,
	ctype_decl  = int8_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_int8_s ]).
:- ttr_def(go_c_int8, [
	ctype_res   = int8_t,
	ctype_call  = pointer(int8_t),
	ctype_decl  = int8_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_int8_s ]).

:- ttr_match(in_c_int16, (c_int16, ground, ground)).
:- ttr_match(go_c_int16, (c_int16, term, ground)).
:- ttr_def(in_c_int16, [
	ctype_call  = int16_t,
	ctype_decl  = int16_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_int16_s ]).
:- ttr_def(go_c_int16, [
	ctype_res   = int16_t,
	ctype_call  = pointer(int16_t),
	ctype_decl  = int16_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_int16_s ]).

:- ttr_match(in_c_int32, (c_int32, ground, ground)).
:- ttr_match(go_c_int32, (c_int32, term, ground)).
:- ttr_def(in_c_int32, [
	ctype_call  = int32_t,
	ctype_decl  = int32_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_int32_s ]).
:- ttr_def(go_c_int32, [
	ctype_res   = int32_t,
	ctype_call  = pointer(int32_t),
	ctype_decl  = int32_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_int32_s ]).

:- ttr_match(in_c_int64, (c_int64, ground, ground)).
:- ttr_match(go_c_int64, (c_int64, term, ground)).
:- ttr_def(in_c_int64, [
	ctype_call  = int64_t,
	ctype_decl  = int64_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_int64_s ]).
:- ttr_def(go_c_int64, [
	ctype_res   = int64_t,
	ctype_call  = pointer(int64_t),
	ctype_decl  = int64_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_int64_s ]).

:- ttr_match(in_c_uint8,  (c_uint8, ground, ground)).
:- ttr_match(go_c_uint8,  (c_uint8, term, ground)).
:- ttr_def(in_c_uint8, [
	ctype_call  = uint8_t,
	ctype_decl  = uint8_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_uint8_s ]).
:- ttr_def(go_c_uint8, [
	ctype_res   = uint8_t,
	ctype_call  = pointer(uint8_t),
	ctype_decl  = uint8_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_uint8_s ]).

:- ttr_match(in_c_uint16, (c_uint16, ground, ground)).
:- ttr_match(go_c_uint16, (c_uint16, term, ground)).
:- ttr_def(in_c_uint16, [
	ctype_call  = uint16_t,
	ctype_decl  = uint16_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_uint16_s ]).
:- ttr_def(go_c_uint16, [
	ctype_res   = uint16_t,
	ctype_call  = pointer(uint16_t),
	ctype_decl  = uint16_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_uint16_s ]).

:- ttr_match(in_c_uint32, (c_uint32, ground, ground)).
:- ttr_match(go_c_uint32, (c_uint32, term, ground)).
:- ttr_def(in_c_uint32, [
	ctype_call  = uint32_t,
	ctype_decl  = uint32_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_uint32_s ]).
:- ttr_def(go_c_uint32, [
	ctype_res   = uint32_t,
	ctype_call  = pointer(uint32_t),
	ctype_decl  = uint32_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_uint32_s ]).

:- ttr_match(in_c_uint64, (c_uint64, ground, ground)).
:- ttr_match(go_c_uint64, (c_uint64, term, ground)).
:- ttr_def(in_c_uint64, [
	ctype_call  = uint64_t,
	ctype_decl  = uint64_t,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_get_c_uint64_s ]).
:- ttr_def(go_c_uint64, [
	ctype_res   = uint64_t,
	ctype_call  = pointer(uint64_t),
	ctype_decl  = uint64_t,
	call_cref   = yes,
	from_c      = ciao_mk_c_uint64_s ]).

:- ttr_match(in_atm, (atm, ground, ground)).
:- ttr_match(go_atm, (atm, term, ground)).
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

:- ttr_match(in_string, (string, ground, ground)).
:- ttr_match(go_string, (string, term, ground)).
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

:- ttr_match(in_address, (address, ground, ground)).
:- ttr_match(go_address, (address, term, ground)).
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

:- ttr_match(in_c_uint8_list, (c_uint8_list, ground, ground)).
:- ttr_match(go_c_uint8_list, (c_uint8_list, term, ground)).
:- ttr_def(in_c_uint8_list, [
	ctype_call  = pointer(unsigned+char),
	ctype_decl  = pointer(unsigned+char),
	check       = ciao_is_char_code_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_get_c_uint8_array,
	compound    = yes ]).
:- ttr_def(go_c_uint8_list, [
	ctype_res   = pointer(unsigned+char),
	ctype_call  = pointer(pointer(unsigned+char)),
	ctype_decl  = pointer(unsigned+char),
	call_cref   = yes,
	from_c      = ciao_mk_c_uint8_list,
	compound    = yes ]).

:- ttr_match(in_c_int_list, (c_int_list, ground, ground)).
:- ttr_match(go_c_int_list, (c_int_list, term, ground)).
:- ttr_def(in_c_int_list, [
	ctype_call  = pointer(int),
	ctype_decl  = pointer(int),
	check       = ciao_is_int_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_get_c_int_array,
	compound    = yes ]).
:- ttr_def(go_c_int_list, [
	ctype_res   = pointer(int),
	ctype_call  = pointer(pointer(int)),
	ctype_decl  = pointer(int),
	call_cref   = yes,
	from_c      = ciao_mk_c_int_list,
	compound    = yes ]).

:- ttr_match(in_c_double_list, (c_double_list, ground, ground)).
:- ttr_match(go_c_double_list, (c_double_list, term, ground)).
:- ttr_def(in_c_double_list, [
	ctype_call  = pointer(double),
	ctype_decl  = pointer(double),
	check       = ciao_is_num_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_get_c_double_array,
	compound    = yes ]).
:- ttr_def(go_c_double_list, [
	ctype_res   = pointer(double),
	ctype_call  = pointer(pointer(double)),
	ctype_decl  = pointer(double),
	call_cref   = yes,
	from_c      = ciao_mk_c_double_list,
	compound    = yes ]).

:- ttr_match(go_any_term, (any_term, term, ground)).
:- ttr_def(go_any_term, [
	ctype_res   = ciao_term,
	ctype_call  = pointer(ciao_term),
	ctype_decl  = ciao_term,
	call_cref   = yes,
	from_c      = '=' ]).
