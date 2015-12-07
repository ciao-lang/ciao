:- module(_, _, []).

?- error_in_lns(_, _, warning,
'The GSL library does not seem to be installed. Using dummy (non-functional) version instead. If you would like to use the math library and related libraries such as the cost analysis please install GSL, and then run again the Ciao configuration and rebuild the system.').

:- load_compilation_module(library(gsl_imports/gsl_imports_dummy_tr)).
:- add_sentence_trans(gsl_imports_dummy_tr:gsl_imports_def/3, 750).

:- op(1150, fx, [dummy]).

throw_dummy_error(P) :-
	throw(error(existence_error, P)),
	fail.

:- dummy properties_to_matrix/5.
:- dummy matrix_to_properties/5.
:- dummy matrix_destroy/1.
:- dummy vector_destroy/1.
:- dummy gsl_vector_alloc/2.
:- dummy gsl_linalg_QR_decomp/2.
:- dummy vector_to_list/3.
:- dummy list_to_vector/3.
:- dummy gsl_matrix_alloc/3.
:- dummy gsl_linalg_QR_unpack/4.
:- dummy gsl_linalg_QR_U/2.
:- dummy gsl_linalg_QR_lssolve/5.
:- dummy gsl_linalg_QR_Rsvx/2.
:- dummy gsl_version/1.
:- dummy polynomial_root/5.
