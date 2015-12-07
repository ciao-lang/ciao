:- package(menu).
:- use_package(assertions).
:- doc(nodoc, assertions).

:- load_compilation_module(library(menu/menu_tr)).
:- add_sentence_trans(menu_tr:menu_term_expansion/3, 330).

:- include(library(menu/menu_op)).
:- include(library(menu/menu_common)).

:- push_prolog_flag(multi_arity_warnings, off).

:- pop_prolog_flag(multi_arity_warnings).

:- use_module(library(menu/menu_generator),
	    [menu/1,
		menu/2,
		menu/3,
		menu/4,
		get_menu_flag/3,
		set_menu_flag/3,
		space/1,
		get_menu_flags/1,
		restore_menu_flags_list/1,
		eq/3,
		neq/3,
		uni_type/2,
		vmember/2
	    ]).

:- reexport(library(menu/menu_rt)).

% EXAMPLE: 
% ana , title # flag - option : pre_action :: post_action <- guard.
