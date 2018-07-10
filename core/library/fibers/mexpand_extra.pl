% (included file)

% TODO: general, separate from fibers?

% EXTENDED MODULE INTERFACES
% ==========================
%
% Auxiliary code to extend module expansion for functor symbols and
% with arbitrary metadata.
% 
% Metadata is organized by using traits (see traits package):
%
%  - meta_* : clauses for introspection at runtime
%  - static_* : facts for compile-time (usually at module interfaces)
%
% Traits for metadata must be declared with 'static_trait'
% (compile-time only) or 'trait' (runtime).

% ===========================================================================
% FIRST-PASS

% A static_trait declaration

decl_static_trait_spec(Trait, F, A, Cs) :-
	functor(Head, F, A),
	decl_head(Head, _, Trait, Head2),
	functor(Head2, F2, A2),
	Cs = [(:- new_declaration(F2/A2, on))].

% ---------------------------------------------------------------------------
% Emit metadata (for sentence translation)

% has_meta_i(MX, M, Trait)
:- data has_meta_i/3.
% has_export_functor(F, A, M)
:- data has_export_functor/3.

reset_mexpand_extra(M) :-
	retractall_fact(has_meta_i(_,M,_)),
	retractall_fact(has_export_functor(_,_,M)).

% declaration (runtime) of metadata for X (pred head, module, etc.)
ensure_meta_i(M, X, Trait) -->
	{ Trait = transient -> % TODO:T253 Fixme (no mod name)! 
	    MX = X
	; mod_concat_head(M, X, MX)
	},
	( { has_meta_i(MX, M, Trait) } -> []
	; { functor(MX, F, A) },
	  [(:- impl(Trait, F/A))],
	  { assertz_fact(has_meta_i(MX, M, Trait)) }
	).

% Clause for metadata
meta_cl(M, X, Trait, H, B) -->
	ensure_meta_i(M, X, Trait),
	{ Trait = transient -> % TODO:T253 Fixme (no mod name)! 
	    MX = X
	; mod_concat_head(M, X, MX)
	},
 	[('\6\dot'(as(MX, Trait), H) :- B)].

% Fact for metadata
meta_fact(M, X, Trait, H) -->
	meta_cl(M, X, Trait, H, true).

% Static fact for metadata
static_fact(_M, X, Trait, H) --> !,
	{ decl_head(H, X, Trait, Decl) },
	[(:- Decl)].

% Export predicate/functor symbol
% TODO: use normal functor/predicate symbol exports
ensure_export_functor(M, X) -->
	{ functor(X, F, A) },
	( { has_export_functor(F, A, M) } ->
	    []
	; [(:- '$export_functor'(F, A))],
	  { assertz_fact(has_export_functor(F, A, M)) }
	).

% TODO: move to traits package?

:- use_module(library(terms), [atom_concat/2]).

% decl_head(+Head, +SelfAny, +Trait, -Decl):
%   Obtain a compile-time fact (as internal declaration) for the a
%   given functor, trait, and predicate head.
%
% (see traits_tr:trait_head/4)

decl_head(Head, SelfAny, Trait, Decl) :-
	Head =.. [F|Args],
	mangle_static_hook(Trait, F, NF),
	Decl =.. [NF, SelfAny|Args].

% (see traits_tr:mangle_hook/3)
mangle_static_hook(Trait, F, NF) :-
	atom_concat(['static$', Trait, '.', F], NF).

% ---------------------------------------------------------------------------

% (duplicated)
% mod_concat_head('', H, H2) :- !, H2 = H. % TODO: remove support for empty module?
mod_concat_head(M, H, H2) :-
	H =.. [N|As],
	mod_concat(M, N, N2),
	H2 =.. [N2|As].

% (duplicated)
mod_concat(M, N, N2) :-
	atom_concat(M, '.', N1),
	atom_concat(N1, N, N2).

% ===========================================================================
% SECOND-PASS

:- use_module(library(compiler/c_itf_internal), [
	defines_module/2,
	decl/2,
	uses/2,
	base_name/2,
	defines_pred/3, exports_pred/3,
	module_error/0]).

% functor_expansion(+X, +M, +QM, -NX):
%   Obtain the module expansion for functor X, from module M, given
%   optional module qualification QM (NX has form _:_).

functor_expansion(X, _, _, _) :- var(X), !, fail.
functor_expansion(X, M, -, NX) :- !,
	unqualified_functor_expansion(X, M, NX).

unqualified_functor_expansion(X, M, NX) :-
	mod_base(M, Base),
	functor(X, F, A),
	( % Consult if X is declared at M
	  decl(Base, '$export_functor'(F, A)) -> NX = M:X
	; % or imported from IMod
	  uses(Base, IFile),
	  base_name(IFile, IBase),
	  decl(IBase, '$export_functor'(F, A)) -> 
	    base_mod(IBase, IMod),
	    NX = IMod:X
	; fail % not found! % TODO: module_warning/1
	).

mod_base(Mod, Base) :- % TODO: bad indexing
	defines_module(Base0, Mod), !,
	Base = Base0.

base_mod(Base, Mod) :-
	defines_module(Base, Mod0), !,
	Mod = Mod0.

is_exported_nd(Mod,F,A) :-
        mod_base(Mod, Base),
        is_exported_nd_(Base, F, A).

is_exported_nd_(Base, F, A) :-
	exports_pred(Base, all, all), !,
	defines_pred(Base, F, A).
is_exported_nd_(Base, F, A) :-
        exports_pred(Base, F, A).

% TODO: generalize c_itf_internal:module_warning/1, use location
mod_error(Mod, Err) :-
	message(error, ['(in ', Mod, ') '|Err]),
	set_fact(module_error).

% Consult static fact (Mod:X as Trait).G
% TODO: (only one solution)
current_static_fact(Mod:X, Trait, G) :-
	mod_base(Mod, Base),
	( decl_head(G, X, Trait, Decl),
	  decl(Base, Decl) ->
	    true
	; fail
	).

