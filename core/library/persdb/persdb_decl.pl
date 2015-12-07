:- package(persdb_decl).
:- multifile('$is_persistent'/2).
:- data '$is_persistent'/2.
:- meta_predicate('$is_persistent'(spec,?)).

:- multifile persistent_dir/2.
:- data persistent_dir/2.

:- multifile persistent_dir/4.
:- data persistent_dir/4.

