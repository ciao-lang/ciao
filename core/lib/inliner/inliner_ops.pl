:- package(inliner_ops).
:- new_declaration(inline/1).
:- op(1170, fx, (inline)).

:- new_declaration(unfold/1).
:- op(1170, fx, (unfold)).

:- new_declaration(use_inline/1).
:- op(1170, fx, (use_inline)).

:- new_declaration(renamer/1).
:- op(1170, fx, (renamer)).
