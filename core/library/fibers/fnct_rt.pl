% (included file)
% :- doc(title, "Runtime for extended module system").

% TODO: general, separate from fibers?

:- export(fnct_mod/2). % Get Mod where G is defined
fnct_mod(G, Mod) :-
	fnct_stub(G, G2),
	(G2 as fnct).decl_at_mod(Mod0),
	!, % (cut should not be needed)
	Mod = Mod0.

:- export(fnct_stub/2).
% G2 is G or stub G (using '$fnct_stub_rename'/2 if needed)
fnct_stub(G, G2) :-
	( (G as fnct).decl_at_mod(_) -> G2 = G
	; '$fnct_stub_rename'(G, G1), (G1 as fnct).decl_at_mod(_) -> G2 = G1
	; fail
	).

:- export(fnct_stub_only/1).
% We have only a stub for G but not its implementation
fnct_stub_only(G) :-
	( \+ (G as fnct).decl_at_mod(_),
	  '$fnct_stub_rename'(G, G1),
	  (G1 as fnct).decl_at_mod(_) ->
	     true
	; fail
	).

% (nondet)
:- export(fnct_property/2).
fnct_property(G, Prop) :-
	fnct_stub(G, G2),
	(G2 as fnct).prop(Prop).

