:- module(fibers_blt, [], [assertions, fsyntax, hiord, fibers]).

:- doc(title, "Builtins for fibers").
:- doc(author, "Jose F. Morales").

% Do nothing
:- suspendable(true). % TODO:T253 really needed? it may have problems; add as builtin if needed?
true :- throw(error(undefined, fibers_blt:true/0)).

% fiber_meta_call/1 for suspendable predicates
%
%   NOTE: low-level, it can call any module (even if not imported)
%
:- suspendable(fiber_meta_call(term)).
fiber_meta_call(_) :- throw(error(undefined, fibers_blt:fiber_meta_call/1)).

% residue_reify(Goal, Residue):
%
%   Execute Goal until we have a residue (a stuck resolvent), 
%   unify Residue with this residue. Residue=[] if the residue
%   is empty.
%
%   NOTE: low-level, it can call any module (even if not imported)
%
:- suspendable(residue_reify(term,term)). % (builtin)
residue_reify(_,_) :- throw(error(undefined, fibers_blt:residue_reify/2)).

% yield_residue(_): forces suspension for residue_reify/2
:- suspendable(yield_residue(term)). % (builtin)
yield_residue(_) :- throw(error(undefined, fibers_blt:yield_residue/1)).
