:- package(loops).

% Loops notation

% TODO: missing 'break' and 'continue'
% TODO: make inline and generic iterators optional? do unfolding automatically?

% NOTE: Use is/2 explicitly, otherwise loops will not work with fun_eval(false)

:- use_package(xsyntax/'_xsyntax').
:- use_package(xsyntax/'_xcontrol'). % (needed for statevars and loops)
:- use_package(statevars). % statevars are required for iterators

% Enable "Term { ... }" syntax
:- set_prolog_flag(read_postfix_blocks, on).
:- op(40, yf, ({})).

% If-then-else syntax
:- op(950, fx, (while)).
:- op(950, fx, (for)).
:- op(950, fx, (if)).
:- op(955, xfy, (else)). % TODO: was 800, make it higher to support 'else if'

% If-then-else-if
:- notation(if (Cond) { Then } else if A,
            if (Cond) { Then } else { if A }).
% If-then-else-if-else-...
:- notation(if (Cond) { Then } else ( A else B ),
            if (Cond) { Then } else { A else B }).
% If-then-else
:- notation(if (Cond) { Then } else { Else },
            ( Cond -> Then ; Else )).
% Note: ( Cond -> Then ) is equivalent to ( Cond -> Then ; fail )
:- notation(if (Cond) { Then },
            ( Cond -> Then ; true )).

% While loops
:- notation(while (Cond) { Goal },
            '\6\loop'([], true, Cond, Goal, true)).

% Iterators
:- op( 700, xfx, (in)).

% For-each loops with iterators
:- notation_macro(for (Iters) { Goal },
                  '\6\loop'('iter0.map'(vars,IterInfo),
                            'iter0.map'(init,IterInfo),
                            'iter0.map'(cond,IterInfo),
                            'iter0.map'(next,IterInfo),
                            Goal),
                  % (replace IterInfo by processing of .iter new)
                  IterInfo, 'iter0.new'(Iters)).
% Combination of iterators
:- notation('iter0.new'((X in Iter)), iter(X, 'iter.new'(Iter), _Curr)).
:- notation('iter0.new'((A,B)), ('iter0.new'(A),'iter0.new'(B))).
%
:- notation('iter0.map'(vars, iter(X, _, _)), X).
:- notation('iter0.map'(init, iter(_, A, Curr)), 'iter.init'(A, Curr)).
:- notation('iter0.map'(cond, iter(X, A, Curr)), 'iter.cond'(A, X, Curr)).
:- notation('iter0.map'(next, iter(_, A, Curr)), 'iter.next'(A, Curr)).
:- notation('iter0.map'(What, (A,B)), ('iter0.map'(What, A),'iter0.map'(What, B))).

% (allow no spaces)
% TODO: hardwire?
:- notation(while(A) { Goal }, while (A) { Goal }).
:- notation(for(A) { Goal }, for (A) { Goal }).
:- notation(for(A,B) { Goal }, for (A,B) { Goal }).
:- notation(for(A,B,C) { Goal }, for (A,B,C) { Goal }).

% TODO: use traits
:- if(defined(loops__def_gen_iter)).
:- discontiguous iter_cond/2.
:- discontiguous iter_next/2.
:- endif.

% generic iterator
:- notation('iter.new'(Xs), gen_iterable(Xs)).
:- notation('iter.init'(gen_iterable(In), Curr), (Curr:=In)).
:- notation('iter.cond'(gen_iterable(_), X, Curr), iter_cond(Curr, X)).
:- notation('iter.next'(gen_iterable(_), Curr), (iter_next(Curr, Curr2), Curr:=Curr2)).

% list iterator
:- notation('iter.new'([]), list_iterable([])).
:- notation('iter.new'([X|Xs]), list_iterable([X|Xs])).
:- notation('iter.new'(list(Xs)), list_iterable(Xs)).
:- notation('iter.init'(list_iterable(In), Curr), (Curr:=In)).
:- notation('iter.cond'(list_iterable(_), X, Curr), (Curr=[X|_])).
:- notation('iter.next'(list_iterable(_), Curr), (Curr=[_|Xs], Curr:=Xs)). % TODO: optimize this unification
:- if(defined(loops__def_gen_iter)).
iter_cond([X|_], X).
iter_next([_|Xs], Xs).
:- endif.

% pure list iterator (without cuts) % TODO: experimental!
:- notation('iter.new'(pure_list(Xs)), pure_list_iterable(Xs)).
:- notation('iter.init'(pure_list_iterable(In), Curr), (Curr:=In)).
:- notation('iter.cond'(pure_list_iterable(_), X, Curr), ('\6\posneg'(Curr=[X|_], Curr=[]))).
:- notation('iter.next'(pure_list_iterable(_), Curr), (Curr=[_|Xs], Curr:=Xs)). % TODO: optimize this unification

% range iterator
:- op(550, yfx, ..).
:- notation('iter.new'(A..B), range_iterable(A, B, _, 1)).
:- notation('iter.new'(A..Step..B), range_iterable(A, B, _, Step)).
:- notation('iter.init'(range_iterable(A, B, End, Step), Curr), (End=B, Curr:=A)).
:- notation('iter.cond'(range_iterable(_, _, End, _), X, Curr), (X=Curr, Curr=<End)).
:- notation('iter.next'(range_iterable(_, _, _, Step), Curr),
                     % (Curr:=Curr+Step)
            (Tmp is Curr+Step, Curr:=Tmp)).
:- if(defined(loops__def_gen_iter)).
mk_iter_range(A, B, Step, range_iter(B, Step, A)).
iter_cond(range_iter(B, _Step, X), X) :- X=<B.
iter_next(range_iter(B, Step, Curr), range_iter(B, Step, Curr2)) :- Curr2 is Curr+Step.
:- endif.

% term argument iterator
:- notation('iter.new'(args(T)), args_iterable(N, T)).
:- notation('iter.init'(args_iterable(N, T), Curr), (functor(T, _, N), Curr:=1)).
:- notation('iter.cond'(args_iterable(N, T), X, Curr), (Curr=<N, arg(Curr,T,X))).
:- notation('iter.next'(args_iterable(N, T), Curr),
            % (Curr:=Curr+1)
            (Tmp is Curr+1, Curr:=Tmp)).
:- if(defined(loops__def_gen_iter)).
mk_iter_args(T, args_iter(N, T, Curr)) :- functor(T, _, N), Curr=1.
iter_cond(args_iter(N, T, Curr), X) :- Curr=<N, arg(Curr,T,X).
iter_next(args_iter(N, T, Curr), args_iter(N, T, Curr2)) :- Curr2 is Curr+1.
:- endif.
