:- module(fastrw,
        [fast_read/1,
         fast_read/2,
         fast_write/1,
         fast_write/2],
         [assertions,isomodes]).

:- doc(title, "Fast reading and writing of terms").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Oscar Portela Arjona").

:- doc(module, "This library provides predicates to support reading /
   writing of terms on a format designed to be:

@begin{itemize}
@item Simple to implement at the engine level, with very few
  dependencies.
@item Faster to handle than the standard representation.
@end{itemize}
").

:- doc(bug, "Both @pred{fast_read/2} and @pred{fast_write/2} simply
   set the current output/input and call @pred{fast_read/1} and
   @pred{fast_write/1}.  Therefore, in the event an error happens
   during its execution, the current input / output streams may be
   left pointing to the @var{Stream}").

:- doc(fast_read(Term), "The next term is read from current standard
   input and is unified with @var{Term}. The syntax of the term must
   agree with fast_read / fast_write format. If the end of the input
   has been reached, @var{Term} is unified with the term
   'end_of_file'. Further calls to @pred{fast_read/1} will then cause
   an error.").

:- trust pred fast_read/1.
:- impl_defined(fast_read/1). % engine/io_basic.c

:- doc(fast_write(Term),"Output @var{Term} in a way that
   @pred{fast_read/1} and @pred{fast_read/2} will be able to read it
   back.").

:- trust pred fast_write/1.
:- impl_defined(fast_write/1). % engine/io_basic.c

:- doc(fast_write(Stream, Term), "Output @var{Term} to @var{Stream} in
   a way that @pred{fast_read/1} and @pred{fast_read/2} will be able
   to read it back.").

:- pred fast_write(+stream,@term).

fast_write(Stream, Term):-
        current_output(Old),
        set_output(Stream),
        fast_write(Term),
        set_output(Old).

:- doc(fast_read(Stream, Term), "The next term is read from
   @var{Stream} and unified with @var{Term}. The syntax of the term
   must agree with fast_read / fast_write format. If the end of the
   input has been reached, @var{Term} is unified with the term
   'end_of_file'. Further calls to @pred{fast_read/2} will then cause
   an error.").

:- pred fast_read(+stream,?term).

fast_read(Stream, Term):-
        current_input(Old),
        set_input(Stream),
        fast_read(Term),
        set_input(Old).

%% :- use_package(dcg).

%% version(0'C).
%% 
%% index_of(Dict, T, N) :- index_of_(Dict, T, 0, N).
%% 
%% index_of_(V, T, I, N) :-
%%         var(V), !,
%%         V = [T|_], N = I.
%% index_of_([T0|_], T, I, N) :-
%%         T0 == T, !,
%%         N = I.
%% index_of_([_|D], T, I, N) :-
%%         I1 is I+1,
%%         index_of_(D, T, I1, N).

%% :- export(fast_write_to_string/3).
%%
%% :- pred fast_write_to_string(+term, list, list).
%% 
%% fast_write_to_string(T, S, R) :-
%%         version(V),
%%         S = [V|S_],
%%         fastrw_term(T,_Dict, S_, R).
%% 
%% fastrw_term(V,Vdict) --> {var(V)}, !,
%%         "_",
%%         {index_of(Vdict, V, N), number_codes(N,S)},
%%         string(S), [0].
%% fastrw_term(I,_) --> {integer(I)}, !,
%%         "I",
%%         {number_codes(I,S)},
%%         string(S), [0].
%% fastrw_term(F,_) --> {float(F)}, !,
%%         "F",
%%         {number_codes(F,S)},
%%         string(S), [0].
%% fastrw_term([],_) --> !,
%%         "]".
%% fastrw_term(A,_) --> {atom(A)}, !,
%%         "A",
%%         {atom_codes(A,S)},
%%         string(S), [0].
%% fastrw_term([X|Xs],Vdict) --> {integer(X), X > 0, X =< 255}, !,
%%         """",
%%         [X],
%%         fastrw_string(Xs, Vdict).
%% fastrw_term([X|Xs],Vdict) --> !,
%%         "[",
%%         fastrw_term(X,Vdict),
%%         fastrw_term(Xs,Vdict).
%% fastrw_term(S,Vdict) --> {functor(S,F,A)},
%%         "S",
%%         {atom_codes(F,C)},
%%         string(C), [0,A],
%%         fastrw_args(S, 1, A, Vdict).
%% 
%% fastrw_args(S,I,N,Vdict) --> {I =< N}, !,
%%         {arg(I,S,T)},
%%         fastrw_term(T,Vdict),
%%         {I1 is I+1},
%%         fastrw_args(S,I1,N,Vdict).
%% fastrw_args(_,_,_,_) --> "".
%% 
%% fastrw_string([X|Xs],Vdict) --> {integer(X), X > 0, X =< 255}, !,
%%         [X],
%%         fastrw_string(Xs,Vdict).
%% fastrw_string(T,Vdict) -->
%%         [0],
%%         fastrw_term(T,Vdict).
%% 
%% string([]) --> "".
%% string([C|Cs]) -->
%%         [C],
%%         string(Cs).
