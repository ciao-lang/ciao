:- module(pattern_filler, [get_pattern/2, complete_with_pattern/3],
	    [assertions, regtypes]).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(system)).
:- use_module(library(operators)).

:- doc(bug, "Simplify this module and rewrite its documentation").

:- doc(author, "David Trallero Mena").

:- doc(title, "Binary Operator Patterns Filler").

:- doc(module, "This module has been thought to complete terms that
   are composed using binary operators. This is really useful because
   most of the grammars used in many applications (for example
   @apl{lpmake}) are built using binary (and unary) operators. For
   example a ficticious lpmake entry like

@tt{entry_point <- [list_of_dependencies] :- ...}

Would express that @tt{entry_point} would be executed after
@tt{list_of_dependencies} would be already processed. But many times
the user will desire to write something like:

@tt{entry_point <- :- ...}

to specify that there are no dependencies. But when processing it, the
programmer desires to see:

@tt{entry_point <- [] :- ...}

This is exactly what this package do, complete the fields that user
does not fill-in.").

:- export(op_pattern/1).
:- regtype op_pattern/1.

op_pattern(_).

:- pred get_pattern(Term, Pattern)
	:: (term(Term), op_pattern(Pattern))
   # "@var{Pattern} is an abstract type that represent @var{Term}
     operators, its order and its default values. Default values are
     specified in @var{Term}: whatever <- default_value_for<- :
     Default_value_for: ...".

get_pattern(Term, [Head|Pattern]) :-
	arg(1, Term, Head),
	get_operators(Term, Pattern, []).

:- pred complete_with_pattern(Term, Pattern, NTerm)
	: (term(Term), op_pattern(Pattern), var(NTerm))
	=> term(NTerm)
   # "Missing terms in @var{Term} are completed using @var{Pattern}
      and the result is returned in @var{NTerm}. Example: if a pattern
      is got from (aa + bb - cc) (look at @pred{get_pattern/2}, and
      (gg + -ff) is intended to be completed, the result would be (gg
      + bb - ff).".

complete_with_pattern(Term, [Head|Pattern], NTerm) :-
	(arg(1, Term, Head) -> true ; true),
	complete_operators(Pattern, Term, NTerm).

:- pred get_operators(Term, L, LT)
   # "Operators of @var{Term} and its default values are the elements
      of @var{L}. @var{LT} is the trail. Example:

@begin{verbatim}
?- X = (aa + bb - cc), get_operators( X , L , LT ).

L = [[-|cc],[+|bb]|LT],
X = aa+bb-cc ? 
@end{verbatim}

[_|_] is used instead (_,_) because someone told me it is faster. ".

get_operators(Term, L, L) :-
	var(Term),
	!.
get_operators(Term, L, LT) :-
	Term =.. [OP, A, B],
	current_op(_, _, OP),
	get_def_op_opt(B, D),
	L = [[OP|D]|LsA],
	get_operators(A, LsA,  LsAT),
	get_operators(B, LsAT, LT),
	!.
get_operators(_, L, L).

:- pred get_def_op_opt(Operand, DefValue)
   # "When having a term like 'a + b - c', it is easy to know that b
      is the default value for + and c for -, but when processing the
      term, it is not that clear, because that terms is something
      like: '-(+(a,b),c)'. So, basically, the default value is the
      second operand iff it is an atom, or the recursively 1st operand
      of the second operand. Clear Example: '(a+b-c*d)' is
      '-(+(a,b),*(c,d))'. The default value of '-' is the 1st operand
      of '*(c,d)' (that is the 2nd operand of -).".

get_def_op_opt(B, B) :-
	var(B),
	!.
get_def_op_opt(B, B) :-
	functor(B, _, 0),
	!.
get_def_op_opt(B, D) :-
	get_def_op_opt__(B, D).

get_def_op_opt__(O, O) :-
	var(O),
	!.
get_def_op_opt__(O, D) :-
	O =.. [OP, A, _B],
	current_op(_, _, OP),
	!,
	get_def_op_opt__(A, D).
get_def_op_opt__(A, A).

:- pred complete_operators(Op, Term, NTerm)
   # "This is the main predicate. Here is how it works:

      @begin{itemize}
      @item Pass the unary operators to the corresponding binary ones
            (if they exists).

      @item Reorder operators. Now that (unary) operators have
            changed, reading the term can change the order of building
            it (i.e., '+'(_,'-'(_)) is different than '+'('-'(_,_),_)
            for example) due to priorities.

      @item Get the pattern of the new reordered term (The unfilled
            fields will become in variables).

      @item Now it is as easy as unify the patterns, i.e., make the
            free variables from the previous point get bound to the
            default corresponding value in the @var{Op} patter.

      @item Transform the pattern into a term.
      @end{itemize}".

complete_operators(O, Term, NTerm) :-
	remove_unary_operators(Term, TermWOUO),
	reorder_operators(TermWOUO, CleanTerm),
	get_pattern(CleanTerm, [_|CleanPattern]),
	complete_pattern(O, CleanPattern, CompleteTerm),
	base_case(Term, Base),
	build_term(CompleteTerm, Base, NTerm).
%	complete_operators__( O , CleanTerm , Left , NT ),
%	complete_out_operators( Left, NT , NTerm ).

complete_pattern([[O|D]|Os], [[O|'$fillin']|Rs], [[O|D]|Ts]) :-
	!,
	complete_pattern(Os, Rs, Ts).
complete_pattern([[O|_]|Os], [[O|D]|Rs], [[O|D]|Ts]) :-
	!,
	complete_pattern(Os, Rs, Ts).
complete_pattern([[O|D]|Os], Rs, [[O|D]|Ts]) :-
	!,
	complete_pattern(Os, Rs, Ts).
complete_pattern(A,  [], A).
complete_pattern([], _,  []).

base_case(Term, Term) :-
	(var(Term) ; atom(Term)),
	!.
base_case(Term, Base) :-
	Term =.. [_, A, _],
	base_case(A, Base),
	!.
base_case(Term, Term).

:- pred remove_unary_operators(Term, NTerm)
	: (term(Term), var(NTerm))
	=> term(NTerm)
   # "Whenever a unary operator appears in @var(Term), it is
      substituted by its binary operator putting the atom '$fillin' in
      the missing operand.".

remove_unary_operators(Term, NTerm) :-
	Term =.. [O, A, B],
	current_op(_, Type, O),
	(Type = xfy ; Type = yfx ; Type = xfx),
	!,
	remove_unary_operators(A, NA),
	remove_unary_operators(B, NB),
	NTerm =.. [O, NA, NB].
remove_unary_operators(Term, NTerm) :-
	Term =.. [O, A],
	current_op(_, Type, O),
	(Type = fy ; Type = yf ; Type = xf ; Type = fx),
	current_op(_, Type2, O),
	(Type2 = xfy ; Type2 = yfx ; Type2 = xfx),
	!,
	remove_unary_operators(A, NA),
% ---: do we have to look wether is fx or xf???
	NTerm =.. [O, '$fillin', NA].
remove_unary_operators(Term, Term).

build_term([[O|V]], B, A) :-
	current_op(_, Type, O),
	( (Type = xfy ; Type=xfx ) ->
	    A =.. [O, B, V]
	; Type = yfx,
	  A =.. [O, B, V]
	).
build_term([[O|V]|Os], B, A) :-
	build_term(Os,       V, As),
	build_term([[O|As]], B, A).

% Given a term compose _only_ by operator of _arity 2_,
% it writes the term without parenthesis. The idea
% is to read them later to "reasociate" operators 
% according its priorities.
plain_write(S, T) :-
	T =.. [O, A, B],
	current_op(_, _, O),
	plain_write(S, A),
	write(S, ' '),
	writeq(S, O),
	write(S, ' '),
	plain_write(S, B).
plain_write(S, T) :-
	writeq(S, T).

% What comes here is just a reused code taken from sformat. The idea is
% basically write a term in a plain way (i.e., without parenthesis) and
% reread it. This is necessary because when the unary terms are
% transformed into binary ones, the association order changes, and it is
% mandatory operators order to be preserved when 'completing' with the
% pattern.

:- initialization(create_temp_files).

:- data rread_temp_file/1.

check_writable_file(FileName) :-
	mktemp_in_tmp('reorder_operators_XXXXXX', FileName).

%% Initialization: create a file name in some suitable place (check
%% several), otherwise put a note that a pipe has to be used.
create_temp_files :-
	retractall_fact(rread_temp_file(_)), % Remove garbage
	( check_writable_file(FileName) -> % Can write somewhere?
	    delete_file(FileName), % We do not need it now
	    asserta_fact(rread_temp_file(FileName))
	;
	    asserta_fact(rread_temp_file(pipe)) % Filename != pipe !!
	).

reorder_operators(Term, NTerm) :-
	current_fact(rread_temp_file(FileName)),
	( FileName = pipe ->
	    pipe(Out, In),
	    plain_write(In, Term),
	    display(In, '.'),
	    close(In),
	    read_term(Out, NTerm),
	    close(Out)
	;
	    open(FileName, write, WriteStream),
	    plain_write(WriteStream, Term),
	    display(WriteStream, '.'),
	    close(WriteStream),
	    open(FileName, read, ReadStream),
	    read_term(ReadStream, NTerm, []),
	    close(ReadStream),
	    delete_file(FileName)
	).

%% X = (ana , title # flag - option : pre_action # guard),Y = (aa , tt # ff - oo), get_operators(X , L , [] ), complete_operators( L , Y , Z ).

%% X = (aa + bb - cc), Y = (aa + -ff), get_operators(X , L , [] ), complete_operators( L , Y , Z ).

%% X = (aa + bb - cc), Y = (aa + -ff), get_pattern( X , L ), complete_with_pattern( Y , L , Z ).
