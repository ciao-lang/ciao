/*  Adapted from shared code written by D.H.D.Warren + Richard O'Keefe; */
/*  all changes                                                         */
/*  Copyright (C) 1997-2002 UPM-CLIP. */

:- module(read, [read/1, read/2, read_term/2, read_term/3,
                read_top_level/3, second_prompt/2, read_option/1],
		[assertions, nortchecks, isomodes, define_flag]).

:- use_module(library(tokenize)).
:- use_module(library(operators)).
:- use_module(library(lists), [list_insert/2, dlist/3, length/2, append/3]).

:- doc(title,"Term input").  

:- doc(module,"This module provides falicities to read terms in
   Prolog syntax.  This is very convenient in many cases (and not only
   if you are writing a Prolog compiler), because Prolog terms are
   easy to write and can convey a lot of information in a
   human-readable fashion.").

:- doc(author, "Daniel Cabeza (modifications and documentation,"||
	       " adapted from SICStus 0.6 code)").
:- doc(author, "Manuel Carro (modifications and documentation)").
:- doc(author, "Jose F. Morales (modifications for curly blocks,"||
               "postfix blocks, infix dot, string constants, and doccomments)").

% TODO: Can this reader be implemented deterministically?
%   The reader can do backtracking at some points:
%     - when an operator can be infix and postfix at the same time
%       (e.g. op(400, xfy, a). op(400, yf, a). X = a a. )
%   This problem is inherited by many Prolog.
% (jfran)

:- set_prolog_flag(multi_arity_warnings, off).

:- doc(define_flag/3,"Defines flags as follows:
	@includedef{define_flag/3}
	(See @ref{Changing system behaviour and various flags}).

        If flag is @tt{on} (it is @tt{off} by default), a variable
        followed by a parenthesized lists of arguments is read as a
        @pred{call/N} term, except if the variable is anonymous, in
        which case it is read as an anonymous predicate abstraction
        head. For example, @tt{P(X)} is read as @tt{call(P,X)} and
        @tt{_(X,Y)} as @tt{''(X,Y)}.").

define_flag(read_hiord, [on,off], off).

% When enabled, read "{ <list of sentences> }" terms
define_flag(read_curly_blocks, [on,off], off).

% When enabled, allow "term term" as valid terms
define_flag(read_postfix_blocks, [on,off], off).

% When enabled, strings are read as '\6\string' terms (not as lists)
define_flag(read_string_data_type, [on,off], off).

% When enabled, infix dot '.' is read as '\6\dot' terms (not as a list)
define_flag(read_infix_dot, [on,off], off).

:- doc(read(Term), "Like @tt{read(Stream,Term)} with @var{Stream}
        associated to the current input stream.").

:- pred read(?term) + iso.

read(X) :-
        current_input(Stream),
        read_internal(X, Stream, Stream, _, _, _, read/1).

:- pred read(+Stream,?Term): stream * term + iso
# "The next term, delimited by a full-stop (i.e., a @tt{.} followed by
   either a space or a control character), is read from @var{Stream}
   and is unified with @var{Term}. The syntax of the term must agree
   with current operator declarations. If the end of @var{Stream} has
   been reached, @var{Term} is unified with the term @tt{end_of_file}.
   Further calls to @tt{read/2} for the same stream will then cause an
   error, unless the stream is connected to the terminal (in which case
   a prompt is opened on the terminal).".


read(Stream, X) :-
        current_input(CurIn),
        read_internal(X, Stream, CurIn, _, _, _, read/2).

:- pred read_term(+Stream,?Term,+Options) : 
        stream * term * list(read_option) + iso # 
"Reads a @var{Term} from @var{Stream} with the ISO-Prolog
@var{Options}.  These options can control the behavior of read term (see @pred{read_option/1}).".

read_term(Stream, X, Options) :-
        read_term_aux(Options, Stream, 3, X).


:- pred read_term(?Term,+Options) : term * list(read_option) + iso
# "Like @pred{read_term/3}, but reading from the @concept{current input}".

read_term(X, Options) :-
        current_input(Stream),
        read_term_aux(Options, Stream, 2, X).

read_term_aux(Options, Stream, N, X) :-
        option_list(Options, Vs, Ns, Ss, Lns, VarDict, N),
        current_input(CurIn),
        read_internal(X, Stream, CurIn, VarDict, Tokens, Lns, read_term/N),
        extract_vars(Vs, Tokens),
        extract_names(Ns, Ss, VarDict).

:- doc(read_top_level(Stream,Data,Variables),
        "Predicate used to read in the Top Level.").

read_top_level(Stream, Data, Variables) :-
        current_input(CurIn),
	read_internal(Data, Stream, CurIn, Variables, _, _, read_top_level/3).

option_list(V, _, _, _, _, _, Arg) :- var(V), !,
        throw(error(instantiation_error,read_term/Arg-Arg)).
option_list([], _, _, _, _, _, _) :- !.
option_list([O|Os], Vs, Ns, Ss, Ls, Dict, Arg) :- !,
        option(O, Vs, Ns, Ss, Ls, Dict, Arg),
        option_list(Os, Vs, Ns, Ss, Ls, Dict, Arg).
option_list(Os, _, _, _, _, _, Arg) :-
        throw(error(type_error(list,Os),read_term/Arg-Arg)).


:- doc(doinclude, read_option/1).

:- prop read_option(?Option) => atom + regtype # "@var{Option} is an
allowed @pred{read_term/[2,3]} option. These options are:
@includedef{read_option/1}
They can be used to return the singleton
variables in the term, a list of variables, etc.".


read_option(variables(_V)).
read_option(variable_names(_N)).
read_option(singletons(_S)).
read_option(lines(_StartLine, _EndLine)).
read_option(dictionary(_Dict)).

option(V, _, _, _, _, _, Arg) :- var(V), !,
        throw(error(instantiation_error,read_term/Arg-Arg)).
option(variables(Vs), variables(Vs), _, _, _, _, _) :- !.
option(variable_names(Ns), _, variable_names(Ns), _, _, _, _) :- !.
option(singletons(Ss), _, _, singletons(Ss), _, _, _) :- !.
option(lines(L0,L1), _, _, _, lines(L0,L1), _, _) :- !.
option(dictionary(Dict), _, _, _, _, Dict, _) :- !.
option(Op, _, _, _, _, _, Arg) :-
        throw(error(domain_error(read_option,Op),read_term/Arg-Arg)).

extract_vars(V, _) :- var(V), !. % No need of computing it
extract_vars(variables(Vs), Tokens) :-
        extract_vars2(Tokens, Vs1), Vs = Vs1.

extract_vars2([], Vs) :-
        list(Vs), !.
extract_vars2([var(V,_)|Tokens], Vs) :- !,
        list_insert(Vs, V),
        extract_vars2(Tokens, Vs).
extract_vars2([_|Tokens], Vs) :-
        extract_vars2(Tokens, Vs).
        
extract_names(Ns, Ss, _) :- var(Ns), var(Ss), !. % No need of computing it
extract_names(variable_names(Ns), singletons(Ss), VarDict) :-
        extract_names2(VarDict, Ns1, [], Ss1, []),
        Ns = Ns1, Ss = Ss1.

extract_names2(D, Ns, Ns, Ss, Ss) :- var(D), !.
extract_names2(dic(Str,[Var|Sing],L,R), Ns, Ns_, Ss, Ss_) :-
        extract_names2(L, Ns, Ns1, Ss, Ss1),
        name(Name,Str),
        Eq = (Name=Var),
        Ns1 = [Eq|Ns2],
        ( var(Sing) ->
              Ss1 = [Eq|Ss2]
        ; Ss1 = Ss2
        ),
        extract_names2(R, Ns2, Ns_, Ss2, Ss_).

read_internal(Answer, Stream, CurIn, Variables, Tokens, Lines, Predicate) :-
        catch(read_internal(Answer, Stream, CurIn, Variables, Tokens, Lines),
              error(ErrorTerm,_),
              (set_input(CurIn), throw(error(ErrorTerm,Predicate)))).

read_internal(Answer, Stream, CurIn, Variables, Tokens, lines(Ln0,Ln1)) :-
	set_input(Stream),
        line_count(Stream, L0), Ln0 is L0+1,
        read_tokens(Tokens, Variables),
        line_position(Stream, Pos),
        line_count(Stream, L1), Ln1 is L1+sign(Pos),
        ( Tokens = [] -> Term = end_of_file
        ; clearerr(Stream), % Just in case we have reached eof
          read(Tokens, 1200, Term, LeftOver),
          all_read(LeftOver) ->
            the_syntax_error([], 0, _, _) % erase any leftovers
        ; syntax_error_data(Tokens, Msg, ErrorLoc),
          throw(error(syntax_error([Ln0,Ln1,Msg,ErrorLoc]),'while reading'))
        ),
        set_input(CurIn),
	Answer = Term.

%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.
all_read([.]) :- !.
all_read([unexpected(-1)|S]) :- !,
        syntax_error(['unexpected end of file'], S).
all_read([unexpected(0'\n)|S]) :- !,
        syntax_error(['unexpected new line'], S).
all_read(S) :-
	syntax_error(['operator expected after expression'], S).

%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.
expect(Token, [Token|Rest], Out) :- !, Out=Rest.
expect(Token, S0, _) :-
	syntax_error([Token,' or operator expected'], S0).

%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op.
%   current_prefixop(O -> Self, Rarg)
%   current_postfixop(O -> Larg, Self)
%   current_infixop(O -> Larg, Self, Rarg)

%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, -Ans, -LeftOver)

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	S1 = [Token|_],
	\+ cant_start_expr(Token),
	(   Precedence < Oprec ->
	    syntax_error(['prefix operator ',Op,' in context with precedence ',
	                  Precedence],
		         S1)
	;   read(S1, Aprec, Arg, S2),
	    functor(Term, Op, 1),
	    arg(1, Term, Arg),
	    read_rest(S2, Oprec, Term, Precedence, Answer, S)
	).


cant_start_expr(')').
cant_start_expr(']').
cant_start_expr('}').
cant_start_expr('|').
cant_start_expr(',').
cant_start_expr(.).


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	syntax_error(['expression expected'], []).


%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

read(X, _, _, _, _) :- var(X), !, fail.		% space saver
read(var(Variable,Name), ['('|S1], Precedence, Answer, S) :-
        current_prolog_flag(read_hiord, on),
        !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3), !,
        ( Name = "_" ->
          Term =.. ['',Arg1|RestArgs]
        ; Term =.. [call,Variable,Arg1|RestArgs]
        ),
	read_rest(S3, 0, Term, Precedence, Answer, S).
read(var(Variable,_), S0, Precedence, Answer, S) :- !,
	read_rest(S0, 0, Variable, Precedence, Answer, S).
read(atom(-), [number(Number)|S1], Precedence, Answer, S) :-
	number(Number), !,
	Negative is -Number,
	read_rest(S1, 0, Negative, Precedence, Answer, S).
read(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3), !,
        ( Term =.. [Functor,Arg1|RestArgs] ->
            read_rest(S3, 0, Term, Precedence, Answer, S)
        ; syntax_error(['maximum arity exceeded'],S3)
        ).
read(atom(Functor), S0, Precedence, Answer, S) :-
	current_prefixop(Functor, Prec, Right),
	% TODO: It may backtrack here! Can it be rewritten in a
	% deterministic way?
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).
read(atom(Atom), S0, Precedence, Answer, S) :- !,
	read_rest(S0, 0, Atom, Precedence, Answer, S).
read(number(Number), S0, Precedence, Answer, S) :-
	number(Number), !,
	read_rest(S0, 0, Number, Precedence, Answer, S).
read(doccomment(Col, Mark, Chars), S0, Precedence, Answer, S) :-
	% A doccomment read as a prefix operator (JFMC)
	Prec = Precedence,
	Right = Precedence,
	% TODO: Copied from prefix atoms, check again -- I removed
	% backtracking.
	S0 = [Token|_],
	\+ cant_start_expr(Token), !,
	read(S0, Right, Arg, S1),
	functor(Term, '\6\doccomment', 5),
	arg(1, Term, Col),
	arg(2, Term, Mark),
	arg(3, Term, prefix),
	arg(4, Term, Chars),
	arg(5, Term, Arg),
	read_rest(S1, Prec, Term, Precedence, Answer, S).
read(doccomment(Col, Mark, Chars), S0, Precedence, Answer, S) :- !,
	% A doccomment, not as an operator (JFMC) -- e.g., for end-of-file
	functor(Term, '\6\doccomment', 3),
	arg(1, Term, Col),
	arg(2, Term, Mark),
	arg(3, Term, Chars),
	( S0 = [] -> % introduce fake '.', for doccomments ending files
	    read_rest([.], 0, Term, Precedence, Answer, S)
	; read_rest(S0, 0, Term, Precedence, Answer, S)
	).
read('[', [']'|S1], Precedence, Answer, S) :- !,
	read(atom([]), S1, Precedence, Answer, S).
read('[', S1, Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_list(S2, RestArgs, S3), !,
	read_rest(S3, 0, [Arg1|RestArgs], Precedence, Answer, S).
read('(', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	read_rest(S3, 0, Term, Precedence, Answer, S).
read(' (', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	read_rest(S3, 0, Term, Precedence, Answer, S).
read('{', ['}'|S1], Precedence, Answer, S) :- !,
	read(atom({}), S1, Precedence, Answer, S).
read('{', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	read_curly_rest(Term, S2, CurlyTerm, S3),
	read_rest(S3, 0, CurlyTerm, Precedence, Answer, S).
read(string(List), S0, Precedence, Answer, S) :-
        current_prolog_flag(read_string_data_type, on),
        !,
        % string wrapped as a special term
	functor(String, '\6\string', 1),
        arg(1, String, List),
	read_rest(S0, 0, String, Precedence, Answer, S).
read(string(List), ['|','|'|S0], Precedence, Answer, S) :- !,
        % || allowed only if strings are read as lists
        current_prolog_flag(read_string_data_type, off), 
	!,
        dlist(List,Head,Tail),
        read(S0, 0, Tail, S1),
	read_rest(S1, 0, Head, Precedence, Answer, S).
read(string(List), S0, Precedence, Answer, S) :- !,
        % string read as list
	read_rest(S0, 0, List, Precedence, Answer, S).
read(badatom(_), S0, _, _, _) :- !,
        syntax_error(['invalid character code in constant sequence'], S0).
read('/* ...', S0, _, _, _) :- !,
        syntax_error(['non-terminated /* comment'], S0).
read(Token, S0, _, _, _) :-
	syntax_error([Token,' cannot start an expression'], S0).

% Parses (a) {'{' expr(1200) '}'} or (b) {'{' list-of-clauses '}'}
read_curly_rest(Term, S2, CurlyTerm, S) :-
	S2 = ['}'|S3], !,
	% We are in case (a)
	S = S3,
	CurlyTerm = {Term}.
read_curly_rest(Term, S2, CurlyTerm, S) :-
        current_prolog_flag(read_curly_blocks, on),
	S2 = ['.'|S3], !,
	% We are in case (b)
	read_curly_block(S3, Rest, S4),
	S = S4,
	CurlyTerm = '\6\curly_block'([Term|Rest]).
read_curly_rest(_Term, S2, _CurlyTerm, _S) :-
        ( current_prolog_flag(read_curly_blocks, on) ->
	    syntax_error(['}, ., or operator expected'], S2)
	; syntax_error(['} or operator expected'], S2)
	).

% Parses a list-of-clauses ended by '}'
read_curly_block(['}'|S1], TermList, S) :- !,
	TermList = [],
	S = S1.
read_curly_block(S1, TermList, S) :-
	read(S1, 1200, Term, S2),
	expect('.', S2, S3),
	TermList = [Term|Rest],
	read_curly_block(S3, Rest, S).

%   read_rest(+Tokens, +LPrec, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).  It
%   checks for following postfix or infix operators.  The tokens
%   Tokens-LeftOver are parsed and combined with Term into Answer;
%   LPrec and Prec are the lower and upper bounds for the precedence
%   of the topmost operator.

read_rest([atom(F)|S1], LPrec, Term, Precedence, Answer, S) :-
	% operator that is both infix and postfix
	current_infixop(F, L, O, R),
	Precedence >= O, LPrec =< L,
	current_postfixop(F, L1, O1),
	Precedence >= O1, LPrec =< L1,
	!,
	% TODO: can nondet code be avoided here? (jfmc)
	( % try infix
	  read(S1, R, Other, S2),
	  infix_functor(F, Expr),
	  arg(1, Expr, Term),
	  arg(2, Expr, Other),
	  read_rest(S2, O, Expr, Precedence, Answer, S)
	; % try postfix
	  functor(Expr, F, 1),
	  arg(1, Expr, Term),
	  read_rest(S1, O1, Expr, Precedence, Answer, S)
	).
read_rest([atom(F)|S1], LPrec, Term, Precedence, Answer, S) :-
	current_infixop(F, L, O, R),
	Precedence >= O, LPrec =< L, !,
	% read infix
	read(S1, R, Other, S2),
	infix_functor(F, Expr),
	arg(1, Expr, Term),
	arg(2, Expr, Other),
	read_rest(S2, O, Expr, Precedence, Answer, S).
read_rest([atom(F)|S1], LPrec, Term, Precedence, Answer, S) :-
	current_postfixop(F, L, O),
	Precedence >= O, LPrec =< L, !,
	% read postfix
	functor(Expr, F, 1),
	arg(1, Expr, Term),
	read_rest(S1, O, Expr, Precedence, Answer, S).
read_rest([doccomment(Col, Mark, Chars)|S1], LPrec, Term, Precedence, Answer, S) :-
	% A doccomment read as a postfix operator (JFMC)
	L = Precedence,
        O = Precedence,
	Precedence >= O, LPrec =< L,
	!,
	functor(Expr, '\6\doccomment', 5),
	arg(1, Expr, Col),
	arg(2, Expr, Mark),
	arg(3, Expr, postfix),
	arg(4, Expr, Chars),
	arg(5, Expr, Term),
	read_rest(S1, O, Expr, Precedence, Answer, S).
read_rest([F|S1], LPrec, Term, Precedence, Answer, S) :-
	( F = '[' -> Op = '[]' ; F = '{' -> Op = '{}' ),
	% jfmc: extended syntax for postfix blocks
        current_prolog_flag(read_postfix_blocks, on),
%	display(user_error, o(Op,L,O)), nl(user_error),
	current_postfixop(Op, L, O),
%	display(user_error, o2(Op,L,O)), nl(user_error),
	Precedence >= O, LPrec =< L, !,
	functor(Expr, '\6\postfix_block', 2),
	arg(1, Expr, Term),
	arg(2, Expr, Block),
	( F = '[', S1 = [']'|S2] ->
	    S3 = S2,
	    Block = '[]'
	; F = '{', S1 = ['}'|S2] ->
	    S3 = S2,
	    Block = '{}'
	; F = '[' ->
	    Block = [Arg1|RestArgs],
	    read(S1, 999, Arg1, S2),
	    read_list(S2, RestArgs, S3)
	; F = '{' ->
	    Block = CurlyTerm,
	    read(S1, 1200, Arg1, S2),
	    read_curly_rest(Arg1, S2, CurlyTerm, S3)
	),
	read_rest(S3, O, Expr, Precedence, Answer, S).
read_rest([','|S1], LPrec, Term, Precedence, Answer, S) :-
	Precedence >= 1000, LPrec < 1000, !,
	read(S1, 1000, Next, S2),
	read_rest(S2, 1000, (Term,Next), Precedence, Answer, S).
read_rest(['|'|S1], LPrec, Term, Precedence, Answer, S) :-
	current_infixop('|', L, O, R),
	Precedence >= O, LPrec =< L, !,
	read(S1, R, Next, S2),
	read_rest(S2, O, '|'(Term,Next), Precedence, Answer, S).
read_rest(S, _, Term, _, Term, S).

infix_functor('.', Expr) :-
	current_prolog_flag(read_infix_dot, on), !,
	functor(Expr, '\6\dot', 2).
infix_functor(F, Expr) :-
	functor(Expr, F, 2).

%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], TermList, S) :- !,
	TermList = [Term|Rest],
	read(S1, 999, Term, S2),
	read_args(S2, Rest, S).
read_args([')'|S1], TermList, S) :- !,
	TermList = [],
	S = S1.
read_args(S, _, _) :-
	syntax_error([', or ) expected in arguments'], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.

read_list([','|S1], TermList, S) :- !,
	TermList = [Term|Rest],
	read(S1, 999, Term, S2),
	read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- !,
	read(S1, 999, Rest, S2),
	expect(']', S2, S).
read_list([']'|S1], TermList, S) :- !,
	TermList = [],
	S = S1.
read_list(S, _, _) :-
	syntax_error([', | or ] expected in list'], S).

:- data 'syntax error'/2.

:- on_abort(retractall_fact('syntax error'(_,_))).

syntax_error(Message, List) :-
	length(List, Length),
	asserta_fact('syntax error'(Message,Length)), !,
	fail.

syntax_error_data(Tokens, Msg, ErrorLoc) :-
	the_syntax_error([], 1000000, Msg0, AfterError),
        tokens_items(Msg0, Msg),
	length(Tokens, Length),
	BeforeError is Length-AfterError,
        error_localization(Tokens, BeforeError, '', ErrorLoc).

the_syntax_error(Msg0, AfterError0, Msg, AfterError) :-
	current_fact('syntax error'(Msg1,AfterError1), Ptr), !,
	erase(Ptr),
	( AfterError0 > AfterError1 ->
	    the_syntax_error(Msg1, AfterError1, Msg, AfterError)
	;
	    the_syntax_error(Msg0, AfterError0, Msg, AfterError)
	).
the_syntax_error(Msg, AfterError, Msg, AfterError).

error_localization(L, 0, _, Msg) :- !,
        Msg = ['\n','** here **','\n'|Msg_],
        error_localization(L, -1, '', Msg_).
error_localization([T|Ts], BeforeError, Sep0, [Sep,I|Is]) :-
        separator(T, Sep0, Sep),
	token_item(T, I),
	Left is BeforeError-1,
	error_localization(Ts, Left, ' ', Is).
error_localization([], _, _, []).

tokens_items([], []).
tokens_items([T|Ts], [I|Is]) :-
        token_item(T, I),
        tokens_items(Ts, Is).

token_item(atom(X),    X    ) :- !.
token_item(number(X),  X    ) :- !.
token_item(var(_,X),   $$(X)) :- !.
token_item(badatom(_), '') :- !.  
token_item(unexpected(-1), '') :- !.
token_item(unexpected(X), $$(S)) :- !, S = [X].
token_item(string(X),  $$(S)) :- !, append([0'"|X], """", S).
token_item(X,          X).

separator('(' , _, '') :- !.
separator(' (', _, '') :- !.
separator(_, Sep, Sep).

% --

:- data second_prompt/1.

:- pred second_prompt(?Old, ?New) => atom * atom # "Changes the prompt
        (the @em{second prompt}, as opposed to the first one, used by
        the toplevel) used by @pred{read/2} and friends to @var{New},
        and returns the current one in @var{Old}.".

second_prompt('   ').

second_prompt(Old, New) :-
        (atom(New) ->
             retract_fact(second_prompt(Old)),
             asserta_fact(second_prompt(New))
        ; New = Old,
          current_fact(second_prompt(Old))
        ), !.

:- doc(bug, "The comma cannot be redefined as an operator, it is
   defined in any case as op(1000, xfy,[',']).").
