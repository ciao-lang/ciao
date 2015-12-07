:- module(read_from_string,
	[ read_from_string/2,
          read_from_string/3,
          read_from_string_opts/4,
	  read_from_string_atmvars/2,
          read_from_string_atmvars/3,
	  read_from_atom_atmvars/2,
	  read_from_atom/2 % wrong, see code
	],
	[ assertions,
	  basicmodes
	]).

:- doc(title,"Reading terms from strings").

:- doc(author,"Francisco Bueno").
:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Jose F. Morales").

:- doc(module, "This module implements predicates for reading
   (parsing) terms from strings or atom codes.

@begin{alert}
  Use with extreme care. This is a quick and incomplete implementation.
@end{alert}
").

:- doc(bug, "All predicates except @pred{read_from_atom/2} were
   implemented as a quick hack initially written mainly for parsing
   daVinci's messages. The good implementation should: a) create a
   read stream from a string, and b) call the standard reader.").

:- use_module(library(dict)).
:- use_module(library(read)).
:- use_module(library(operators)).
:- use_module(library(port_reify)).

% ---------------------------------------------------------------------------
% The safe but slow version

:- doc(bug, "@pred{read_from_atom/2} is implemented using
   @pred{pipe/2}, which has deadlock problems (IO is synchronous and
   pipe buffer size is limited)").

:- pred read_from_atom(+Atom,-Term) # "Read the term @var{Term} from the
   atom codes in @var{Atom}.".

read_from_atom(Atom, Term) :-
	pipe(ReadFrom, WriteTo),
	display(WriteTo, Atom),
	display(WriteTo, '.'),
	close(WriteTo),
	once_port_reify(read_term(ReadFrom, Term, []), ReadR),
	close(ReadFrom),
	port_call(ReadR).

% ---------------------------------------------------------------------------
% The quick but incomplete versions

:- pred read_from_string_atmvars(+String,-Term) # "Read a term
   @var{Term} from @var{String}. It ignores the unparsed rest of the
   string (see @pred{read_from_string_atmvars/3}).".

:- test read_from_string_atmvars(A,T) : ( A = "a" ) => ( T = a ).
:- test read_from_string_atmvars(A,T) : ( A = "1" ) => ( T = 1 ).
:- test read_from_string_atmvars(A,T) : ( A = "A" ) => ( T = 'A' ).
:- test read_from_string_atmvars(A,T) : ( A = "f(a)" ) => ( T = f(a) ).
:- test read_from_string_atmvars(A,T) : ( A = "f/2" ) => ( T = f/2 ).

read_from_string_atmvars(String,Term):-
	% TODO: The rest is ignored, which may be incorrect in some
	%   cases. See clients of this predicate to implement the
	%   desired behaviour.
	% TODO: use read and unify variable names later?
	readS_internal(String,Term, _, atmvars), !.

:- pred read_from_string_atmvars(+String, -Term, ?Rest)
      # "Read a term @var{Term} from @var{String} up to @var{Rest}
         (which is the non-parsed rest of the list). Unquoted
         uppercase identifiers are read as atoms instead of variables
         (thus, the read term is always ground).".

:- test read_from_string_atmvars(A,T,R) :
	( A = "f(a)" ) => ( T = f(a), R = "" ).
:- test read_from_string_atmvars(A,T,R) :
	( A = "f(a) foo " ) => ( T = f(a), R = " foo " ).

read_from_string_atmvars(String, Term, Rest) :-
	% TODO: use read and unify variable names later?
	readS_internal(String, Term, Rest, atmvars).

:- pred read_from_atom_atmvars(+Atom,-Term) # "Like
   @pred{read_from_string_atmvars/2}, but reads the term @var{Term}
   from the atom codes in @var{Atom}.".

:- test read_from_atom_atmvars(A,T) : ( A = a ) => ( T = a ).
:- test read_from_atom_atmvars(A,T) : ( A = '1' ) => ( T = 1 ).
:- test read_from_atom_atmvars(A,T) : ( A = 'A' ) => ( T = 'A' ).
:- test read_from_atom_atmvars(A,T) : ( A = 'f(a)' ) => ( T = f(a) ).
:- test read_from_atom_atmvars(A,T) : ( A = 'f/2' ) => ( T = f/2 ).

read_from_atom_atmvars(Atom,Term):-
	atom_codes(Atom,String),
	% TODO: The rest is ignored, which may be incorrect in some
	%   cases. See clients of this predicate to implement the
	%   desired behaviour.
	% TODO: use read and unify variable names later?
	readS_internal(String,Term, _, atmvars), !.

:- pred read_from_string(+String, -Term) 
 # "Read a term @var{Term} from @var{String}.".

read_from_string(String,Term) :-
	read_from_string_opts(String,Term,[],variable_names(_)).

:- pred read_from_string(+String, -Term, ?Rest) 
 # "Read a term @var{Term} from @var{String} up to @var{Rest} (which
    is the non-parsed rest of the list).".

read_from_string(String,Term,Rest) :-
	read_from_string_opts(String,Term,Rest,variable_names(_)).

:- pred read_from_string_opts(+String, -Term, ?Rest, +Opts)

 # "@var{String} is parsed into @var{Term} up to @var{Rest} (which is
    the non-parsed rest of the list). The options in @var{Opts} can
    be:

    @begin{description}
    @item{@tt{variable_names(Ns)}} Read variable names in @var{Ns}.
    @end{description}".

read_from_string_opts(String,Term,Rest,Opts) :-
	% Get options
	( var(Opts) -> true
	; Opts = variable_names(N) ->
	    Ns = variable_names(N),
	    Opts0 = vardict(VarDict)
	; Opts = Opts0
	),
	% Read
	readS_internal(String,Term,Rest,Opts0),
	% Extract variable names (if required)
	extract_names(Ns, _, VarDict).

readS_internal([],'',[], _).
readS_internal([C|String0],Term,String,Opts):-
	readS_opts0(C,String0,Term,String,Opts).

readS_opts0(C,String0,Term,String,Opts):- code_class(C, 0), !, % space
        readS_internal(String0,Term,String,Opts).
readS_opts0(0'[,String0,Term,String,Opts):- !, % list
	readS_args0(String0,Term,[0']|String],Opts).
readS_opts0(0'(,String0,Term,String,Opts):- !, % parenthesis
	readS_internal(String0,Term,[0')|String],Opts).
readS_opts0(0'",String0,Term,String,_):- !, % string
	readS_string(String0,Str,String),
	atom_codes(Term,Str).
readS_opts0(0') , _, _, _, _):- !, fail.
readS_opts0(0'] , _, _, _, _):- !, fail.
readS_opts0(0', , _, _, _, _):- !, fail.
readS_opts0(C0,String0,Term,String,Opts):- % struct, atom, or var
        readS_symbol_or_name([C0|String0],NameStr,String1),
	!,
	% TODO: Incorrect treatment of variables
	( NameStr = [U|_],
	  code_class(U, 2) % uppercase or _
          ->
	    % A variable
	    ( Opts = atmvars -> % read as atom
	        name(Term0,NameStr)
	    ; Opts = vardict(Dict) -> % read as var
	        ( NameStr = "_" -> % anonymous variable
		    true
		; % lookup/enter in dictionary
		  dic_lookup(Dict, NameStr, Node),
		  check_singleton(Node, Term0)
		)
	    ; throw(bad_opts(Opts))
	    )
	; name(Term0, NameStr)
	),
	% atom_codes(Name,NameStr),
        % We use name instead because by default we 
        % want '1' to be converted to a number.
	( String1 = [0'(|_] ->
	  % Term0(...)
	    readS_args(String1,Args,String1a,Opts),
	    ( var(Term0) ->
	        Term0a =.. [call, Term0|Args]
	    ; Term0a =.. [Term0|Args]
	    )
	; Term0a = Term0,
	  String1a = String1
	),
	( skip_blanks(String1a, String1b),
	  readS_symbol_or_name(String1b,Name2Str,String2),
	  name(Name2, Name2Str),
	  current_infixop(Name2, _, _, _) ->
	    readS_internal(String2,RestTerm,String,Opts),
	    Term =.. [Name2, Term0a, RestTerm] 
	; atom(Term0a), current_prefixop(Term0a, _, _),
	  term_follows(String1a) ->
	    % TODO: precedence ignored
	    % Term0 ... (prefix op)
	    readS_internal(String1a,RestTerm,String,Opts),
	    Term =.. [Term0a, RestTerm]
	; % Term0
	  Term = Term0a,
	  String = String1a
        ).
readS_opts0(C0,String0,Term,String,_Opts):- % (nothing?)
	Term = '',
	String = [C0|String0].

term_follows(String1) :-
	skip_blanks(String1, String2),
	String2 = [C|_],
	\+ not_term_start(C).

not_term_start(0',).
not_term_start(0')).
not_term_start(0']).

name_char(C) :-
	code_class(C, Ct),
	( Ct = 1 % lowercase
	; Ct = 2 % uppercase or _
	; Ct = 3 % digits
	).

solo_char(C) :-
	code_class(C, Ct),
	( Ct = 0 % blanks
	; Ct = 4 % symbols
	; Ct = 5 % solo
	).

readS_symbol_or_name(String0,NameStr,String1) :-
        ( readS_name(String0,NameStr,String1)
	; readS_symbol(String0,NameStr,String1)
	),
	\+ NameStr = "",
	!.

% Names (including those for variables)
readS_name([], [], []).
readS_name([C|String],Chars,String1):-
	readS_name_(C,String,Chars,String1).

readS_name_(C, String, [C|Chars], String1) :- name_char(C), !,
        readS_name(String,Chars,String1).
readS_name_(C, String, [], [C|String]).

% Symbols (atoms which need no quoting)
readS_symbol([], [], []).
readS_symbol([C|String],Chars,String1):-
	readS_symbol_(C,String,Chars,String1).

readS_symbol_(C, String, [C|Chars], String1) :- code_class(C, 4), !, % symbol
        readS_symbol(String,Chars,String1).
readS_symbol_(C, String, [], [C|String]).

% Skip blanks
skip_blanks([C|String], String1) :- code_class(C, 0), !,
        skip_blanks(String, String1).
skip_blanks(String, String).

%readS_args([],[],[]).
readS_args([0'(|String0],Args,String,Opts):- !,
	readS_args0(String0,Args,[0')|String],Opts).
readS_args(String,[],String,_).

readS_args0(String0,[Arg|Args],String,Opts):-
	readS_internal(String0,Arg,String1,Opts), !,
	readS_args1(String1,Args,String,Opts).
readS_args0(String, [], String,_).

readS_args1([0'\s|String0],Args,String,Opts) :- !,
        readS_args1(String0,Args,String,Opts).
readS_args1([0', |String0],[Arg|Args],String,Opts):- !,
	readS_internal(String0,Arg,String1,Opts),
	readS_args1(String1,Args,String,Opts).
readS_args1(String,[],String,_).

readS_string([],[],[]).
readS_string([C|String],List,String1):-
	readS_string0(C,String,List,String1).

readS_string0(0'",String,[],String):- !.
readS_string0(C,String,[C|List],String1):-
	readS_string(String,List,String1).

% ---------------------------------------------------------------------------
% -- (Copied from read.pl and tokenize.pl) --
%
% TODO: Although this code should be shared, please do not invest any
%   time on it. A real strings as streams implementation will be much
%   useful.

check_singleton(Node, Var) :-
        var(Node), !, Node = [Var|_].
check_singleton([Var|[]], Var). % The [] marks it is not singleton

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
