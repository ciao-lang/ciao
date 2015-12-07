:- module(autodoc_parse,
	[parse_docstring/3,
	 parse_docstring_loc/4],
	[dcg, assertions, regtypes, basicmodes]).

:- doc(module, "Parser for the lpdoc mark-up language (@regtype{docstring/1})").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- use_module(library(messages)).
:- use_module(library(lists), [append/3]).
:- use_module(library(errhandle), [error_protect/1]).

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_aux), [read_file/2]).
:- use_module(lpdoc(autodoc_errors)).
:- use_module(lpdoc(autodoc_index)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_filesystem), [find_file/2, find_source/4]).

:- use_module(lpdoc(comments), [docstring/1]).

%% ---------------------------------------------------------------------------

:- pred parse_docstring(DocSt, S, RS)
	: ( docstate(DocSt), docstring(S) )
	=> doctree(RS)

# "Parses a documentation string @var{S} into @var{RS}.".

parse_docstring(DocSt, S, RS) :-
	parse_docstring_(DocSt, _, noverb, S, RS).

parse_docstring_loc(DocSt, Loc, S, RS) :-
	parse_docstring_(DocSt, Loc, noverb, S, RS).

% Values for @var{Verb}:
%   noverb:   normal text
%   verb:     verbatim text
%   mathtext: math text

parse_docstring_(DocSt, Loc, Verb, S, R) :-
	intercept(
	    parse_docstring__1(DocSt, Verb, S, R),
	    parse_error(ErrorType, Args),
	    handle_parse_error(ErrorType, Loc, Args)
	).

handle_parse_error(Type, Loc, Args) :-
	error_text(Type, MType, Format),
	show_message(MType, Loc, Format, Args),
	!.

%% ---------------------------------------------------------------------------
% A push down automaton that parses the docstring

parse_docstring__1(DocSt, Verb, S, RS2) :-
	transition(read, DocSt, empty, Verb, RS2, S, []),
	!.
parse_docstring__1(DocSt, _Verb, S, RS2) :-
	docst_backend(DocSt, Backend),
	empty_doctree(RS2),
	send_signal(parse_error(rewrite, [Backend, S])).

% TODO: move to autodoc_docstring
environment_mode('verbatim', verb).
environment_mode('displaymath', mathtext).

transition(stop, _DocSt, _Stack0, _Verb, []) --> [].
transition(State0, DocSt, Stack0, Verb0, Rs) -->
	( { State0 = push_env(Env) } ->
	    { environment_mode(Env, Verb1) -> true
	    ; Verb1 = noverb
	    },
	    % (ContState in envstack is filled later)
	    { Stack1 = envstack(Env,EnvBody,_,Stack0,Verb0,Rs) },
	    transition(pickspace, DocSt, Stack1, Verb1, EnvBody)
	; { State0 = pop_env } ->
	    { Rs = [] },
	    { Stack0 = envstack(Env,EnvBody,ContState,PrevStack,PrevVerb,PrevRs) },
	    { build_env(Env, EnvBody, EnvR) },
            { PrevRs = [EnvR|Rs0] },
	    transition(ContState, DocSt, PrevStack, PrevVerb, Rs0)
	; { State0 = gotinput(Token) } ->
	    { input_transition(Token, Stack0, State1, Rs, Rs1) },
	    transition(State1, DocSt, Stack0, Verb0, Rs1)
	; { State0 = read } ->
	    get_token(DocSt, Verb0, Token),
	    transition(gotinput(Token), DocSt, Stack0, Verb0, Rs)
	; { State0 = pickspace } ->
	    pick_space(Verb0, Rs, Rs1),
	    transition(read, DocSt, Stack0, Verb0, Rs1)
	; { State0 = ignoreblank } ->
	    ( pick_blank(Blank), { Blank = normal } -> [] ; [] ), % ignore spaces
	    transition(read, DocSt, Stack0, Verb0, Rs)
	).

input_transition(Token, Stack, State, Rs, Rs0) :-
	( Token = comment(_) ->
	    State = ignoreblank, Rs = Rs0
	; section_level(Token, T, Lt) ->
	    ( Stack = envstack(Env,_,ContState,_,_,_),
	      Env = section_(Le, _), Lt =< Le ->
	        % a section of lower or equal level than the current
		% section environment starts a new section environment: put
		% token back in the queue and close the environment
	        ContState = gotinput(Token),
		State = pop_env, Rs = Rs0
	    ; % otherwise, begin a section
	      State = push_env(section_(Lt, T)), Rs = Rs0
	    )
	; end_env(Token, EnvE) ->
	    % end the environment
	    ( Stack = empty ->
	        send_signal(parse_error(nobegin, [EnvE]))
	    ; Stack = envstack(Env,_,ContState,_,_,_),
	      ( Env = EnvE ->
	          true
	      ; send_signal(parse_error(wrongend, [Env, EnvE]))
	      ),
	      ContState = pickspace
	    ),
	    State = pop_env, Rs = Rs0
	; Token = end_of_input_ ->
	    % end of input
	    ( Stack = empty ->
	        State = stop, Rs = Rs0
	    ; Stack = envstack(Env,_,ContState,_,_,_),
	      ( nochars_env(Env) ->
	          true
	      ; send_signal(parse_error(noend, [Env]))
	      ),
	      % (we will repeat end_of_input_ until Stack is empty)
	      ContState = gotinput(Token),
	      State = pop_env, Rs = Rs0
	    )
	; begin_env(Token, EnvB) -> State = push_env(EnvB), Rs = Rs0
	; Token = err(Error) -> send_signal(Error), State = read, Rs = Rs0
	; string_token(Token) -> State = read, Rs = [Token|Rs0]
	; % Command tokens
	  State = pickspace,
	  normalize_token(Token, Token2),
	  Rs = [Token2|Rs0]
        ).

string_token(string_esc(_)).
string_token(string_verb(_)).

normalize_token(Token0, Token) :-
	Token0 =.. [Cmd, Body],
	is_index_cmd(Cmd),
	!,
	normalize_index_cmd(Cmd, Body, Token).
normalize_token(Token, Token).

% ---------------------------------------------------------------------------

begin_env(begin(EnvStr), Env) :- atom_codes(Env, EnvStr).

end_env(end(EnvStr), Env) :- atom_codes(Env, EnvStr).

% Enviroments that finish abruptly (with no @end)
nochars_env(section_(_, _)).

build_env(section_(Level, Title), EnvBody, EnvR) :- !,
	EnvR = section_env([level(Level)], local_label(_), Title, EnvBody).
build_env(Env, EnvBody, EnvR) :-
	EnvR = env_(Env, EnvBody).

% Obtain the name and level of a section command
section_level(section(T), T, 2).
section_level(subsection(T), T, 3).
section_level(subsubsection(T), T, 4).

%% ---------------------------------------------------------------------------

envmode_parse_paragraph_break(noverb).

% Recognize just a token (gives a command, string, end_of_input_, or parsing error)
get_token(_DocSt, _Verb, Token, [], []) :- !,
	Token = end_of_input_.
get_token(_DocSt, Verb, Token) --> { Verb = mathtext },
	% Parse a mathtext (until @end{...})
	pick_until_endcmd(Text),
	{ \+ Text = [] },
	!,
	{ Token = raw(Text) }.
get_token(DocSt, Verb, Token) -->
	% Parse a command
	start,
	command_body(Struct),
	!,
	{ handle_command(Struct, DocSt, Verb, Token) }.
get_token(_DocSt, Verb, Token) -->
	% Parse a paragraph break
	{ envmode_parse_paragraph_break(Verb) },
	pick_blank(Blank),
	{ Blank = paragraph },
	!,
	{ Token = p("") }.
get_token(_DocSt, Verb, Token) -->
	% Parse normal text
	normal_str(String, Verb), { String \== [] },
	!,
	{ Verb = verb ->
	    Token = string_verb(String)
	; Token = string_esc(String)
	}.
get_token(_DocSt, _Verb, Token, S, S0) :-
	% Else error
	S = [_|S0],
	Token = err(parse_error(docstring, [S])).

normal_str([X|Xs], Verb) -->
	( { Verb = verb } ->
	    normal_char_verb(X)
	; pick_blank(Blank) ->
	    { Blank = normal },
	    % only normal blanks are accepted
	    { X = (0' ) }
	; normal_char(X)
	),
	!,
	normal_str(Xs, Verb).
normal_str([], _Verb) --> [].

% Parses a single leading space. This is useful for the raw_nleb
% command for the texinfo backend.
% TODO: Can this be avoided? --JF
pick_space(verb, R, R0) --> !,
	% In verbatim blanks are left untreated
	( space -> { R = [string_esc(" ")|R0] }
	; { R = R0 }
	).
pick_space(noverb, R, R0) --> !,
	( pick_blank(Blank) ->
	    { R = [string_esc(" ")|R1] },
	    { Blank = paragraph -> R1 = [p("")|R0]
	    ; Blank = normal -> R1 = R0
	    }
	; { R = R0 }
	).
pick_space(mathtext, R, R) --> !.

pick_blank(Blank) -->
	( spaces_or_tabs, newline ->
	    spaces_or_tabs,
	    ( newline ->
	        spaces_or_tabs,
	        % Two newlines generates a paragraph break
	        { Blank = paragraph }
	    ; % One newline is just a blank
	      { Blank = normal }
            )
	; space_or_tab ->
	    spaces_or_tabs,
	    % Several blanks are just collapsed
	    { Blank = normal }
	).

pick_until_endcmd([], Ys, Ys) :- Ys = "@end{"||_, !.
pick_until_endcmd([X|Xs]) --> [X], !, pick_until_endcmd(Xs).
pick_until_endcmd([]) --> !.

command_body('{') --> open, !.
command_body('}') --> close, !.
command_body('@') --> start, !.
command_body(comment([])) -->
	"comment{", % a commented text in documentation (ignored)
	!,
	balanced_braces(1, _).
command_body(Struct) -->
	command_chars1(CommandS),
	{ atom_codes(Command, CommandS) },
	( blank, %space,
	  % simple commands which end in space
	    {BodyList = [[]]}
	; open,
	  % TODO: obtain this from cmd_type!
	  % commands with several arguments 
	  % (currently cannot contain other commands)
	    {( Command = 'href'
	     ; Command = 'email'
	     ; Command = 'image'
	     ; Command = 'bibitem'
	     ; Command = 'pbundle_download'
	     ; Command = 'pbundle_href'
	     )},
%%	    command_args(BodyList)
	    command_balanced_args(BodyList)
	; open,
	  % TODO: obtain this from cmd_type!
	  % commands with several arguments 
	  % (containing balanced text)
	    {( Command = 'defmathcmd'
	     )},
	    command_balanced_args(BodyList)
	; open,
	  % normal commands: look for closing brace, enter recursively
	  balanced_braces(1, CommandBody),
	  % TODO: Recursion (handle_cmd_args) should really be done
	  %       here instead of individually
	  {BodyList = [CommandBody]}
	),
	{ Struct =.. [Command|BodyList] }.

% (at least 1 character)
command_chars1([C|Cs]) -->
	command_char(C),
	command_chars(Cs).

command_chars([C|Cs]) -->
	command_char(C),
	command_chars(Cs).
command_chars([]) -->
	[].

%% command_args([Arg|RArgs]) -->
%% 	all_chars(Arg),
%% 	close,
%% %	( spaces, open ->
%% 	( blanks, open ->
%% 	    command_args(RArgs)
%% 	; { RArgs = [] }
%% 	).

% like command_args, but looks for balanced text
command_balanced_args([Arg|RArgs]) -->
	balanced_braces(1, Arg),
%	( spaces, open ->
	( blanks, open ->
	    command_balanced_args(RArgs)
	; { RArgs = [] }
	).

all_chars([0'@, 0'{|Cs]) --> start, open, !,
	all_chars(Cs).
all_chars([0'@, 0'}|Cs]) --> start, close, !,
	all_chars(Cs).
all_chars([0'@, 0'@|Cs]) --> start, start, !,
	all_chars(Cs).
all_chars([C|Cs]) --> normal_char(C), !, all_chars(Cs).
all_chars([]) --> [].

spaces_or_tabs --> space_or_tab, spaces_or_tabs.
spaces_or_tabs --> [].

space_or_tab --> space.
space_or_tab --> tabchar.

spaces --> space, spaces.
spaces --> [].

blank --> space.
blank --> newline.
blank --> tabchar.

blanks --> blank, blanks.
blanks --> [].

normal_char(X) --> [X], {X \== 0'@, X \== 0'{, X \== 0'}}.
normal_char_verb(X) --> [X], {X \== 0'@}.
command_char(X) --> [X], {X \== 0'@, X \== 0'{, X \== 0'}, X \== 0' ,
	    X \== 0'\n, X \== 0'\t}.

start --> [0'@].
open --> [0'{].
close --> [0'}].
space --> [0' ].
tabchar --> [0'\t].
newline --> [0'\n].

parse_predname(Functor, Arity, PredNameS) :-
	predname_g(FunctorS, ArityS, PredNameS, []),
	!,
	atom_codes(Functor, FunctorS),
	number_codes(Arity, ArityS).
parse_predname(0, 0, PredNameS) :-
	error_message("illegal predicate name ~s in code inclusion command",
	    [PredNameS]).

% TODO: incomplete parsing
predname_g(FunctorS, ArityS) -->
	all_chars(FA),
	{ append(FunctorS, "/"||ArityS, FA) -> true }.
%	all_chars(FunctorS),
%	"/",
%	all_chars(ArityS).

balanced_braces(1, []) -->
	"}",
	!.
balanced_braces(N, [0'@, 0'@|Rest]) -->
	"@@",
	!,
	balanced_braces(N, Rest).
balanced_braces(N, [0'@, 0'{|Rest]) -->
	"@{",
	!,
	balanced_braces(N, Rest).
balanced_braces(N, [0'@, 0'}|Rest]) -->
	"@}",
	!,
	balanced_braces(N, Rest).
balanced_braces(N, [0'{|Rest]) -->
	"{",
	!,
	{N1 is N+1},
	balanced_braces(N1, Rest).
balanced_braces(N, [0'}|Rest]) -->
	"}",
	!,
	{N1 is N-1},
	balanced_braces(N1, Rest).
balanced_braces(N, [X|Rest]) -->
	[X],
	balanced_braces(N, Rest).

%% ---------------------------------------------------------------------------

handle_command(Command, DocSt, Verb, NewCommand):-
	functor(Command, Cmd, A),
	functor(BT, Cmd, A),
	( cmd_type(BT) ->
	    Command =.. [_|Xs],
	    BT =.. [_|Ts],
	    parse_cmd_args(Ts, Xs, DocSt, Ys),
	    B1 =.. [Cmd|Ys],
	    handle_incl_command(B1, DocSt, Verb, NewCommand)
	; Command =.. [CommandName,Body],
	  NewCommand = err(parse_error(unrecognizedcmd, [CommandName, Body]))
	),
	!.
handle_command(Struct, _DocSt, _Verb, R) :-
	functor(Struct, F, _),
	R = err(parse_error(handle, [F])).

parse_cmd_args([], [], _, []).
parse_cmd_args([T|Ts], [X|Xs], DocSt, [Y|Ys]) :-
	( T = d -> parse_docstring(DocSt, X, Y)
	; T = p -> parse_predname(F, A, X), Y = F/A
	; T = s -> Y = X
	; fail % unknown type
	),
	parse_cmd_args(Ts, Xs, DocSt, Ys).

% Handle commands that include more text to be parsed
handle_incl_command(include(FileS), DocSt, Verb, RContent) :-
	!,
	atom_codes(RelFile, FileS),
	( error_protect(find_file(RelFile, File)),
	  read_file(File, Content) ->
	    %% These are not turned off for now...
	    docst_message("{-> Including file ~w in documentation string", [File], DocSt),
	    parse_docstring__1(DocSt, Verb, Content, RContent),
	    docst_message("}", DocSt)
	; RContent = err(parse_error(cannot_read, [RelFile]))
	).
handle_incl_command(includeverbatim(FileS), DocSt, _Verb, RContent) :-
	!,
	atom_codes(RelFile, FileS),
	( ( error_protect(find_source(RelFile, _, _, File))
	  ; error_protect(find_file(RelFile, File))
	  ),
	  read_file(File, Content) -> % TODO: detect language and do syntax highlight
	    docst_message("{-> Including file ~w verbatim in documentation string", [File], DocSt),
	    docst_message("}", DocSt),
	    % TODO: why not string_verb?
	    RContent = string_esc(Content)
	; RContent = err(parse_error(cannot_read, [RelFile]))
	).
% TODO: Treat this command here or in autodoc? --JF
%       It adds a dependency to clause_read.
handle_incl_command(includefact(Pred), DocSt, Verb, RContent) :-
	Pred = Functor/Arity,
	!,
	( Functor \== 0,
	  functor(Pattern, Functor, Arity),
	  clause_read(_, Pattern, true, _, _, _, _) ->
	    docst_message("-> Including fact ~w in documentation string", [Functor], DocSt),
	    ( Arity = 1 ->
		true
	    ; send_signal(parse_error(aritynot1, []))
	    ),
	    arg(1, Pattern, Content),
	    parse_docstring__1(DocSt, Verb, Content, RContent)
	; RContent = err(parse_error(tryinclude, [Pred]))
	).
handle_incl_command(includedef(Pred), DocSt, _Verb, RContent) :-
	Pred = Functor/Arity,
	!,
	( portray_to_string(Functor, Arity, Content) ->
	    docst_message("-> Including code for ~w in documentation string", [Functor/Arity], DocSt),
	    % TODO: here type is not 'normal' but 'verb'
	    escape_string(normal, Content, DocSt, NContent),
	    build_env('verbatim', [raw_string(NContent)], RContent)
	; RContent = err(parse_error(tryinclude, [Pred]))
        ).
%% Rest of commands
handle_incl_command(Struct, _DocSt, _Verb, XNewComm) :-
	XNewComm = Struct.

% TODO: See includefact above --JF
:- use_module(library(assertions/assrt_lib), [clause_read/7]).

% ---------------------------------------------------------------------------
% Auxiliary predicate to output to string
% TODO: Find a better way to implement it

:- use_module(library(dec10_io)).
:- use_module(library(pretty_print), [pretty_print/3]).
:- use_module(library(vndict),       [complete_dict/3, varnamesl2dict/2]).

portray_to_string(Functor, Arity, Content) :-
	Functor \== 0,
	functor(Pattern, Functor, Arity),
	copy_term(Pattern, TmpPattern),
	clause_read(_, TmpPattern, _, _, _, _, _),
	!,
	telling(Old),
	mktemp(autodocXXXXXX, Tmp),
	tell(Tmp),
	current_prolog_flag(write_strings, X),
	set_prolog_flag(write_strings, on),
	( clause_read(_, Pattern, Body, Dict, _, _, _),
	    Clause = clause(Pattern, Body),
	    varnamesl2dict(Dict, ICiaoDict),
	    complete_dict(ICiaoDict, Clause, CiaoDict),
	    pretty_print(Clause, [nl(no)], CiaoDict),
	    fail
	; true
	),
	set_prolog_flag(write_strings, X),
	told,
	tell(Old),
	read_file(Tmp, Content),
	delete_file(Tmp).

:- use_module(lpdoc(autodoc_aux), [read_file/2]).
:- use_module(library(system), [delete_file/1, mktemp/2]).
