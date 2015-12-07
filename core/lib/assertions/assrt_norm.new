:- module(assrt_norm,
	[   normalize_assertions/2,
 	    normalize_assertions/3,
	    normalize_assertion/9,

	    norm_goal_prop/3, 
	    denorm_goal_prop/3,

	    print_assertions/1,
	    print_unformatted_assertions/1
	],
	[ assertions, basicmodes, regtypes
	]).

:- reexport(library('assertions/assrt_db'),
	[ assertion_read/9,
	  assertion_body/7
	]).

:- use_module(library('assertions/assrt_write'),[write_assertion/6]).
:- use_module(library('assertions/assertions_props')).
:- use_module(library('assertions/c_itf_props')).
:- use_module(library('compiler/c_itf')).
:- use_module(library(lists),[append/3]).
:- use_module(library(messages)).

%% ---------------------------------------------------------------------------
:- pred normalize_assertions(M,Base) : moddesc * atm
	# "Normalizes assertions of module @var{M}, file @var{Base},
	   in clauses of @pred{clause_of/7} (see @file{c_itf})
           leaving them as @pred{assertion_read/9} facts.".
%% ---------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings,off).

normalize_assertions(M,Base):-
	normalize_assertions(M,Base,[]).

%% ---------------------------------------------------------------------------
:- pred normalize_assertions(moddesc,atm,Opts)
	# "Same as @pred{normalize_assertions/1} except that it passes on
           the options in @var{Opts}.".
%% ---------------------------------------------------------------------------
%%    This predicate calls
%%    normalize_assertions_pass_one/1 and then
%%    normalize_assertions_pass_two/1, thus leaving all assertions
%%    in the database in fully normalized form

normalize_assertions(M,Base,Opts):-
	normalize_assertions_pass_one(M,Base),
	normalize_assertions_pass_two_opts(M,Opts).

:- pop_prolog_flag(multi_arity_warnings).

%% ---------------------------------------------------------------------------
:- pred normalize_assertion(M,Assrt,PD,AStatus,AType,NABody,S,LB,LE) 
	:: moddesc * term * term * assrt_status * assrt_type * assrt_body
                   * atm * int * int
        # "Normalizes one assertion (see @pred{assertion_read/9} and
	   @pred{normalize_assertions/1}.".
%% ---------------------------------------------------------------------------

normalize_assertion(M,Assrt,PD,AStatus,AType,NABody,S,LB,LE) :-
	normalize_one_assertion_pass_one(
                  M,Assrt,PD,AStatus,AType,ABody,S,LB,LE),
	% modedefs have already been transformed -- leave as is 
        (  AType = modedef 
	-> NABody = ABody
	;  normalize_properties(ABody,NABody,M,_Functor,[],AType,S,LB,LE) ).

%% ---------------------------------------------------------------------------
:- pred normalize_assertions_debug(M) :: moddesc # "Same as
   @pred{normalize_assertions/0} but it reports on the normalization
   process by printing all assertions after normalization passes one
   and two. If @var{M} is instantiated only information on module
   @var{M} is printed. Otherwise information for all modules is
   printed.".
%% ---------------------------------------------------------------------------

normalize_assertions_debug(M,Base) :-
	normalize_assertions_debug_opts(M,Base,[]).

normalize_assertions_debug_opts(M,Base,Opts) :-
	prolog_flag(write_strings, Old, on),
	io_aux:message(['{Normalizing assertions in ',M,' (pass one)']),
	normalize_assertions_pass_one(M,Base),
	io_aux:message('}'),
	print_unformatted_assertions(_M),
	io_aux:message(['{Normalizing assertions in ',M,' (pass two)']),
	normalize_assertions_pass_two_opts(M,Opts),
	io_aux:message('}'),
        current_output(CI),
        set_output(user_error),
	print_unformatted_assertions(_M),
        set_output(CI),
	set_prolog_flag(write_strings, Old).


%% ---------------------------------------------------------------------------
:- pred print_assertions(M) :: moddesc # "Prints the assertions stored
   in the database as @pred{assertion_read/9} facts, performing some
   pretty-printing and simplification (e.g., eliminating empty
   fields). If @var{M} is instantiated, only information on module
   @var{M} is printed. Otherwise information for all modules is
   printed.".
%% ---------------------------------------------------------------------------

print_assertions(M) :-
	io_aux:message('{Printing assertions read '),
	assertion_read(PD,M,Status,Type,Body,Dict,_S,_LB,_LE),
	%% Using now version in library(assrt_write)
	write_assertion(PD,Status,Type,Body,Dict,status),
	fail.
print_assertions(_M) :-
	io_aux:message('}').

%% ---------------------------------------------------------------------------
:- pred print_unformatted_assertions(M) :: moddesc # "Prints the
   assertions stored in the database as @pred{assertion_read/9} facts,
   in a raw format (no attempt is made to simplify the assertions). If
   @var{M} is instantiated, only information on module @var{M} is
   printed. Otherwise information for all modules is printed.".
%% ---------------------------------------------------------------------------

print_unformatted_assertions(M) :-
	io_aux:message('{Printing assertions read'),
	assertion_read(PD,M,Status,Type,Body,Dict,_S,_LB,_LE),
	%% Using now version in library(assrt_write)
	local_write_assertion(PD,Status,Type,Body,Dict,status,M),
	fail.
print_unformatted_assertions(_M) :-
	io_aux:message('}').

local_write_assertion(PD,Status,Type,Body,_Dict,_Flag,M) :-
	assertion_body(PD,DP,CP,AP,GP,CO,Body),
	io_aux:message(['(in module ',M,':)']),
	io_aux:message([':- ',Status,' ',Type,' ',PD,
                       ' :: ',DP,' : ',CP,' => ',AP,' + ',GP,' # ',CO]).

%% ---------------------------------------------------------------------------
:- pred normalize_assertions_pass_one(moddesc) # "For each assertion
   normalizes the assertion body (but not the properties inside or
   those properties and modes which appear in the head). The predicate
   descriptor (both that in the assertion body and the first argument
   of @pred{assertion_read/9}) is partially normalized in that
   @tt{functor/arity} formats are expanded into terms, but modes and
   properties in arguments are left for the second pass (i.e.,
   @tt{p(X,+)} may be present, but not @tt{p/2}). However, if the
   assertion is a @tt{modedef} then it is fully normalized at this
   time (including body properties, which are normalized but not
   checked) so that @pred{normalize_assertions_pass_two/1} can use it
   later while normalizing other assertions. The (partially)
   normalized assertion is left asserted as an @pred{assertion_read/9}
   fact.".
%% ---------------------------------------------------------------------------

normalize_assertions_pass_one(M,Base) :-
%	defines_module(Base,M),
	(  %% Normalize all assertions in this module
	   clause_of(Base,1,Assrt,Dict,S,LB,LE), 
	   normalize_one_assertion_pass_one(
		M,Assrt,PD,AStatus,AType,NNAss,S,LB,LE),
	   assertz_fact(assertion_read(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)),
 	   fail
	;  true ).
	
normalize_one_assertion_pass_one(M,Assrt,PD,AStatus,AType,NNAss,S,LB,LE) :-
	normalize_if_assertion_body(M,Assrt,AStatus,AType,NAss,S,LB,LE),
	assertion_body(PD,_DP,_CP,_SP,_GP,_CO,NAss),
	(  AType = modedef
	-> %% modedef body props have to be normalized at this time!
	   normalize_modedef_properties(NAss,NNAss,M,S,LB,LE)
	;  NNAss = NAss
	).
%% Changed back so that it fails for decls which should not be recognized 
%% as assertions! MH

%% ---------------------------------------------------------------------------
:- pred normalize_assertions_pass_two/1 # "For each assertion left by
   @pred{normalize_assertions_pass_one/1} (except for @tt{modedef}
   mode declarations, which are left as is) extracts all head
   properties and modes, normalizes all body properties, adds the head
   properties and the properties implied by the head modes to the body
   properties, and checks that a definition (or, at least, a
   declaration) is available for each property (issuing a warning
   otherwise). The old (partially) normalized assertion is eliminated
   and the fully normalized assertion is left asserted (again as an
   @pred{assertion_read/9} fact) in its place. Body property
   conjunctions are (currently) represented as lists to facilitate
   processing.".

:- pred normalize_assertions_pass_two_opts/2 # "Same as
   @pred{normalize_assertions_pass_two/1} except that it admits options
   in the second argument.".
%% ---------------------------------------------------------------------------

normalize_assertions_pass_two(M) :-
	normalize_assertions_pass_two_opts(M,[]).

normalize_assertions_pass_two_opts(M,Opts) :-
	( assertion_read(PD,M,AStatus,AType,NAss,Dict,S,LB,LE),
	  %% modedefs already transformed in pass one -- leave as is
	  AType \== modedef,
	  retract_fact(assertion_read(PD,AM,AStatus,AType,NAss,Dict,S,LB,LE)),
	  normalize_properties(NAss,NPropAss,M,F/A,Opts,AType,S,LB,LE),
	  check_body_properties(M,AM,F,A,NPropAss,S,LB,LE),
	  assertion_body(NPD,_,_,_,_,_,NPropAss),
	  assertz_fact(
	     assertion_read(NPD,AM,AStatus,AType,NPropAss,Dict,S,LB,LE)),
	  fail
	; true ).

%% -------------------------------------------------------------------------
:- pred normalize_if_assertion_body(
   M,Ass,AssrtStatus,AssrtType,NBodyAndHead,S,LB,LE)
:: moddesc * assrt_body * assrt_status * assrt_type 
   * assrt_body * atm * int * int

# "The assertion-related declaration @var{U} is in canonical format in
   @var{N}.".
%% -------------------------------------------------------------------------

normalize_if_assertion_body(M,Ass,AssrtStatus,AssrtType,NBodyAndHead,S,LB,LE):-
	normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody),
	%% At this point it has been recognized as an assertion...
        %% Check for old comments (using ;):
        (  UBody = (_;_) 
	-> warning_message(loc(S,LB,LE),
	     "old comment syntax (assertion ignored)",[]),
	   %% More verbose version...
	   %% warning_message(loc(S,LB,LE),
	   %%  "old comment syntax in ~w assertion body: ~w (assrt ignored)",
	   %%  [AssrtType,Ass]),
	   fail
        ;  normalize_assertion_body(M,AssrtType,UBody,NBodyAndHead,S,LB,LE) ).

normalize_assertion_body(_M,AssrtType,UBody,NBodyAndHead,_S,_LB,_LE) :-
	norm_body(UBody,Format,NBody),
	assertion_format(AssrtType,Format), % Do not put before norm_body!
	!,
	assertion_body(PD,DP,CP,AP,GP,CO,NBody),
	%% Put all heads  in f(var,..,var) form
	(  PD = F/A 
	-> functor(NPD,F,A)
	;  NPD=PD ),
	assertion_body(NPD,DP,CP,AP,GP,CO,NBodyAndHead).
normalize_assertion_body(M,AssrtType,UBody,_NBodyAndHead,S,LB,LE) :-
 	error_message(loc(S,LB,LE),"~w assertion syntax in module ~w: ~w",
                      [AssrtType,M,UBody]),
	fail.
	
% For debugging...
report_assertion_body(ND) :-
	prolog_flag(write_strings, Old, on),
	assertion_body(PD,DP,CP,AP,GP,CO,ND),
	simple_message("***~n",[]),
	simple_message("Predicate = ~w~n",[PD]),
	simple_message("Cmpt Info = ~w~n",[DP]),
	simple_message("Call Info = ~w~n",[CP]),
	simple_message("Answ Info = ~w~n",[AP]),
	simple_message("Othe Info = ~w~n",[GP]),
	simple_message("Comment   = ~s~n",[CO]),
	set_prolog_flag(write_strings, Old).

%% ---------------------------------------------------------------------------
:- pred normalize_status_and_type(
  +assrt_body,go(assrt_status),go(assrt_type),go(assrt_body)).
%% ---------------------------------------------------------------------------

normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody) :- 
	Ass  =.. [AssrtType,UBody],
	assrt_type(AssrtType),
	default_assrt_status(AssrtType,AssrtStatus),
	!.
normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody) :- 
	Ass  =.. [AssrtType,AssrtStatus,UBody],
	assrt_type(AssrtType),
	nonvar(AssrtStatus),
	assrt_status(AssrtStatus),
	!.
normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody) :- 
	Ass  =.. [AssrtType,AssrtStatus,UBody],
	assrt_type(AssrtType),
	var(AssrtStatus),
	default_assrt_status(AssrtType,AssrtStatus),
	!.

%% ---------------------------------------------------------------------------
:- pred default_assrt_status(+assrt_type,-assrt_status) 
# "Defines the status to be used for a given assertion type, if an
   assertion status is not specified explicitly.".
%% ---------------------------------------------------------------------------

default_assrt_status(entry,   true ) :- !. %% ???
default_assrt_status(modedef, true ) :- !. %% ???
default_assrt_status(X,       check) :-
	assrt_type(X),
	!.

%% ---------------------------------------------------------------------------
:- pred assertion_format(assrt_type(AssrtType),assrt_format_code(Code)) 
# "@var{Code} describes an admissible format in which assertions of
   the class @var{AssrtType} can be written.".
%% ---------------------------------------------------------------------------

%% Admissible assertion formats:
assertion_format(pred,    X) :- assrt_format_code(X).
assertion_format(decl,    X) :- assrt_format_code(X). %% ?
assertion_format(prop,    X) :- assrt_format_code(X). 
%% Obsolete: delete eventually...
%% assertion_format(type,    t).
%% Not needed any more...
%%assertion_format(type,    g). %% Added for now to put typedef there...
%% assertion_format(compat,  d). %% Not using these as basic any more?!
assertion_format(calls,   c).
assertion_format(success, s).
assertion_format(comp,    g).
%% These to become obsolete?
assertion_format(entry,   c).
assertion_format(entry,   t).
%% Not an assertion any more, but a status instead
%% assertion_format(trust,   X) :- assrt_format_code(X).
assertion_format(modedef, X) :- assrt_format_code(X).

:- prop assrt_format_code(X) + regtype
   # "@var{X} is a designator for an assertion format.".

assrt_format_code(p).
assrt_format_code(d).
assrt_format_code(c).
assrt_format_code(s).
assrt_format_code(g).
assrt_format_code(t).

%% ---------------------------------------------------------------------------
:- pred norm_body(B,NB) 
   # "@var{NB} is a normalized assertion body corresponding to the
     unnomalized assertion body @var{B}.".
%% ---------------------------------------------------------------------------

%% MH: Added new assertions for transition. Marked as %N
%% MH: No comments allowed now in basic assertions (difficult to document).

% ------------ A  B   C  D  E --FormatId--------------------------- %ABCDE
norm_body((PD::DP:CP=>AP+GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111%N
norm_body((PD::DP:CP=>AP+GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110
norm_body((PD::DP:CP=>AP   #CO),p,(PD::DP  :CP  =>AP  +true,CO)):-!.%11101%N%N
norm_body((PD::DP:CP=>AP      ),p,(PD::DP  :CP  =>AP  +true#"")):-!.%11100
norm_body((PD::DP:CP    +GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011%N
norm_body((PD::DP:CP    +GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010
norm_body((PD::DP:CP       #CO),p,(PD::DP  :CP  =>true+true#CO)):-!.%11001%N
norm_body((PD::DP:CP          ),p,(PD::DP  :CP  =>true+true#"")):-!.%11000
norm_body((PD::DP   =>AP+GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111%N
norm_body((PD::DP   =>AP+GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110
norm_body((PD::DP   =>AP   #CO),p,(PD::DP  :true=>AP  +true#CO)):-!.%10101%N
norm_body((PD::DP   =>AP      ),p,(PD::DP  :true=>AP  +true#"")):-!.%10100
norm_body((PD::DP       +GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011%N
norm_body((PD::DP       +GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010
norm_body((PD::DP          #CO),p,(PD::DP  :true=>true+true#CO)):-!.%10001%N
norm_body((PD::DP             ),d,(PD::DP  :true=>true+true#"")):-!.%10000
norm_body((PD    :CP=>AP+GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111%N
norm_body((PD    :CP=>AP+GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110
norm_body((PD    :CP=>AP   #CO),s,(PD::true:CP  =>AP  +true#CO)):-!.%01101%N
norm_body((PD    :CP=>AP      ),s,(PD::true:CP  =>AP  +true#"")):-!.%01100
norm_body((PD    :CP    +GP#CO),p,(PD::true:CP  =>true+GP  #CO)):-!.%01011%N
norm_body((PD    :CP    +GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010
norm_body((PD    :CP       #CO),p,(PD::true:CP  =>true+true#CO)):-!.%01001%N
norm_body((PD    :CP          ),c,(PD::true:CP  =>true+true#"")):-!.%01000
norm_body((PD       =>AP+GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111%N
norm_body((PD       =>AP+GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110
norm_body((PD       =>AP   #CO),p,(PD::true:true=>AP  +true#CO)):-!.%00101%N
norm_body((PD       =>AP      ),s,(PD::true:true=>AP  +true#"")):-!.%00100
norm_body((PD           +GP#CO),p,(PD::true:true=>true+GP  #CO)):-!.%00011%N
norm_body((PD           +GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010
norm_body((PD              #CO),p,(PD::true:true=>true+true#CO)):-!.%00001%N
norm_body((PD                 ),t,(PD::true:true=>true+true#"")):-!.%00000
% ----------------------------------------------------------------- % ----

%% ---------------------------------------------------------------------------
:- pred normalize_properties(
	Ass,NAss,in(M,moddesc),out(predname),Opts,AType,S,LB,LE) 

   : nabody(Ass) => nabody(NAss) 

   # "The body of @var{NAss} contains the normalized versions of the
     properties in the head and body of @var{Ass}. Body @em{structure}
     is assumed to be normalized in @var{Ass}(i.e., it is assumed that
     the assertion has already been filtered by
     @pred{normalize_body/2}). @var{M} is the current module, @var{AM}
     the module in which the assertion is declared.".
%% ---------------------------------------------------------------------------

normalize_properties(Ass,NAss,M,F/A,Opts,AType,S,LB,LE) :-
       	assertion_body(PD,   DP,  CP,  AP,  GP,CO,Ass),
       	assertion_body(NPD,CNDP,CNCP,CNAP,CNGP,CO,NAss),
	functor(PD,F,A),
	% Normalize properties and modes in head
        get_head_arg_props(PD,NPD,ADP,ACP,AAP,AGP,M,Opts,AType,S,LB,LE),
	% Normalize properties written in "prop * prop" format, 
        % turn conjuction into a list (not such a good idea?)
	norm_arg_props(DP,NDP,NPD,A,M,S,LB,LE),
	norm_arg_props(CP,NCP,NPD,A,M,S,LB,LE),
	norm_arg_props(AP,NAP,NPD,A,M,S,LB,LE),
	% Normalize properties written as "prop" (rather than "prop(Goal)")
	norm_goal_props(GP,NGP,NPD),
	% Add head arg props to the other props found
	append(ADP,NDP,CNDP),
	append(ACP,NCP,CNCP),
	append(AAP,NAP,CNAP),
	append(AGP,NGP,CNGP),
	!.
normalize_properties(Ass,_,_M,AM,_Opts,_AT,S,LB,LE):-
       	assertion_body(PD,  _DP, _CP, _AP, _GP,_CO,Ass),
 	error_message(loc(S,LB,LE),
                      "assertion syntax for ~w in module ~w",[PD,AM]),
	fail.
	
normalize_modedef_properties(( PD:: DP: CP=> AP+ GP#CO),
	                     ( PD::NDP:NCP=>NAP+NGP#CO), M,S,LB,LE) :-
	functor(PD,_,A),
	% Normalize properties written in "prop * prop" format, 
        % turn conjuction into a list (not such a good idea?)
	norm_arg_props(DP,NDP,PD,A,M,S,LB,LE),
	norm_arg_props(CP,NCP,PD,A,M,S,LB,LE),
	norm_arg_props(AP,NAP,PD,A,M,S,LB,LE),
	% Normalize properties written as "prop" (rather than "prop(Goal)")
	% In fact, it is better not to normalize these here: 
        % since they have to be normalized w.r.t. the target predicate 
	%% norm_goal_props(GP,NGP,PD),
        % Even better: normalize as normal properties! (this way we do not 
        % leave behind half normalized modedefs which the 
        % assertion pretty printer would not like
%% 	( GP \== true
%% 	-> simple_message("*** Normalizing ~w modedef goal prop ~w",[PD,GP])
%% 	;  true ),
%% 	norm_arg_props(GP,NGP,PD,A,M),
%% 	( NGP \== []
%% 	-> simple_message("*** Normalized ~w modedef goal prop ~w",[PD,NGP])
%% 	;  true ),
	% Except that then they cannot be documented well... so, normalize
	% after all, and undo it later...
%%  	(  GP \== true
%%  	-> simple_message("*** Normalizing ~w modedef goal prop ~w",[PD,GP])
%% 	;  true ),
	norm_goal_props(GP,NGP,PD),
%%  	(  NGP \== []
%% 	-> simple_message("*** Normalized ~w modedef goal prop ~w",[PD,NGP])
%% 	;  true ),
	%% NGP=GP,
	!.
normalize_modedef_properties(( PD:: _DP:  _CP=>  _AP+  _GP# _CO),_,M,S,LB,LE):-
 	error_message(loc(S,LB,LE),
	          "syntax in modedef declaration for ~w in module ~w",[PD,M]),
	fail.
	
%% ---------------------------------------------------------------------------
:- pred get_head_arg_props(Head,NPD,NDP,NCP,NAP,M,Opts,AType,S,LB,LE) 
   => (list(NDP),list(NCP),list(NAP))

   # "@var{Head} is a head descriptor whose arguments possibly include
      mode annotations. These get translated into standard
      properties. @var{NPD} is the new head descriptor. @var{NDP}
      contais the new compatible properties. @var{NCP}
      contais the new call properties. @var{NAP} contains the new
      answer properties.".
%% ---------------------------------------------------------------------------

get_head_arg_props(PD,NPD,DP,CP,AP,GP,M,Opts,AType,S,LB,LE) :-
	functor(PD, F,A),
	functor(NPD,F,A),
	transform_head_arg_props(
           0,A,PD,NPD,DP,CP,AP,GP,F,A,M,Opts,AType,S,LB,LE).

transform_head_arg_props(Last,Last,_PD,_NPD,[],[],[],[],_,_,_,_,_,_,_,_) :-
	!.
transform_head_arg_props(
               PArg,Last,PD,NPD,DP,CP,AP,GP,F,A,M,Opts,AType,S,LB,LE) :-
	Arg is PArg+1,
	arg(Arg,PD,PDA),
	arg(Arg,NPD,NPDA),
	get_arg_props(
           PDA,NPDA,DP-DPT,CP-CPT,AP-APT,GP-GPT,NPD,F,A,M,Opts,AType,S,LB,LE),
	transform_head_arg_props(
		Arg,Last,PD,NPD,DPT,CPT,APT,GPT,F,A,M,Opts,AType,S,LB,LE).

%% Handling of ISO standard-like "modes" and properties which appear 
%% literally in the head.
%% 
%% p(+A) p(+) p(int) p(+int) p(+list(int)) ...
%% p(ilist(A,integer)) (parametric mode)
%% 
%% Argument is a variable - do nothing
get_arg_props(PDA,PDA,D-D,C-C,A-A,G-G,_NPD,_F,_A,_M,_Opts,_AType,_S,_LB,_LE) :-
	var(PDA),
	!.
%% Argument is a defined (possibly parametric) mode, 
get_arg_props(PDA,NPDA,NDP,NCP,NAP,NGP,NPD,_F,_A,M,Opts,AType,S,LB,LE) :- 
	with_or_without_arg(PDA,NNPDA,Prop),
	%% This M below forces modedefs to be in the file
        %% i.e., they must be in the file or in includes...
        %% But they could possibly also be imported from a module?
	assertion_read(Prop,M,_AStatus,modedef,NPropAss,_Dict,_AS,_ALB,_ALE),
	(  member('-modes',Opts), 
	   \+ propfunctor(AType)
	-> %% Keep modes (and their properties!): do nothing.
	   NPDA = PDA, NDP=DL-DL, NCP=CL-CL, NAP=AL-AL, NGP=GL-GL
	;  %% Assumed that the Props have already been put in list form!
	   NPDA = NNPDA,
	   NPropAss= ((_Prop::CompatProps:CallProps=>AnswerProps+GoalProps#_)),
	   !,
	   resolve_applications(CompatProps,ACompatProps,S,LB,LE),
	   diff_append_props(ACompatProps,NDP),
	   resolve_applications(CallProps,ACallProps,S,LB,LE),
	   diff_append_props(ACallProps,NCP),
	   resolve_applications(AnswerProps,AAnswerProps,S,LB,LE),
	   diff_append_props(AAnswerProps,NAP),
           % Goal Props in modedef should have to be normalized at this point.
           % Since now they come normalized as normal properties, first 
	   % denormalize a bit (to conj) and then fully normalize:
%%  	   (  GoalProps \== []
%% 	   -> simple_message("*** Processing ~w modedef goalprops ~w",[F/A,GoalProps])
%% 	   ;  true ),
%% 	   list_to_conj(GoalProps,CGoalProps),
%% 	   norm_goal_props(CGoalProps,NGoalProps,NPD),
%%  	   (  GoalProps \== []
%%  	   -> simple_message("*** Processed ~w modedef goalprops ~w",[F/A,NGoalProps])
%% 	   ;  true ),
%%  	   (  GoalProps \== []
%% 	   -> simple_message("*** Processing ~w modedef goalprops ~w",[F/A,GoalProps])
%% 	   ;  true ),
	   norm_goal_props(DNGoalProps,GoalProps,_),
	   norm_goal_props(DNGoalProps,NGoalProps,NPD),
%%  	   (  NGoalProps \== []
%%  	   -> simple_message("*** Processed ~w modedef goalprops ~w",[F/A,NGoalProps])
%% 	   ;  true ),
	   resolve_applications(NGoalProps,AGoalProps,S,LB,LE),
	   diff_append_props(AGoalProps,NGP)
	).
%% Else, argument is assumed to be a (possibly parametric) property
%% If '-headprops' is on, leave as is:
get_arg_props(PDA,PDA,D-D,C-C,A-A,G-G,_NPD,_F,_A,_M,Opts,AType,_S,_LB,_LE) :-
	member('-headprops',Opts), 
	\+ propfunctor(AType),
	!.
%% Else, properties are added to compatibility field!
get_arg_props(PDA,NPDA,NDP,NCP,NAP,NGP,_NPD,_F,_A,_M,_Opts,_AType,_S,_LB,_LE):-
	with_or_without_arg(PDA,NPDA,Prop),
	!,
	diff_append_props([Prop],NDP),
	diff_append_props([],NCP),
	diff_append_props([],NAP),
	diff_append_props([],NGP).
get_arg_props(PDA,PDA,DP-DP,CP-CP,AP-AP,G-G,_NPD,F,A,M,_Opts,_AType,S,LB,LE) :-
	error_message(loc(S,LB,LE),
             "syntax of argument '~w' in assertion for ~w/~w in module ~w",
	              [PDA,F,A,M] ).

%% with no argument variable, e.g., p(+), p(in(foo))
with_or_without_arg(PDA,NPDA,Prop) :-
          ground(PDA),
	  !,
 	  PDA =.. [F|Rest],
 	  Prop =.. [F,NPDA|Rest].
%% with argument variable, e.g., p(+(X)), p(in(X,foo))
with_or_without_arg(PDA,NPDA,Prop) :-
   	  PDA =.. [_,NPDA|Rest],
	  var(NPDA),
	  ground(Rest),
	  !,
 	  Prop = PDA.

resolve_applications([],[],_S,_LB,_LE) :- 
	!.
%% newer ciao versions translate T(X,Y) to call(T,X,Y) instead.
%% resolve_applications([apply(CF,[Arg])|R],[Prop|NR]) :-
%% 	!,
%% 	CF =.. [PF|FArgs],
%% 	Prop =.. [PF,Arg|FArgs],
%% 	resolve_applications(R,NR).
resolve_applications([Call|R],[Prop|NR],S,LB,LE) :-
	nonvar(Call),
	Call =.. [call,CF|Args],
	!,
	(  nonvar(CF)
	-> CF =.. [PF|FArgs],
	   %% we take care of call(foo(X),Y)
	   append(FArgs,Args,AllArgs), 
	   %% we take care recursively of nesting: call(foo,X,call(bar,Y))
	   resolve_applications(AllArgs,AllArgsResolved,S,LB,LE),
	   Prop =.. [PF|AllArgsResolved]
	;  error_message(loc(S,LB,LE),
	   "principal functor not sufficiently instantiated in mode: ~w",
                         [Call])
        ),
	resolve_applications(R,NR,S,LB,LE).
resolve_applications([Prop|R],[NProp|NR],S,LB,LE) :-
	nonvar(Prop),
	!,
	Prop =.. [Functor|Args],
	resolve_applications(Args,ArgsResolved,S,LB,LE),
	NProp =.. [Functor|ArgsResolved],
	resolve_applications(R,NR,S,LB,LE).
resolve_applications([Prop|R],[Prop|NR],S,LB,LE) :-
	resolve_applications(R,NR,S,LB,LE).

diff_append_props([],T-T).
diff_append_props([H|T],PH-PT) :-
	PH=[H|NPT],
	diff_append_props(T,NPT-PT).

%% ---------------------------------------------------------------------------
:- comment(norm_arg_props/8,"@var{Props} is a term describing
     properties in an assertion call or sucess point. @var{PropExpr}
     is the normalized version of @var{Props} in list format. ").

:- pred norm_arg_props(Props,PropExpr,PD,Arity,M,S,LB,LE) 
   :  (property_conjunction(Props),var(PropExpr),nonvar(PD),int(Arity)) 
   => nonvar(PropExpr).

:- pred norm_arg_props(Props,PropExpr,PD,Arity,M,S,LB,LE) 
   :  (property_starterm(Props),var(PropExpr),nonvar(PD),int(Arity)) 
   => nonvar(PropExpr).

%% ---------------------------------------------------------------------------

% No props
norm_arg_props(true,[],_PD,_Arity,_M,_S,_LB,_LE) :-
	!.
% No props
norm_arg_props([],[],_PD,_Arity,_M,_S,_LB,_LE) :-
	!.
% Abridged props: * main funct or single prop (arity zero or {} main functor)
norm_arg_props(Props,PropExp,PD,Arity,M,S,LB,LE) :-
	%% The last two are the two unary base cases (hardest to detect)
	( Props = _R * _L ; Props = '{}'(_) ; ground(Props) ),
 	!,
 	norm_abridged_props(Props,PropExp,PD,Arity,M,S,LB,LE).
% Normal props (conjucntion)
norm_arg_props(Props,PropExp,_PD,_Arity,_M,_S,_LB,_LE) :-
 	norm_normal_props(Props,PropExp).
	
% No disjunctions supported yet... 
norm_normal_props((Prop,Rest),[Prop|NRest]) :-
	!,
	norm_normal_props(Rest,NRest). 
norm_normal_props(FinalProp,[FinalProp]).

norm_abridged_props(Ps * P,NPs,PD,Arg,M,S,LB,LE) :-
	!,
	add_argvars(P,Arg,PD,NP),
	NArg is Arg - 1,
	norm_abridged_props(Ps,NLs,PD,NArg,M,S,LB,LE),
	append(NLs,NP,NPs).
norm_abridged_props(P,NP,PD,Arg,_M,_S,_LB,_LE) :-
	Arg = 1,
	add_argvars(P,Arg,PD,NP).
norm_abridged_props(_P,_NP,PD,_Arg,M,S,LB,LE) :-
	error_message(loc(S,LB,LE),
	   "arity mismatch in declaration for ~w in ~w",[PD,M]).

add_argvars('{}'(P),Arg,PD,NPs) :- 
	!,
	add_tuple_argvars(P,Arg,PD,NPs).
add_argvars(P,Arg,PD,[NP]) :- 
	add_argvar(P,Arg,PD,NP).

add_tuple_argvars(','(P,PR),Arg,PD,[NP|NPs]) :-
	!,
	add_argvar(P,Arg,PD,NP),
	add_tuple_argvars(PR,Arg,PD,NPs).
add_tuple_argvars(P,Arg,PD,[NP]) :-
	add_argvar(P,Arg,PD,NP).

add_argvar(P,Arg,PD,NP) :-
	P =.. [F|Vars],
	arg(Arg,PD,Var),
	NP =.. [F,Var|Vars].

%% ---------------------------------------------------------------------------
:- comment(norm_goal_props(Props,PropList,NPr), "@var{Props} is a
   term describing global properties in an assertion. The standard
   format is a conjunction of 0-ary (or, possibly unary)
   properties. @var{PropList} is the normalized version of @var{Props}
   in list format.").

:- pred norm_goal_props(+Props,-PropList,-NPr) # "Normalizes global
   properties.".
:- pred norm_goal_props(-Props,+PropList,+NPr) # "Denormalizes global
   properties.".
%% ---------------------------------------------------------------------------

%% Needs to be improved?
norm_goal_props(true,[],_) :-
	!.
norm_goal_props((GP,GPs),[NGP|NGPs],NPD) :-
	!,
	norm_goal_prop(GP,NGP,NPD),
	norm_goal_props(GPs,NGPs,NPD).
norm_goal_props(GP,[NGP],NPD) :-
	!,
	norm_goal_prop(GP,NGP,NPD).

%% Univ is not smart enough for one version
norm_goal_prop(GP,NGP,NPD) :-
	nonvar(GP),
	!,
	GP  =..[F|Args],
	NGP =.. [F,NPD|Args].
norm_goal_prop(GP,NGP,NPD) :-
	nonvar(NGP),
	!,
	NGP =.. [F,NPD|Args],
	GP  =..[F|Args].

denorm_goal_prop(NGP,GP,NPD) :-
	norm_goal_prop(GP,NGP,NPD).
	
%% ---------------------------------------------------------------------------
:- pred check_body_properties(
   in(CurrentMod,moddesc),in(AssrtMod,moddesc),
   in(F,atom),in(A,int),in(NAss,nabody),in(S),in(LB),in(LE))

        # "Checks each property in the body of assertion @var{NAss}
          (see @pred{check_property/4}). Checks only assertions in the
          current module @var{CurrentMod}.".
%% ---------------------------------------------------------------------------

check_body_properties(M,M,F,A,NAss,S,LB,LE):- !,
       	assertion_body(_NPD,CNDP,CNCP,CNAP,CNGP,_CO,NAss),
	check_properties(CNDP,F,A,M,S,LB,LE),
	check_properties(CNCP,F,A,M,S,LB,LE),
	check_properties(CNAP,F,A,M,S,LB,LE),
	check_properties(CNGP,F,A,M,S,LB,LE).
check_body_properties(M,AM,_Functor,_Arity,_NAss,_S,_LB,_LE):- 
	M \== AM.

%% ---------------------------------------------------------------------------
:- pred check_properties(Props,in(F,atm),in(A,int),in(M,moddesc),
	                 in(S,atm),in(LB,int),in(LE,int))

	: list(Props)

        # "Checks each property in @var{Props} (see @pred{check_property/4}).".
%% ---------------------------------------------------------------------------

check_properties([],_F,_A,_M,_S,_LB,_LE).
check_properties([Prop|Props],F,A,M,S,LB,LE) :-
	check_property(Prop,F,A,M,S,LB,LE),
	check_properties(Props,F,A,M,S,LB,LE).

%% ---------------------------------------------------------------------------
:- pred check_property(+Prop,in(F,atom),in(A,int),in(M,moddesc),
	               in(S,atm),in(LB,int),in(LE,int)) 

# "Checks that, for a property @var{Prop} (which appears in a
   declaration for @var{F}/@var{A} in module @var{M}), a definition
   for that property is visible to that module.".
%% ---------------------------------------------------------------------------

%% 0.8 version: the decls are not sometimes available yet...
%% but line numbers available!
check_property(Prop,F,A,M,S,LB,LE) :- 
  	%% simple_message("** Checking ~w",[Prop]),
	functor(Prop,PF,PA),
	(  ( imports(M,_PM,PF,PA,_)
	   ; defines(M,PF,PA) )
	-> true
	;  warning_message(loc(S,LB,LE),
		  "~w used in assrt for ~w in ~w not defined or imported",
		  [PF/PA,F/A,M] ) ),
	!.

%% 0.7 version
%% check_property(Prop,F,A,M) :- 
%%   	%% simple_message("** Checking ~w",[Prop]),
%% 	functor(Prop,PF,PA),
%% 	functor(NProp,PF,PA),
%% 	(  assertion_read(NProp,PM,_AStatus,Type,_NPropAss,_Dict,_LC),
%% 	   propfunctor(Type)
%%            %% simple_message("** Found ~w as a ~w in module ~w",[NProp,Type,PM])
%% 	-> true
%% 	;  warning_message("no decl for prop ~w used in assrt for ~w in ~w",
%% 		  [PF/PA,F/A,M] ) ),
%% 	(  ( imports(M,PM,PF,PA)
%% 	   ; defines(M,PF,PA) )
%% 	-> true
%% 	;  warning_message("~w used in assrt for ~w in ~w not defined or imported",
%% 		  [PF/PA,F/A,M] ) ),
%% 	!.

%% ---------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+57,2001/02/07,16:12*10+'CET'), "Split module
   assrt_lib (Francisco Bueno Carrillo)").

%% ---------------------------------------------------------------------------
