% (included file)

% Common code for assertion normalization. The input/output databases
% and the behavior of this code is modified through the following
% predicates (which the user of this code must define):
%
% (Pass one:) 
%  - pass_one_cleanup(M): cleanup before pass one
%  - get_source_assrt(Base,Assrt,Dict,S,LB,LE): input for
%    @pred{normalize_assertions_pass_one/2}
%
% (Pass two:) 
%  - pass_two_not_required(AType): skip assertion of type @var{AType}
%  - pass_two_check_body(AType,M,AM,Spec,NPropAss,S,LB,LE):
%      check assertion body 
%  - ignore_norm_props(AType): ignore property normalization for AType
%  - norm_normal_props/2: turn conjunctions into list
%    (it can be extended to support disjunctions)
% 
% The interface to the output database (pass one and reused for pass
% two) must be defined by:
% 
%  - add_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE): assert
%  - del_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE): retract
%  - get_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE): consult 
%
% ===========================================================================

:- use_module(library(assertions/assertions_props)).
:- use_module(library(assertions/c_itf_props)).

:- use_module(library(lists), [member/2, append/3, cross_product/2, delete/3]).
:- use_module(library(messages)).

%% ---------------------------------------------------------------------------
:- pred normalize_assertions(M,Base,Opts) :: moddesc(M)
   # "This predicate calls @pred{normalize_assertions_pass_one/2} and
   then @tt{normalize_assertions_pass_two_opts(M, Opts)}, thus leaving
   all assertions in the database in fully normalized form.".

normalize_assertions(M,Base,Opts):-
    normalize_assertions_pass_one(M,Base),
    normalize_assertions_pass_two_opts(M,Opts).

%% ---------------------------------------------------------------------------
:- pred normalize_assertion(M,Assrt,PD,AStatus,AType,NABody,S,LB,LE) 
   :: moddesc * term * term * assrt_status * assrt_type * assrt_body * atm * int * int
   # "Normalizes one assertion (see @pred{normalize_assertions/3}).".

normalize_assertion(M,Assrt,PD,AStatus,AType,NABody,S,LB,LE) :-
    normalize_one_assertion_pass_one(M,Assrt,PD,AStatus,AType,ABody,S,LB,LE),
    ( ignore_norm_props(AType) ->
        NABody = ABody
    ; normalize_properties(ABody,NABody,M,_Functor,[],AType,S,LB,LE)
    ).

% %% ---------------------------------------------------------------------------
% :- pred normalize_assertions_debug(M,Base) :: moddesc * atm # "Same as
%    @pred{normalize_assertions/0} but it reports on the normalization
%    process by printing all assertions after normalization passes one
%    and two. If @var{M} is instantiated only information on module
%    @var{M} is printed. Otherwise information for all modules is
%    printed.".
% %% ---------------------------------------------------------------------------
% 
% normalize_assertions_debug(M,Base) :-
%       normalize_assertions_debug_opts(M,Base,[]).
% 
% normalize_assertions_debug_opts(M,Base,Opts) :-
%       prolog_flag(write_strings, Old, on),
%       messages_basic:message(inform, ['{Normalizing assertions in ',M,' (pass one)']),
%       normalize_assertions_pass_one(M,Base),
%       messages_basic:message(inform, '}'),
%       print_unformatted_assertions(_M),
%       messages_basic:message(inform, ['{Normalizing assertions in ',M,' (pass two)']),
%       normalize_assertions_pass_two_opts(M,Opts),
%       messages_basic:message(inform, '}'),
%         current_output(CI),
%         set_output(user_error),
%       print_unformatted_assertions(_M),
%         set_output(CI),
%       set_prolog_flag(write_strings, Old).

%% ---------------------------------------------------------------------------
:- pred normalize_assertions_pass_one/2 :: moddesc * atm # "For each assertion
   normalizes the assertion body (but not the properties inside or
   those properties and modes which appear in the head). The predicate
   descriptor (both that in the assertion body and the first argument
   of @pred{add_assertion_of/9}) is partially normalized in that
   @tt{functor/arity} formats are expanded into terms, but modes and
   properties in arguments are left for the second pass (i.e.,
   @tt{p(X,+)} may be present, but not @tt{p/2}). However, if the
   assertion is a @tt{modedef} then it is fully normalized at this
   time (including body properties, which are normalized but not
   checked) so that @pred{normalize_assertions_pass_two/1} can use it
   later while normalizing other assertions. The (partially)
   normalized assertion is left asserted using @pred{add_assertion_of/9}
   fact.".

normalize_assertions_pass_one(M,Base) :-
    % defines_module(Base,M),
    pass_one_cleanup(M),
    ( %% Normalize all assertions in this module
      get_source_assrt(Base,Assrt,Dict,S,LB,LE),
      normalize_one_assertion_pass_one(M,Assrt,PD,AStatus,AType,NNAss,S,LB,LE),
      add_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE),
      fail
    ; true
    ).

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
:- pred normalize_assertions_pass_two_opts/2 # "For each assertion left by
   @pred{normalize_assertions_pass_one/2} (except for @tt{modedef}
   mode declarations, which are left as is) extracts all head
   properties and modes, normalizes all body properties, adds the head
   properties and the properties implied by the head modes to the body
   properties, and checks that a definition (or, at least, a
   declaration) is available for each property (issuing a warning
   otherwise). The old (partially) normalized assertion is eliminated
   and the fully normalized assertion is left asserted (see
   @pred{add_assertion_of/9}) in its place. Body property
   conjunctions are (currently) represented as lists to facilitate
   processing. The second argument admit options.".

normalize_assertions_pass_two_opts(M,Opts) :-
    ( get_assertion_of(PD,M,AStatus,AType,NAss,Dict,S,LB,LE),
      \+ pass_two_not_required(AType),
      del_assertion_of(PD,AM,AStatus,AType,NAss,Dict,S,LB,LE), % TODO: why AM instead of M?
      normalize_properties(NAss,NPropAss,M,Spec,Opts,AType,S,LB,LE),
      pass_two_check_body(AType,M,AM,Spec,NPropAss,S,LB,LE),
      assertion_body(NPD,_,_,_,_,_,NPropAss),
      add_assertion_of(NPD,AM,AStatus,AType,NPropAss,Dict,S,LB,LE),
      fail
    ; true
    ).

%% -------------------------------------------------------------------------
:- pred normalize_if_assertion_body(M,Ass,AssrtStatus,AssrtType,NBodyAndHead,S,LB,LE)
   :: moddesc * term * assrt_status * assrt_type * assrt_body * atm * int * int
   # "The assertion-related declaration @var{U} is in canonical format in
   @var{N}.".

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

% % For debugging...
% report_assertion_body(ND) :-
%     prolog_flag(write_strings, Old, on),
%     assertion_body(PD,DP,CP,AP,GP,CO,ND),
%     simple_message("***~n",[]),
%     simple_message("Predicate = ~w~n",[PD]),
%     simple_message("Cmpt Info = ~w~n",[DP]),
%     simple_message("Call Info = ~w~n",[CP]),
%     simple_message("Answ Info = ~w~n",[AP]),
%     simple_message("Othe Info = ~w~n",[GP]),
%     simple_message("Comment   = ~s~n",[CO]),
%     set_prolog_flag(write_strings, Old).

%% ---------------------------------------------------------------------------
% :- pred normalize_status_and_type(+assrt_body,go(assrt_status),go(assrt_type),go(assrt_body)).

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
:- pred assertion_format(AssrtType, Code) :: assrt_type * assrt_format_code
# "@var{Code} describes an admissible format in which assertions of
   the class @var{AssrtType} can be written.".
%% ---------------------------------------------------------------------------

%% Admissible assertion formats:
assertion_format(pred,    X) :- assrt_format_code(X).
assertion_format(decl,    X) :- assrt_format_code(X). %% ?
assertion_format(prop,    X) :- assrt_format_code(X).
assertion_format(test,    X) :- assrt_format_code(X). %% For unit-test -- EMM
%% Obsolete: delete eventually...
%% assertion_format(type,    t).
%% Not needed any more...
%%assertion_format(type,    g). %% Added for now to put typedef there...
%% assertion_format(compat,  d). %% Not using these as basic any more?!
assertion_format(calls,   c).
assertion_format(calls,   t).
assertion_format(success, s).
%% Entry for unit-test -- EMM
assertion_format(texec,   g).
assertion_format(texec,   c).
assertion_format(texec,   t).
%% DTM: New assertion type
assertion_format(exit,    s).
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
:- pred norm_body(B,F,NB)
   # "@var{NB} is a normalized assertion body corresponding to the
     unnomalized assertion body @var{B}.".
%% ---------------------------------------------------------------------------

%% MH: No comments allowed now in basic assertions (difficult to document).

% ------------ A  B   C  D  E --FormatId--------------------------- %ABCDE
norm_body((PD::DP:CP=>AP+GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111
norm_body((PD::DP:CP=>AP+GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110
norm_body((PD::DP:CP=>AP   #CO),p,(PD::DP  :CP  =>AP  +true#CO)):-!.%11101
norm_body((PD::DP:CP=>AP      ),p,(PD::DP  :CP  =>AP  +true#"")):-!.%11100
norm_body((PD::DP:CP    +GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011
norm_body((PD::DP:CP    +GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010
norm_body((PD::DP:CP       #CO),p,(PD::DP  :CP  =>true+true#CO)):-!.%11001
norm_body((PD::DP:CP          ),p,(PD::DP  :CP  =>true+true#"")):-!.%11000
norm_body((PD::DP   =>AP+GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111
norm_body((PD::DP   =>AP+GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110
norm_body((PD::DP   =>AP   #CO),p,(PD::DP  :true=>AP  +true#CO)):-!.%10101
norm_body((PD::DP   =>AP      ),p,(PD::DP  :true=>AP  +true#"")):-!.%10100
norm_body((PD::DP       +GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011
norm_body((PD::DP       +GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010
norm_body((PD::DP          #CO),d,(PD::DP  :true=>true+true#CO)):-!.%10001
norm_body((PD::DP             ),d,(PD::DP  :true=>true+true#"")):-!.%10000
norm_body((PD    :CP=>AP+GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111
norm_body((PD    :CP=>AP+GP   ),s,(PD::true:CP  =>AP  +GP  #"")):- GP = srcloc(_Src,_LB,_LE), !.%01110
norm_body((PD    :CP=>AP+GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110
norm_body((PD    :CP=>AP   #CO),s,(PD::true:CP  =>AP  +true#CO)):-!.%01101
norm_body((PD    :CP=>AP      ),s,(PD::true:CP  =>AP  +true#"")):-!.%01100
norm_body((PD    :CP    +GP#CO),g,(PD::true:CP  =>true+GP  #CO)):-!.%01011
norm_body((PD    :CP    +GP   ),c,(PD::true:CP  =>true+GP  #"")):- GP = srcloc(_Src,_LB,_LE), !.%01010
norm_body((PD    :CP    +GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010
norm_body((PD    :CP       #CO),c,(PD::true:CP  =>true+true#CO)):-!.%01001
norm_body((PD    :CP          ),c,(PD::true:CP  =>true+true#"")):-!.%01000
norm_body((PD       =>AP+GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111
norm_body((PD       =>AP+GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110
norm_body((PD       =>AP   #CO),s,(PD::true:true=>AP  +true#CO)):-!.%00101
norm_body((PD       =>AP      ),s,(PD::true:true=>AP  +true#"")):-!.%00100
norm_body((PD           +GP#CO),g,(PD::true:true=>true+GP  #CO)):-!.%00011
norm_body((PD           +GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010
norm_body((PD              #CO),t,(PD::true:true=>true+true#CO)):-!.%00001
norm_body((PD                 ),t,(PD::true:true=>true+true#"")):-!.%00000
% ----------------------------------------------------------------- % ----

%% ---------------------------------------------------------------------------
:- pred normalize_properties(Ass,NAss,in(M,moddesc),out(predname),Opts,AType,S,LB,LE) 
%   : nabody(Ass) => nabody(NAss) % TODO: wrong types with rtchecks (EMM)
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
    append(ACP,NCP,CNCP0),
    append(AAP,NAP,CNAP0),
    append(AGP,NGP,CNGP),
    % Normalize special properties (e.g., mshare/1)
    norm_addhv_props(CNCP0,NPD,CNCP),
    norm_addhv_props(CNAP0,NPD,CNAP),
    % TODO: Possibly sort to prettify and avoid duplicate properties.
    !.
normalize_properties(Ass,_,M,_FA,_Opts,_AT,S,LB,LE):-
    assertion_body(PD,  _DP, _CP, _AP, _GP,_CO,Ass),
    error_message(loc(S,LB,LE),
                  "assertion syntax for ~w in module ~w",[PD,M]),
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
%%      ( GP \== true
%%      -> simple_message("*** Normalizing ~w modedef goal prop ~w",[PD,GP])
%%      ;  true ),
%%      norm_arg_props(GP,NGP,PD,A,M),
%%      ( NGP \== []
%%      -> simple_message("*** Normalized ~w modedef goal prop ~w",[PD,NGP])
%%      ;  true ),
    % Except that then they cannot be documented well... so, normalize
    % after all, and undo it later...
%%      (  GP \== true
%%      -> simple_message("*** Normalizing ~w modedef goal prop ~w",[PD,GP])
%%      ;  true ),
    norm_goal_props(GP,NGP,PD),
%%      (  NGP \== []
%%      -> simple_message("*** Normalized ~w modedef goal prop ~w",[PD,NGP])
%%      ;  true ),
    %% NGP=GP,
    !.
normalize_modedef_properties(( PD:: _DP:  _CP=>  _AP+  _GP# _CO),_,M,S,LB,LE):-
    error_message(loc(S,LB,LE),
              "syntax in modedef declaration for ~w in module ~w",[PD,M]),
    fail.
    
%% ---------------------------------------------------------------------------
:- pred get_head_arg_props(Head,NPD,NDP,NCP,NAP,GP,M,Opts,AType,S,LB,LE) 
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
% TODO: ignore modes in test assertions (this is needed for p_unit handling of assertions; but the unittest lib does in a different way; merge!)
get_arg_props(PDA,PDA,D-D,C-C,A-A,G-G,_NPD,_F,_A,_M,_Opts,AType,_S,_LB,_LE) :-
    AType = test,
    !.
%% Argument is a defined (possibly parametric) mode, 
get_arg_props(PDA,NPDA,NDP,NCP,NAP,NGP,NPD,_F,_A,M,Opts,AType,S,LB,LE) :-
    with_or_without_arg(PDA,NNPDA,Prop),
    %% This M below forces modedefs to be in the file
    %% i.e., they must be in the file or in includes...
    %% But they could possibly also be imported from a module?
    get_assertion_of(Prop,M,_AStatus,modedef,NPropAss,_Dict,_AS,_ALB,_ALE),
    (  member('-modes',Opts), 
       \+ propfunctor(AType)
    -> %% Keep modes (and their properties!): do nothing.
       !,
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
%%         (  GoalProps \== []
%%         -> simple_message("*** Processing ~w modedef goalprops ~w",[F/A,GoalProps])
%%         ;  true ),
%%         list_to_conj(GoalProps,CGoalProps),
%%         norm_goal_props(CGoalProps,NGoalProps,NPD),
%%         (  GoalProps \== []
%%         -> simple_message("*** Processed ~w modedef goalprops ~w",[F/A,NGoalProps])
%%         ;  true ),
%%         (  GoalProps \== []
%%         -> simple_message("*** Processing ~w modedef goalprops ~w",[F/A,GoalProps])
%%         ;  true ),
       norm_goal_props(DNGoalProps,GoalProps,_),
       norm_goal_props(DNGoalProps,NGoalProps,NPD),
%%         (  NGoalProps \== []
%%         -> simple_message("*** Processed ~w modedef goalprops ~w",[F/A,NGoalProps])
%%         ;  true ),
       resolve_applications(NGoalProps,AGoalProps,S,LB,LE),
       diff_append_props(AGoalProps,NGP)
    ).
%% Else, argument is assumed to be simply a term
get_arg_props(PDA,PDA,D-D,C-C,A-A,G-G,_NPD,_F,_A,_M,_Opts,_AType,_S,_LB,_LE).

% We assume that numbers cannot be modes.
with_or_without_arg(N,_NPDA,_Prop) :-
    number(N), !,
    fail.
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
%%      !,
%%      CF =.. [PF|FArgs],
%%      Prop =.. [PF,Arg|FArgs],
%%      resolve_applications(R,NR).
resolve_applications([Call|R],[Prop|NR],S,LB,LE) :-
    nonvar(Call),
    Call =.. [call,CF|Args],
    !,
    (  nonvar(CF)
    -> CF =.. [PF|FArgs],
       %% we take care of call(foo(X),Y)
       append(FArgs,Args,AllArgs), % TODO: merge with prop_apply/3? (>1 args)
       % apply(Args,FArgs,AllArgs),  % (version for old hiord)
       %% we take care recursively of nesting: call(foo,X,call(bar,Y))
       resolve_applications(AllArgs,AllArgsResolved,S,LB,LE),
       Prop =.. [PF|AllArgsResolved]
    ;  error_message(loc(S,LB,LE),
       "principal functor not sufficiently instantiated in mode: ~w",
                     [Call]),
       fail
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

% Note: for old hiord, deprecated
%   apply([],Args,Args).
%   apply([A|Args],FArgs,[A|AllArgs]):-
%       append(FArgs,Args,AllArgs).

%% ---------------------------------------------------------------------------
:- doc(norm_arg_props/8,"@var{Props} is a term describing
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
    functor(PD, _, A), A > 0, % TODO: it was not in ciaopp
    ( Props = _R * _L
    ; Props = '{}'(_)
    ; ground(Props)
    ),
    !,
    norm_abridged_props(Props,PropExp,PD,Arity,M,S,LB,LE).
% Normal props (conjunction)
norm_arg_props(Props,PropExp,_PD,_Arity,_M,_S,_LB,_LE) :-
    norm_normal_props(Props,PropExp).
    
norm_abridged_props(Ps * P,NPs,PD,Arg,M,S,LB,LE) :-
    add_argvars(P,Arg,NArg0,PD,NP),
    NArg is NArg0 - 1,
    norm_abridged_props(Ps,NLs,PD,NArg,M,S,LB,LE), !,
    append(NLs,NP,NPs).
norm_abridged_props(P,NP,PD,Arg,_M,_S,_LB,_LE) :-
    add_argvars(P,Arg,NArg0,PD,NP),
    % check the rest of args are nonvar:
    NArg is NArg0 - 1,
    \+ add_argvars(P,NArg,_,PD,NP), !.
norm_abridged_props(_P,_NP,PD,_Arg,M,S,LB,LE) :-
    error_message(loc(S,LB,LE),
       "arity mismatch in declaration for ~w in ~w",[PD,M]),
    fail.

add_argvars('{}'(P),Arg,NArg,PD,NPs) :- 
    !,
    add_tuple_argvars(P,Arg,NArg,PD,NPs).
add_argvars(P,Arg,NArg,PD,[NP]) :- 
    add_argvar(P,Arg,NArg,PD,NP).

add_tuple_argvars(','(P,PR),Arg,NArg,PD,[NP|NPs]) :-
    !,
    add_argvar(P,Arg,NArg,PD,NP),
    % all refered to the same NArg:
    add_tuple_argvars(PR,NArg,NArg,PD,NPs).
add_tuple_argvars(P,Arg,NArg,PD,[NP]) :-
    add_argvar(P,Arg,NArg,PD,NP).

add_argvar(M:P,Arg,NArg,PD,M:NP) :- !,
    add_argvar(P,Arg,NArg,PD,NP).
add_argvar(P,Arg,NArg,PD,NP) :-
    arg(Arg,PD,Var),
    var(Var), !,
    NArg = Arg,
    prop_apply(P,Var,NP).
add_argvar(P,Arg,NArg,PD,NP) :-
    NArg1 is Arg-1,
    NArg1 > 0,
    add_argvar(P,NArg1,NArg,PD,NP).

%% ---------------------------------------------------------------------------
:- doc(norm_goal_props(Props,PropList,NPr), "@var{Props} is a
   term describing global properties in an assertion. The standard
   format is a conjunction of 0-ary (or, possibly unary)
   properties. @var{PropList} is the normalized version of @var{Props}
   in list format.").

:- pred norm_goal_props(+Props,-PropList,+NPr) # "Normalizes global properties.".
:- pred norm_goal_props(-Props,+PropList,?NPr) # "Denormalizes global properties.".
:- pred norm_goal_props(+Props,-PropList,-NPr) # "Denormalizes global properties.".

%% Needs to be improved?
norm_goal_props(true,[],_) :-
    !.
norm_goal_props((GP,GPs),[NGP|NGPs],NPD) :-
    !,
    norm_goal_prop(GP,NGP,NPD),
    norm_goal_props(GPs,NGPs,NPD).
norm_goal_props(GP,[NGP],NPD) :-
    !,
    norm_goal_prop_(GP,NGP,NPD).

norm_goal_prop_(M:GP,M:NGP,NPD):- !,
    norm_goal_prop(GP,NGP,NPD).
norm_goal_prop_(GP,NGP,NPD):-
    norm_goal_prop(GP,NGP,NPD).

:- doc(norm_goal_prop(Prop,NProp,NPr), "@var{Prop} is a
   term describing a global property in an assertion. @var{NProp}
   is its normalized version, where @var{NPr} is the extra argument.
   E.g., @tt{norm_goal_prop(regtype,regtype(p(X)),p(X))}.").

:- pred norm_goal_prop(+Prop,-NProp,+NPr) # "Normalizes a global property.".
:- pred norm_goal_prop(-Prop,+NProp,-NPr) # "Denormalizes a global property.".

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

:- pred denorm_goal_prop(+NProp,-Prop,-NPr) # "Denormalizes a global property.".

denorm_goal_prop(NGP,GP,NPD) :-
    norm_goal_prop(GP,NGP,NPD).


norm_addhv_props([],_,[]).
norm_addhv_props([P0|Ps0],PD,Ps) :-
    norm_addhv_prop(P0,PD,Ps,PsT),
    norm_addhv_props(Ps0,PD,PsT).

% TODO: (JF) keep as meta-predicate or context annotation for 'native_props:mshare'/1

:- use_module(library(terms_vars), [varset/2]).
norm_addhv_prop(mshare(Sh),PD,[mshare(Vs,Sh)|T],T) :- !,
    varset(PD,Vs). % TODO: or maybe varset(Sh,Vs)?
% TODO: ensure Sh sorted w.r.t Vs?
norm_addhv_prop(ivar(X),PD,[var(X),indep(CP)|T],T) :- !,
    varset(PD,Vs0),
    delete(Vs0,X,Vs),
    cross_product([[X],Vs],CP).
norm_addhv_prop(P,_,[P|T],T).
