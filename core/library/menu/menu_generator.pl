:- module( menu_generator , 
	             [
		      menu/1,
		      menu/2,
		      menu/3,
		      menu/4,
		      get_menu_flag/3,
		      set_menu_flag/3,
		      space/1,
		      get_menu_configs/1,
		      save_menu_config/1,
		      remove_menu_config/1,
		      restore_menu_config/1,
		      show_menu_configs/0,
		      show_menu_config/1,
		      
		      get_menu_options/2, % -> for JavaScript code!
		     
		      get_menu_flags/1,
		      restore_menu_flags_list/1,
		      get_menu_flags/2,
		      restore_menu_flags/2,

		      generate_js_menu/1,
		      eq/3,
		      neq/3,
		      uni_type/2,
		      vmember/2

% ,remove_less_restrictive_guards/3
% ,guard_included_in_path/3
% ,join_guard_and_path/3
		     ], 
		     [ hiord, 
		       assertions,
		       regtypes,
		       argnames,
		       persdb,
		       nortchecks
		     ] ).

:- include( library( 'menu/menu_common' ) ).

member_and_remove( A , [A|As] , As ) :-
	!.
member_and_remove( A , [B|As] , [B|Bs] ) :-
	member_and_remove( A , As , Bs ).


:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(library(messages)).

:- use_module(library(prompt)).
:- use_module(library(lists), [
	                           reverse/2, 
	                           append/3,
				   length/2,
				   sublist/2
				  ] ).

:- data      menu_flag/3.


persistent_dir( menudbdir , '~/.ciao.d/menu_flags' ).

:- persistent( menu_config/2 , menudbdir ).



%:- multifile pp_flag/2 , 
%	     valid_flag_values/2,
%	     pp_flag/1.
%
% There was a bug: One time we tried to create a menu for mecce. We
% changed some preprocess_flags predicates to multifiles. Then we
% discover a bug in Ciao, that didnt allow us to execute code form
% multifiles. So, we have to retract the changes. Unfortunately, I
% forgot to comment the multifiles lines in menu_generator, also, in
% preprocess_flags (exporting this "new-multifiles" predicates). When
% menu_generator was trying to find the possible options of a flag (it
% uses valid_flags_values/2), the predicate fails (it was not
% multifile), so it tried with valid_flag_value/2 (note there is no 's' at
% the end) and a findall... you can imagine the rest.


:- trust pred menu_flag( Menu , Flag , Value ) :
	term * atom * term

# "This predicate is internal and stored the menu configuration.
 @var{Menu} is the menu, as defined in @pred{menu_opt/6} and
 @var{Flag} is the flag value stored. @var{Value} is the value
 stored.".




:- trust pred menu_default( Menu , Flag , DefaultValue ) :
	term * atm * atm

# "@var{Menu} is a term that has to correspond with the 1st argument
  of @pred{Menu}. @var{Flag} is the desired flag to have a default
  value. @var{DefaultValue} is the default value of @var{Flag}.".


:- trust pred menu_default( Menu , Flag , DefaultValue )  => 
	atm  * atm * atm.


:- trust pred menu_default( Menu , Flag , DefaultValue ) :
	atm  * var * var

# "This call mode can be used to ask which flags and its values
  has a menu @var{menu}".


:- trust pred menu_default( Menu , Flag , DefaultValue ) :
	atm  * atm * var

# "This call mode can be used to ask which value have the flag
  @var{Flag} in the menu @var{menu}".




:- pred menu_opt( Menu , Flag , Text , Guard , BeforePrinting , SelectedHook ) :
	term * atm * atm * callable * callable * callable

# "@var{Menu} is a term that specifies the menu name. It can be an
  atom or just a predicate of arity 1, where the 1st argument
  indicates the menu level (i.e., ana(1) is the level 1 of 'ana'
  menu). @var{Flag} is the flag that will be asked. 

  @var{Text} is the test that will be printed when asking the
  @var{Flag}. 

  @var{Guard} is a predicate of arity 1 that is invoked to see if the
  flag should be asked. The argument is the selected menu options till
  moment in the way: [flag1=value1, flag2=value2, ...]. 

  @var{BeforePrinting} is a predicate of arity 0, that is invoked
  whenever the menu option has been selected the validator menu
  options chooser.

  @var{SelectedHook} is a predicate of arity 2, that is invoked
  whenever the flag has been selected by the user. The 1st argument
  are the current selected values, including the current flag, and in
  the 2nd argument the possible modified list is expected.

  In summary, if @var{Guard} holds, then @var{BeforePrinting} is
  executed (no action is taken whether it fails or not), and after the
  user has types the option @var{SelectedHook} is invoked.".


:- trust pred menu_opt( Menu , Flag , Text , Guard , BeforePrinting ,
	SelectedHook ) : term * term * term * term * term *term.


:- trust pred menu_config( Menu , List ) =>
	atm * list.


:- trust pred menu_config( Menu , List ) :
	atm * list

# "@pred{menu_config/2} is used to store user menu configuration
   permanently.  @var{Menu} is a term that has to correspond with the
   1st argument of @pred{Menu}. @var{List} are the flags saved for
   that menu in the way [flag1=value1, flag2=value2, ...].".


:- trust pred menu_config( Menu , List ) :
	atm * var

# "This call mode can be used to ask the @var{List} flag
  configuation of a menu @var{Menu}.".


%% HOOKS
%% -----

:- pred hook_menu_flag_values( Menu , Flag , Values )
	: atom * atom * var
        => menu_flag_values(Values)

# "It is a hook. It is invoked whenever a menu question is
  printed. @var{Values} is a term which specifies the possible
  values. If @var{Values} is alist(List) -atom list-, then menu will
  check if the typed value by user belongs to List. If @var{Values} is
  a term ask(T,Flag), the menu will invoke
  @pred{hook_menu_check_flag_value/3} hook to check if introduced value is
  valid.".

:- export(menu_flag_values/1).
:- regtype menu_flag_values( X ) # "Flag values".

menu_flag_values( alist( X ) ) :- list( X , atm ).
menu_flag_values(        X   ) :- list( X , atm ).
menu_flag_values( ask( T ) ) :- 
	( T == int ; T == nnegint ; T == atom ; T == atm ).


:- pred hook_menu_check_flag_value( M , F , V ) # "It is a hook. It is
invoked whenever the menu needs to check whether the answer introduced
for the menu @var{M} is correct. This happens when
@pred{hook_menu_flag_values/3} returns in its second argument
something different than alist(_).".


:- pred hook_menu_flag_help( M , F , H )

# "It is a hook. It is invoked whenever the user ask for a help
  description, @var{H}, of the flag @var{F} in the menu @var{M}.".


:- pred hook_menu_default_option( M , F , D )

# "It is a hook. It is invoked whenever the menu needs to offer a
  default option to the user in the menu @var{M} and it has not been
  neither introduced before nor specified by @pred{menu_default/3}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEFINITIONS         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- argnames cc( flag , message , guard, pre, post ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% THE OUTPUT FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pred space( N ) : num( N )
# "prints @var{N} spaces.".


space( 0 ) :- !.

space( N ) :-
	N > 0,
	!,
	N1 is N - 1,
	display( ' ' ),
	space( N1 ).

space( _ ).




display_with_spaces( Label , DesiredLen ) :-
	display( Label ),
	display( ':' ),
	atom_length( Label , Len ),
	Rest is DesiredLen - Len - 1,
	space( Rest ).




mul_display( L ) :-
	nmul_display( L , 0 , 79 ).

nmul_display( [A|R] , Init , Max ) :-
	atom( A ),
	!,
	line_position( user_output , Current ),
	atom_length( A , AL ) , 
	(
	    AL + Current > Max
	->
	    nl,
	    space( Init )
	;
	    true
	),
	display( user , A ),
	nmul_display( R , Init , Max ).

nmul_display( [A|R] , _Init , Max ) :-
	list( A ),
	!,
	line_position( user_output , NInit ),
	display( '[' ),
	line_position( user_output , LInit ),
	list_display( A , LInit , Max ),
	nmul_display( R , NInit , Max ).

nmul_display( [_|R] , Init , Max ) :-
	!,
	nmul_display( R , Init , Max ).

nmul_display( [] , _ , _ ).



list_display( [ ] , _ , _ ) :- 
	!.

list_display( [A] , Init , Max ) :- 
	!,
	atom(A),
	line_position( user_output , Current ),
	atom_length( A , AL ) , 
	(
	    AL + Current + 1 > Max
	->
	    nl,
	    space( Init )
	;
	    true
	),
	display( user , A   ),
	display( user , ']' ).

list_display( [A|R] , Init , Max ) :- 
	!,
	atom(A),
	line_position( user_output , Current ),
	atom_length( A , AL ) , 
	(
	    AL + Current + 2 > Max
	->
	    nl,
	    space( Init )
	;
	    true
	),
	display( user , A   ),
	display( user , ', ' ),
	list_display( R , Init, Max ).



:-  pred display_long_atom( A ) : atm
# "@var(A) atom is printed in 80 characters-wide.".


display_long_atom( X ) :-
	atom(X),
	!,
	atom_codes( X , XC ),
	display_long_str( XC ).
display_long_atom( X ) :-
	display_long_str( X ).

display_long_str( XC ) :-
	line_position( user_output , NInit ),
	display_long_atom_n( XC , NInit , 79 ).

display_long_atom_n( X , I , Max ) :-
	append( Y  , [32|XE] , X ),
	!,
	atom_codes( YC , Y ),
	atom_length( YC , AL ) , 
	line_position( user_output , Current ),
	(
	    AL + Current > Max
	->
	    nl,
	    space( I )
	;
	    true
	),	
	display( YC ), display( ' ' ),
	display_long_atom_n( XE , I , Max ).

display_long_atom_n( X , I , Max ) :-
	atom_codes( YC , X ),
	atom_length( YC , AL ) , 
	line_position( user_output , Current ),
	(
	    AL + Current > Max
	->
	    nl,
	    space( I )
	;
	    true
	),	
	display( YC ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% THE MENU ITSELF 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag( multi_arity_warnings, off ).


:- pred menu( M ) # "Like menu( M , true ).".

menu( M ) :- 
	menu( M , true ).


:- pred menu( M , Bool ) # "Like @pred{menu/4} with no selected
options, taking the menu level from the term @var{M} (example: ana(1)
is expert, ana is naive), and using @var{Bool} to decide whether print
help message or not.".

menu( M , B ) :- 
	atom( M ),
	!,
	menu( M , 0 , B ).

menu( M , B ) :- 
	M =.. [F,L],
	!,
	menu( F , L , B ).


:- pred menu( M , Level , Bool ) # "Like @pred{menu/4} with no
selected options.".

menu( A , B, C ) :-
	menu( A , B , C , [] ).


:- pred menu( M , Level , Bool , AlreadySelectedOpts) # "Execute the
menu @tt{X}. @var{Level} specifies the menu level. @var{Bool} decides
whether print the help message. @var{AlreadySelectedOpts} is a list
with the selected options.".

menu( X , Level , Bool , AlreadySelectedOpts ) :-
	atom( X ),
	select_and_ask_menu( X , Level , Bool , AlreadySelectedOpts , Out ),
	reverse( Out , Out2 ),
	restore_menu_flags( X , Out2 ).


select_and_ask_menu( X , Level , Bool , AlreadySelectedOpts , Out ) :-
	findall( cc${flag    => O,
	             message => T,
		     guard   => G ,
		     pre     => PreG,
		     post    => PG }, %(O,T,G,PreG,PG) , 
	         (menu_opt${   menu    => PossM, 
			       flag    => O,
			       message => T,
			       guard   => G,
			       pre     => PreG,
			       post    => PG },
		  is_good_menu( PossM , X , Level )),
		 Menu ),
	Menu \== [],
	exec_guard( Menu , AlreadySelectedOpts , NoPosibles , Posibles , [] ),
	( Bool == true -> display( '\n\n\t\t\t(Press h for help)\n\n' ) ; true ),
	ask_menu( Posibles , NoPosibles , X , AlreadySelectedOpts , Out ).


:- set_prolog_flag( multi_arity_warnings, on ).



is_good_menu( M , Functor , Level ) :-
	M =.. [Functor , L1 ],
	!,
	L1 =< Level.

is_good_menu( M , Functor , _ ) :- 
	functor( M , Functor , 0 ).


% If the flag already exists in Entry then we have to remove the
% question
exec_guard( [CC | AR] , Entry , NoPos , Pos , A ) :- 
	CC= cc${ flag => F},
	member( F=V , Entry ),
	nonvar( V ),
	!,
	exec_guard( AR , Entry , NoPos , Pos , A ).

exec_guard( [CC | AR] , Entry , NoPos , Pos , A ) :- 
	CC= cc${ guard => G },
	!,
	(
	    G( Entry )
	->
	    Pos   = [ CC | PosR   ],
	    NoPos = NoPosR
	;
	    NoPos = [ CC | NoPosR ],
	    Pos   = PosR
	),
	exec_guard( AR , Entry , NoPosR , PosR , A ).

exec_guard( [] , _ , [] , A , A ).


ask_menu( Pos , NoPos , CurrentMenu , Result , Out ) :-
	(
	    member_and_remove( ask_menu( M ) , Result , Res ), L = 0
	; 
	    member_and_remove( ask_menu( M , L ) , Result , Res )
	),
	!,
	select_and_ask_menu( M , L , false , [changed_to_menu(M)|Res] , Result2 ),
	functor( CurrentMenu , CMF , _ ),
	ask_menu( Pos , NoPos , CurrentMenu , [changed_to_menu(CMF)|Result2] , Out ).

ask_menu( [ CC | PR ] , NoPos , CurrentMenu , Result , Out ) :-
	CC = cc${ flag => F  , message => Label },
	member( F = V , Result ),
	nonvar( V ),
	!,
	note_message( "~w: [~w] Selected.", [Label,V] ),
	ask_menu( PR , NoPos , CurrentMenu , Result , Out ).

ask_menu( [ CC | PR ] , NoPos , CurrentMenu , Result , Out ) :-
	CC = cc${ flag => O , message => Label , pre => PreG , post => PG },
	get_menu_options( CurrentMenu , O , OptsListArg ),
	get_menu_flag( CurrentMenu , O , Default  ),
	
	repeat ,
	( PreG -> true ; true ),
	( ground(Default)
	-> 
	   (
	       atom( Default ) 
	   ->
	       DEF = Default 
	   ;
	       name( Default   , _DEF1 ),
	       atom_codes( DEF , _DEF1 )
	   ),
	   atom_concat( ' (' , DEF    , DD1 ),
	   atom_concat( DD1  , ') ? ' , DefPrn  )
	;
	   DefPrn = '?'
	),
	(
	    ( ( list( OptsListArg ), OptsList = OptsListArg
	      ; OptsListArg = alist( OptsList ) ),
	    OptsList \== [] )
	->
	    display_with_spaces( Label , 32 ),
	    mul_display( [' ',OptsList, DefPrn] )
	;
	    display_with_spaces( Label , 32 ),
	    display( DefPrn )
	),
	ask_option( OptsListArg , Default , CurrentMenu , Opt ),
	(
	    Opt == h
	->
	    display( '\nHelp for flag ''' ),
	    display( O ) , 
	    display( ''':' ),
	    nl,
	    ( hook_menu_flag_help( CurrentMenu , O , Help ) 
	    -> true 
	    ; Help = "No Help Available" ),
	    nl,
	    display_long_atom( Help ),
	    nl,nl,
	    fail
	;
	    true
	),
	NewResult_ = [ O=Opt | Result ],
	PG( NewResult_ , NewResult ),
	% to caught errors. Should be check( ground( NewResult ) ).
	ground( NewResult ),
	exec_guard( NoPos , NewResult , NewNoPos , NewPos , PR ),
	ask_menu( NewPos , NewNoPos , CurrentMenu , NewResult , Out ),
	!.

ask_menu( [] , _ , _ , Out , Out ).
	


ask_option( [Opt] , _ , _ , Opt ) :-
	    !,
	    display( '[Automatically Selected]\n' ).
	
ask_option( OptsList  , Default , M , Opt ) :- 
	( OptsList = ask( int     , F ) ; 
	  OptsList = ask( nnegint , F ) ;
	  OptsList = ask( atom    , F ) ;
	  OptsList = ask( atm     , F ) ),
	!,
	prompt_for_default( Opt , Default ),
	hook_menu_check_flag_value( M , F , Opt ).

ask_option( OptsListArg , Default , _ , OptOut ) :-
	( OptsListArg = alist(OptsList) 
	; OptsListArg = [_|_],
	  OptsListArg = OptsList ),
	!,
	%% Possible default option,
        prompt_for_default( Opt , Default ),
	( 
	    OptsList == []
	-> 
	    OptOut = Opt
	;
  	    closest_option( [h|OptsList] , Opt , OptOut )
	).




closest_option( List , Opt , OptOut ) :-
	atom_sub_member( List , Opt , PosibleOut ),
	(
	    check_for_exact_match( PosibleOut , Opt , OptOut )
	->
	    true
	;
	    (
		PosibleOut == []
	    ->
	        note_message( "Incorrect Option" , [] )
	    ;
	        note_message( "Please be more specific. Posible Options: ~w",
                              [ PosibleOut ] )
	    ),
	    fail
	).


%
% This are the posible cases:
%
% * INPUT:
%   [... aab, aa ... ]  Opt = aa
%   OUTPUT: [aab,aa], as the output is not a list of
%           1 element => we can think it is bad.
%
% SOLUTION: exact match. If there is any option what 
%           exactly match with the Opt, then is it the one selected.
%
atom_sub_member( [] , _ , [] ).

atom_sub_member( [A|As] , Opt , [A|Os] ) :-
	atom_concat( Opt , _ , A ),	    
	!,
	atom_sub_member( As , Opt , Os ).

atom_sub_member( [ _ | As ] , Opt , Out ) :-
	atom_sub_member( As , Opt , Out ).




check_for_exact_match( [ A ] , _  ,  A  ).

check_for_exact_match(  A , Opt , Opt ) :-
	member( Opt , A ).



:- pred restore_menu_flags( M , F )
	: (atom(M), list(F))

# "Restore the flag of the menu @var{M}. @var{F} is a list of terms F=V,
  which indicate the flag (F) and the value (V). @var{M} is the target
  menu to which those flags \"belong\". Additionally, @var{F} can
  contains terms like @tt{changed_to_menu(NM)} that will put NM as the
  new target menu.".

restore_menu_flags( _Menu , [ changed_to_menu(M) | Fs ] ) :-
	!,
	restore_menu_flags( M , Fs ).

restore_menu_flags( Menu , [ F=V | Fs ] ) :-
	!,
	set_menu_flag( Menu , F , V ),
	restore_menu_flags( Menu , Fs ).

restore_menu_flags( _ , [] ).



:- pred set_menu_flag( M , F , V )
	: (atom(M), atom(F), var(V))

# "Set the value @var{V} of the flag @var{F} in the menu (-branch)
  @var{M}.".

set_menu_flag( Menu , F , V ) :-
	nonvar(V),
	functor( Menu , NMenu , _ ),
	(data_facts:retract_fact( menu_flag( NMenu , F , _ ) )->true;true),
	data_facts:asserta_fact( menu_flag( NMenu , F , V ) ).


:- pred get_menu_flag( M , F , V )
	: (atom(M), atom(F), var(V))

# "Returns the value in @var{V} of the flag @var{F} in the menu
  (-branch) @var{M}.".

% There is already and option saved
get_menu_flag( Menu , F , V ) :-
	functor( Menu , NMenu , _ ),
 	data_facts:current_fact( menu_flag( NMenu , F , Value ) ),
 	!,
	Value = V.

% Lets try with the "flat" value (menu of level 0)
get_menu_flag( Menu , F , V ) :-
	functor( Menu , NMenu , _ ),
	menu_default( NMenu , F , Value ),
	!,
	V = Value.

get_menu_flag( Menu , F , V ) :-
	hook_menu_default_option( Menu , F , V ).


%-----------------------------------------------------------------------------

%% HOOKS-GLUE
%% ----------

:- push_prolog_flag(multi_arity_warnings,off).

:- pred get_menu_options( Flag , V )
	: (atom(F))

# "Returns possilbe options in @var{V} by fail for the flag
  @var{Flag}.".

get_menu_options( Flag ,  V  ) :-
	get_menu_options( _ , Flag ,  V  ).

get_menu_options( Menu , Flag ,  V  ) :-
	hook_menu_flag_values( Menu , Flag , Values ),
	( Values = ask(A) 
	-> V = ask( A , Flag )
	; V = Values ).

:- pop_prolog_flag(multi_arity_warnings).


%%%%%
%%%%%  TO SAVE MENU FLAGS
%%%%%

%%------------------------------------------------------------------------
%:- entry save_menu_config(Name) : atm.


:- pred save_menu_config(Name) 
	: atm
	# "Save the current flags configuration under the @var{Name} key.".

% Here we have an especification problem. We decided to have 2 (or more)
% menu levels. Basic levels hide some options from the user, while
% advance level show them up. The question is: if we modify a value in
% advance level, do we want that value appear as modified when using
% basic level again?
%
% Example:
% a => 1 , 2 , 3
% a(1) => AA , BB , CC
% b => 10 , 20 , 30
%
% a(1) is the advance option of a.
% by default:
%  * a    is set to 1.
%  * a(1) is set to CC. 
%  * b    is set to 10.
%
%
% if we execute basic level on a 1st instance (everything on default
% values), we will have a = 1 , a(1) = CC, b = 10.
%
% Now, we execute advance menu, and we change a(1) to BB. Again, we
% execute basic level, so a(1) option is not showed. What value does it
% have, CC or BB?

save_menu_config( Name ) :-
	get_menu_flags(  L ),
	% DTM: Not really sure if we want to do this. Read explication.
	save_menu_flags_list( Name , L ).


save_menu_flags_list( Name , List ) :-
	( persdb_rt:retract_fact( menu_config( Name , _ ) ) , fail ; true ),
	persdb_rt:assertz_fact( menu_config( Name , List ) ).



%:- entry remove_menu_config(Name) : atm.

:- pred remove_menu_config(Name) 
	: atm
	# "Remove the configuration stored with the @var{Name} key
          (the same provided in @pred{save_menu_config/1}).".


remove_menu_config( Name ) :-
	persdb_rt:retract_fact( menu_config( Name , _ ) ),
	fail.

remove_menu_config( _Name ).



%:- entry restore_menu_config( Name ) : atm.

:- pred restore_menu_config(Name) 
	: atm
	# "Restore the configuration saved with the @var{Name}
          key (the same provided in @pred{save_menu_config/1}).".


restore_menu_config( Name ) :-
	menu_config( Name , L ),
	restore_menu_flags_list( L ).


:- pred restore_menu_flags_list(L) 
	: list
	# "Restores menu flags. @var{L} is a list of tuple (M,F,V)
          where M is the menu, F is the flag, and V is the value of
          the flag F in the menu M.".

%What happends with non existing flags?
restore_menu_flags_list( [] ) :- !.
restore_menu_flags_list( [(A1,A2,A3)|As] ) :-
	 (set_menu_flag( A1 , A2 , A3 )->true;true),
	 restore_menu_flags_list( As ).




:- pred get_menu_configs( X ) : var( X ) => list( X , atom )

# "Returns a list of atoms in @var{X} with the name of stored
  configurations.".

get_menu_configs( L ) :-
	findall( Name , menu_config( Name , _ ) , L ).
	



:- pred show_menu_configs 
	# "Show all stored configurations.".


show_menu_configs :-
	get_menu_configs( L ),
	show_menu_list( L ).


show_menu_list( [] ) :- !.
show_menu_list( [A|B] ) :-
	write( A ), nl,
	show_menu_list( B ).




:- pred show_menu_config( C )
	: atm
	# "Show specific configuration values pointed by @var{C} key
	(the same provided in @pred{save_menu_config/1}).".

show_menu_config( Name ) :-
	menu_config( Name , F ),
	show_config_list( F ).




show_config_list( [] ).
show_config_list( [ (A1 , A2 , A3) |As] ) :-
	display( 'Menu: ' ) , 
	display( A1 ),
	display( '   ' ),
	display( A2 ),
	display( ' = ' ) ,
	display( A3 ), nl,
	show_config_list( As ).




:- set_prolog_flag( multi_arity_warnings, off ).

:- pred get_menu_flags( L ) : (var(L)) => list( L ) 

# "Return a list @var{L} of all current menu flags, composed by terms
with the form (M,F,V), where M is the menu, F the flag, and V the
value.  This list can be used as argument of
@pred{restore_flags_list/1}".


get_menu_flags( L ) :-
	findall( (AF,B,C) , 
	         (menu_opt${ menu => A, flag => B } ,
		  functor( A , AF , _ ),
		  get_menu_flag( A , B , C )) , 
		  L ).



:- pred get_menu_flags( M , L ) 
	:  (term(M), var(L)) 
        => list( L ) 

# "Return a list @var{L} of the current menu @var{M} composed by terms
with the form (F=V), F the flag, and V the value.  This list can be
used as argument of @pred{restore_menu_flags/2}".

get_menu_flags( M , L ) :-
	functor( M , MF , _ ),
	findall( (B=C) , 
	         (menu_opt${ menu => A, flag => B } ,
		  functor( A , MF , _ ),
		  get_menu_flag( A , B , C )) , 
		  L ).

:- set_prolog_flag( multi_arity_warnings, on ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        FOR GENERATING THE JS CODE         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate write_in_imperative( pred(1) , SOL ).
:- data several_sols/1.

several_sols( [] ).

generate_imperative_guard( '$:'('PA'(true,_,_)) , [] ) :-
	!.
generate_imperative_guard( P , SOL ) :-
	P( X ),
	X = [Y|_],
	(
	    % For security
	    var( Y )
	->
   	    data_facts:retract_fact( several_sols( SOL ) )
	;
	    (list( X )->true;true),
	    (data_facts:retract_fact( several_sols( L ) ) -> true ; L = [] ),
	    data_facts:asserta_fact( several_sols( [X | L] ) ),
	    fail
	),
	!.
generate_imperative_guard( _ , SOL ) :-
	data_facts:retract_fact( several_sols( SOL ) ).



clean_imperative_guard( G , _Menu , PostClean ) :-
	%clean_imperative_guard__( G , Menu , PreClean ),
	G = PreClean,
	remove_true_from_guard( PreClean , Clean ),
	remove_menu_level_from_guard( Clean , Clean1 ),
	length( Clean1 , L ),
	remove_less_restrictive_guards( L , Clean1 , PostClean ).


%% We do have to remove this in order to make it portable!
remove_menu_level_from_guard( [] , [] ).
remove_menu_level_from_guard( [menu_level=naive|As] , ARs ) :-
	!,
	remove_menu_level_from_guard( As , ARs ).
remove_menu_level_from_guard( [A|As] , [AR|ARs] ) :-
	list(A),
	!,
	remove_menu_level_from_guard( A, AR ),
	remove_menu_level_from_guard( As , ARs ).
remove_menu_level_from_guard( [A|As] , [A|ARs] ) :-
	remove_menu_level_from_guard( As , ARs ).


remove_less_restrictive_guards( 0 , Clean , Clean ) :- 
	!.

remove_less_restrictive_guards( N , [A|As] , B ) :-
	sublistlist( As , A ),
	N1 is N - 1,
	!,
	remove_less_restrictive_guards( N1 , As , B  ).

remove_less_restrictive_guards( N , [A|As] , B ) :-
	append( As , [A] , NA ),
	N1 is N - 1,
	remove_less_restrictive_guards( N1 , NA , B  ).



remove_less_restrictive_guards_from_js( []     , []       ).
remove_less_restrictive_guards_from_js( [A|As] , [NA|MAs] ) :-
	A   = menu_js( Flag , T , O , DO , G1 ),
	NA  = menu_js( Flag , T , O , DO , G2 ),
	length( G1 , L ),
	remove_less_restrictive_guards( L , G1 , G2 ),
	remove_less_restrictive_guards_from_js( As , MAs ).


sublistlist( [A | As] , L ) :- 
	( sublist( L , A ) , ! ; sublistlist( As , L ) ).



remove_true_from_guard( [] , [] ).

remove_true_from_guard( [ G | Gs ] , Clean ) :-
	list( G ),
	!,
	remove_true_from_guard( G  , GC  ),
	remove_true_from_guard( Gs , GCs ),
	(GC \== [] -> Clean = [GC | GCs] ; Clean = GCs).

remove_true_from_guard( [ true | Gs ] , GCs ) :-
	!,
	remove_true_from_guard( Gs , GCs ).

remove_true_from_guard( [ G | Gs ] , [ G | GCs ] ) :-
	remove_true_from_guard( Gs , GCs ).



/*
clean_imperative_guard__( [] , _ , [] ).

clean_imperative_guard__( [ [true] | Gs] , Menu , [ [true] | GCs] ) :-
	!,
	clean_imperative_guard__( Gs ,  Menu , GCs ).

clean_imperative_guard__( [ G | Gs] , RMenu , [G | GCs] ) :-
	G = (F=_),
	menu_opt( Menu , F , _ , _ , _ , _ ),
	functor( Menu , RMenu , _ ),
	!,
	clean_imperative_guard__( Gs ,  Menu , GCs ).

clean_imperative_guard__( [ G | Gs] , Menu , Clean ) :-
	list( G ),
	!,
	clean_imperative_guard__( G ,  Menu , GC ),
	clean_imperative_guard__( Gs ,  Menu , GCs ),
	(GC \== [] -> Clean = [GC | GCs] ; Clean = GCs).

clean_imperative_guard__( [ _ | Gs] , Menu , GCs ) :-
	clean_imperative_guard__( Gs , Menu , GCs ).
	
*/


% just a wraper...
write_imperative_guard( [] ) :-
	display( true ),
	!.
write_imperative_guard( [[]] ) :-
	display( true ),
	!.
write_imperative_guard( G ) :-
	write_and_or_guard( G , or ).


write_and_or_guard( [] ,  _ ).
write_and_or_guard( [ G ] , OP ) :-
	length( G , L ),
	!,
	( L > 1 -> display( '(' ) ; true ),
	switch_guard_op( OP , NOP ),
	write_and_or_guard( G , NOP ),
	( L > 1 -> display( ')' ) ; true ).
write_and_or_guard( [ G ] , _ ) :-
	!,
	write_guard_fact( G ).
write_and_or_guard( [ G | Gs ] , OP ) :-
	write_and_or_guard( [ G ] , OP ),
	write_guard_op( OP ),
	write_and_or_guard( Gs , OP ).


switch_guard_op( and , or ).
switch_guard_op( or , and ).


write_guard_op( and ) :- display( ' && ' ).
write_guard_op( or  ) :- display( ' || ' ).

write_guard_fact( (F=V) ) :-
	display( '(v_' ),
	display( F ),
	( 
	    V = neq( A )
	->
	    display( ' != "' ), display( A )
	;
	    display( ' == "' ), display( V )
	),
	display( '")' ).
write_guard_fact( true ) :-
	display( true ).



%%
%% This is  a kludge but nowadays there is no other easy way of solving it.
%%

% :- dynamic menu_node_info/2.

% menu_node( inter_all      ).
% menu_node( inter_optimize ).

% menu_node_translation( opt            , optimize         ).
% menu_node_translation( inter_optimize , optimize         ).
% menu_node_translation( check          , check_assertions ).
% menu_node_translation( ana            , analyze          ).
% menu_node_translation( N              , B                ) :-
% 	functor( N , F , A ),
% 	A > 0,
% 	!,
% 	menu_node_translation( F , B ).
% menu_node_translation( A     , A                ).




is_list_with_no_vars( [] ).
is_list_with_no_vars( [A|B] ) :-
	nonvar( A ),
	is_list_with_no_vars( B ).


:- meta_predicate get_brach_sol( pred(2) , ?, ? ).

:- pred get_branch_sol( P , ASK , REST )

# "For a predicate @var{P} obtained from the argument @tt{post}
multifile @var{menu_opt/7}, it returns the concatenation with another
menu branch, @var{ASK}, and the flag restrictions (values) needed for
calling it in @var{REST}.".

get_branch_sol(P , ASK , REST ) :-
	P( REST, B ),
	(
	    is_list_with_no_vars( REST ),
	    is_list_with_no_vars( B )
	-> 
	    (ASK__ = ask_menu(M,L); ASK__ = ask_menu(M), L=0),
	    member_and_remove( ASK__ , B , ASK_REST ),
	    (
		nonvar( M )
	    -> 
	        true
	    ;
		member( Flag=MM , ASK_REST ),
		MM==M,
		get_menu_options( Flag , Opts ),
		member( M , Opts )
	    ),
	    ASK =.. [M,L]
	; 
	    !,
	    fail
	).


% generate_menu_nodes_info :-
% 	menu_node( M ),
% 	get_menu_options( M , Opt ),
% 	data_facts:asserta_fact( menu_node_info( M , Opt ) ),
% 	fail.
% generate_menu_nodes_info.


:- pred generate_js_menu( DoNotIncludeList )
	: list(DoNotIncludeList)
# "Reads all multifile @pred{menu_opt/6} predicates and writes in
  default output a JavaScript Menu.".

%  generate_js_menu
:- doc( generate_js_menu/1 , "Internal Info. Short description
and general ideas about how the JS menu is generated.

The current model for the JS menu is an array of @tt{menuq} (object in
JS).

@begin{verbatim}
var assert_rtcheck = menus.length ;
var v_assert_rtcheck ;
menus[ menus.length ] = new menuq( 
	""assert_rtcheck"",
        ""Perform Run-Time Checks"",
        ""none,pred,pp_assrt,pp_code"",
        ""none"",
        '((v_menu_level == ""expert"") &&
          (v_inter_all == ""check_assertions""))' ) ;
@end{verbatim}

A variable with the same name as the @em{flag name} is created with
the value of the index of the menu in the array. Another variable with
'v_' (v = value) indicates the value (of the flag) choosed by the user
in the combo-boxes that appear on the webpage (note you should not
read this if you have not seen the webpage working). The object
@tt{menuq} holds several things: the flag name (to find out the index
in the array in some JS functions), the title, the options (notice
there is no space in the options, this is important!), default option
and the guard.

All the problem here is to generate the (JS) guard, because the rest of
information is the same as in @pred{menu_opt/6}. What code does is to
execute the guard in prolog, and obtain a list of the form
[flag=value,flag2=value2...]. Aditionally an element of the list can
be another list, which indicates that the join operator (&& or ||) is
swaped. For example: [a=1,[b=2,c=3],d=4] will be translated in JS like
((a==1)&&((b==2)||(c==3))&&(d==4)).

Prolog Guards (the ones in menu_opt) have been rewritten in order to
do not generate free variables, i.e., they are finite guards now. So
calling them with a variable in its first argument, we get the list
like the named in the previous paragraph. 

% Unfortunately, the current CiaoPP menu does not have all information
% itself to concatenate several menus, i.e., when asking which kind of
% action the user desire, depending on the answer one path or another
% one is taken. Who decide which path? 'auto_inteface' does. We do have
% to take this into account, because generating the assertions of a
% ""subpath"" will produce the activation of it without permision of the
% father (the menu which launch it).

Also, we have to keep in mind that menu has several submenus (or
branches) defined and ones connect with others. For example, the menu

@begin{verbatim}
all, 'Select Menu Level'   # menu_level - naive.
all, 'Select Action Group' # inter_all  - analyze :: all_menu_branch.

check(1), 'Perform Compile-Time Checks' # assert_ctcheck - on.
...

ana     , 'Select Aliasing-Mode Analysis' # modes - shfr <- true. 
@end{verbatim}

defines 3 menus: @tt{all}, @tt{check(1)}, and @tt{ana}. It usually
happends that one menu invoques (connect or continue) with another
menu (then it is a submenu or a branch). When generating the guards we
have to add additional restrictions to the guards in order to make
submenus do not appear in the incorrect moment. For example,
@tt{check(1)} menu will be only active if @tt{menu_level = expert} and
@tt{inter_all=check_assertions} (more on this come later). How are
several menus connected between each other? The process of connecting
the menus is post-processing the selected flags (options) via post
hook (the one defined after :: field). The post-processing hook only
have to add the element @tt{ask_menu(Branch,Level)} or
@tt{ask_menu(Branch)} to the selected flag list (the argument of the
hook). So let us say that when calling all_menu_branch( X , Y ) we
get:

@begin{verbatim}
?- all_menu_branch(A,B).

A = [inter_all=optimize,menu_level=naive|_A],
B = [ask_menu(opt,0),inter_all=optimize,menu_level=naive|_A] ? ;

A = [inter_all=optimize,menu_level=expert|_A],
B = [ask_menu(opt,1),inter_all=optimize,menu_level=expert|_A] ? ;

A = [inter_all=analyze,menu_level=naive|_A],
B = [ask_menu(ana,0),inter_all=analyze,menu_level=naive|_A] ? ;

A = [inter_all=analyze,menu_level=expert|_A],
B = [ask_menu(ana,1),inter_all=analyze,menu_level=expert|_A] ? ;

A = [inter_all=check_assertions,menu_level=naive|_A],
B = [ask_menu(check,0),inter_all=check_assertions,menu_level=naive|_A] ? ;

A = [inter_all=check_assertions,menu_level=expert|_A],
B = [ask_menu(check,1),inter_all=check_assertions,menu_level=expert|_A] ? ;

A = [inter_all=check_certificate,menu_level=_B|_A],
B = [inter_all=check_certificate,menu_level=_B|_A] ? ;

A = [inter_all=optimize,_A,menu_level=naive|_B],
B = [ask_menu(opt,0),inter_all=optimize,_A,menu_level=naive|_B] ? 
@end{verbatim}

Notice that the last option contains free variables in the list (not
taking the tail into account). That is the indicator for us to stop
searching for more solutions. Additionally, we can have more complex
things like:

@begin{verbatim}
?- opt_menu_branch(A,B).

A = [menu_level=naive,inter_optimize=_A|_B],
B = [ask_menu(_A,0),menu_level=naive,inter_optimize=_A|_B] ? ;

A = [menu_level=expert,inter_optimize=_A|_B],
B = [ask_menu(_A,1),menu_level=expert,inter_optimize=_A|_B] ? ;

A = [menu_level=naive,_B,inter_optimize=_A|_C],
B = [ask_menu(_A,0),menu_level=naive,_B,inter_optimize=_A|_C] ? 
@end{verbatim}

In this situation, the menu that will be asked will depend on the
value of the flag @tt{inter_optimize}, so we will have to generate as
many ask_menu as possible values the flag has. The predicate
@pred{generate_menu_path/2} solves all this problem: for a given flag,
it looks up to find out the guard composed by flags that will activate
the flag. 
% This predicate also consider the problem of menu aliasing,
% i.e., the option optimize in inter_ana menu launch opt menu, so
% optimize is an alias for opt or vice-versa. 
For example, for the given flag @tt{ass_not_stat_eval}, the path is:
[v_menu_level=expert, v_inter_all=check_assertions], that means that
@tt{menu_level} flag has to have the value ""expert"" and the
@tt{inter_all} flag has to have the value ""check_assertions"" (note
that menu_level and inter_all defines two menu branches). If we would
execute only the precondition we would get: [v_assert_ctcheck=on].

There are 
% two 
one limitations imposed to the JS menu.
% The menu level indicated by @tt{menu_level} flag, is hirewired. The
% other limitation is more serious.
All flags values in the JS menu are mapped into one set of flags. In
other words, changing a value of a shared flag by two menu branch will
be reflected on the other branch. For example, changing type analysis
in @tt{analyze} menu branch, will modify the value of the same flag in
@tt{check assertions} branch.

The last point to name is about generated JS guards. This world is not
fair, and sometimes happends things you just do not expect. Here is
one of those things. When calling the Prolog guards with the list of
current selected values, the cases that can occur are much less than
when seeing all the possible combinations. Something like
(a=true||a=1)&&(b=2), in Prolog guard execution (with instantiated
things) will mean: the selected options are not nil, and a=1 and b=2,
or in other words: (a==1)&&(b==2). But in JS, unfortunately means:
b=2. Nowadays, this problem is solved by
@pred{clean_imperative_guard/2} which tries to remove all stupid
@tt{true} conditions, but I am sure Murphy is listening to me now and
he started to create an user to make the call
@pred{clean_imperative_guard/2} generates wrong answer.").

generate_js_menu( DoNotInclude ) :-
	findall( menu_opt( Type , Flag , Title , Guard , B4_Print , AfterSel ),
	         (menu_opt( Type , Flag , Title , Guard , B4_Print , AfterSel ),
		  \+ member( Flag , DoNotInclude )
		 ),
		 L ),
%	data_facts:retractall_fact( menu_node_info( _ , _ ) ),
%	generate_menu_nodes_info,
	data_facts:retractall_fact( menu_path( _ , _ , _ ) ),
	generate_menu_path_offline( L ),
	generate_menu_path_offline_kleene( L ),
	generate_js_guards( L , Js_L ),
	proyect_ys_guards( Js_L , [] , Js_PL ),
	remove_less_restrictive_guards_from_js( Js_PL , Js_PL2 ),
	write_js_code( Js_PL2 ).


% For testting...
% write_js_code( [] ).
% write_js_code( [ M | Ms ] ) :-
% 	M = menu_opt( Menu , Flag , Title , Guard , _B4_Print , _AfterSel ),
% 	generate_menu_path( Menu , Path ),
% 	display( path_of( Menu ,  Path ) ), nl,
% 	write_js_code( Ms ).


:- data menu_path/3.

generate_menu_path_offline( Menu ) :- 
	M = menu_opt${ post => AfterSel },
	member( M , Menu ),
	get_branch_sol( AfterSel , A , B ),
	data_facts:asserta_fact( menu_path( A , B , no ) ),
%	display( menu_path( A , B ) ),nl,
	fail.
generate_menu_path_offline( _ ).


generate_menu_path_offline_kleene( Menu ) :- 
	% Take a semi-path (not from original node. Ex: slide <- optimize=slice
	data_facts:current_fact( menu_path( A , B , no ) , Ref ),
	
	% Look which node has that flags
	member( Flag=_ , B ),

	M = menu_opt${ menu => Node , flag => Flag , post => Post },
	member( M , Menu ),
	
	% (Check that it is a node)
	\+ Post = '$:'('PA'(true,_,_)),

%	display( inspecting( A ,Flag,Node ) ),nl,

	% Get the path to that node
	generate_menu_path( Node , Path ),
	data_facts:erase( Ref ),
	append( B , Path , BPath1 ),
	remove_menu_level_from_guard( BPath1 , BPath ),
	data_facts:asserta_fact( menu_path( A , BPath , yes ) ),
%	display( juntion_with( Flag , Node , Path ) ),nl,
%	display( new_menu_path( A , BPath ) ),nl,nl,
	fail.
generate_menu_path_offline_kleene( _ ) :-
	data_facts:retract_fact( menu_path( A , B , no  ) ),
	data_facts:asserta_fact( menu_path( A , B , yes ) ),
%	display( keep_menu_path( A , B ) ),nl,
	fail.
generate_menu_path_offline_kleene( _ ).


generate_menu_path( M , Path ) :-
	atom( M ),
	!,
	Menu =.. [M,0],
	generate_menu_path( Menu , Path ).
generate_menu_path( M , Path ) :-
	menu_path( M , Path , _ ),
	!.
generate_menu_path( _ , [] ).

% generate_menu_path( M , [ (menu_level = expert) | Path ] ) :-
% 	functor( M , F , A ),
% 	A > 0,
% 	!,
% 	generate_menu_path( F , Path ).
% generate_menu_path( M , Path ) :-
% 	menu_node_info( N , Info ),
% 	menu_node_translation( M , MT ),
% 	%% Do consider the expert mode
%         member( MT, Info ),
% 	generate_menu_path( N , N_Path ),
% 	Path = [ (N = MT) | N_Path ],
% 	!.
% generate_menu_path( _ , [] ).



generate_js_guards( [] , [] ).
generate_js_guards( [ M | Ms ] , [ J | Js ] ) :-
	M = menu_opt( Menu , Flag , Title , Guard , _B4_Print , _AfterSel ),
	J = menu_js( Flag , Title , Options , Def_Opt , GUARD ),

	% Generate the path to see how to get this menu
	generate_menu_path( Menu , Path__ ),
	remove_menu_level_from_guard( Path__ , Path ),

	% Options
	get_menu_options( Flag , Options ),

	% Default Options
	get_menu_flag( Menu , Flag , Def_Opt ),
	
%	display( flag( Flag ) ), nl,
	
	generate_imperative_guard( Guard , IG ),
	functor( Menu , RMenu , _ ),
	clean_imperative_guard( IG , RMenu , IGC ),

%  	display( antes_clean( IG ) ),nl,
%  	display( despe_clean( IGC ) ),nl,
%  	display( path_______( Path ) ),nl,nl,

	join_guard_and_path( IGC , Path , GUARD ),
% 	( IGC  = [] -> S_IG  = true ; S_IG  = IGC ),
% 	( Path = [] -> GUARD = IG   ; 
%            (S_IG = true -> GUARD = [Path] ; GUARD = [[[Path], S_IG ]] )),
	generate_js_guards( Ms , Js ).



write_js_code( [] ).
write_js_code( [ M | Ms ] ) :-
	M = menu_js( Flag , Title , Options , Def_Opt , GUARD ),

	% Translation Table
	display( 'var ' ),
	display( Flag ),
	display( ' = menus.length ;\n' ),
	
	% Flags Values
	display( 'var v_' ),
	display( Flag ),
	display( ' ;\n' ),
	
	% Menu
	display( 'menus[ menus.length ] = new menuq( "' ),
	display( Flag ),
	display( '",\n                                   "'),
	display( Title ),
	display( '",\n                                   "'),

	% Options
	display_options( Options ),
	display( '",\n                                   "'),
	display( Def_Opt ),
	display( '",\n                                   \''),
	write_imperative_guard( GUARD ),
	display( '\' ) ;\n\n'),
	write_js_code( Ms ).


proyect_ys_guards( [] , A  , A ).
proyect_ys_guards( [ M | Ms ] , Processed  , Out ) :-
	join( Processed , M , Rest ),
	proyect_ys_guards( Ms , Rest, Out ).


join( Processed , M , Out ) :-
	M  = menu_js( Flag , T , O , DO , G1 ),
	AS = menu_js( Flag , _ , _ , _  , G2 ),
	append( B4 , [ AS | After ] , Processed ),
	!,
	(
	    G1 = [ G11 | _ ],
	    list( G11 )
	->
            append( G1 , G2 , G21 )
	;
	    G21 = [ G1 , G2 ]
	),
	AT = menu_js( Flag , T , O , DO , G21 ), % [ [G1] , [G2] ] ),
	append( B4 , [AT | After] , Out ).
join( Processed , M , C ) :-
	append( Processed , [M] , C ).
	

% Things like this happends
% collapse_ai_vers: case 1
% G:[[inter_all=analyze],[inter_all=check_assertions,check_config_ana=on]]
% P:[inter_all=analyze, menu_level=naive]
%
% collapse_ai_vers: case 2
% G:[[inter_all=analyze],[inter_all=check_assertions,check_config_ana=on]]
% P:[inter_all=check_assertions,menu_level=expert]
%
% So conditions in the Guard that contains (and are no incompatible)
% conditions in the Path, has to be completed with the conditions in the
% Path. So:
%
% collapse_ai_vers: case 1
% [[inter_all=analyze, menu_level=naive],
%  [inter_all=check_assertions,check_config_ana=on]]
%
% collapse_ai_vers: case 2
% [[inter_all=analyze],
%  [inter_all=check_assertions,menu_level=expert,check_config_ana=on]]
%
% When joining:
% [[inter_all=analyze, menu_level=naive],
%  [inter_all=check_assertions,check_config_ana=on]
%  [inter_all=analyze],
%  [inter_all=check_assertions,menu_level=expert,check_config_ana=on]]
%
% After cleaning (conditions included in other are removed):
%
% [[inter_all=analyze, menu_level=naive],
%  [inter_all=check_assertions,menu_level=expert,check_config_ana=on]]

join_guard_and_path( [] , P  , [P] ).
join_guard_and_path( G  , [] , [G] ).
join_guard_and_path( G  , P  , GP  ) :-
	join_guard_and_path__( G  , P  , GP ).


join_guard_and_path__( [] , _  , [] ).
join_guard_and_path__( [G1|Gs] , P  , [NG|NGs] ) :-
	guard_included_in_path( G1 , P , NG ),
	!,
 	join_guard_and_path__( Gs , P  , NGs ).
join_guard_and_path__( [G1|Gs] , P  , [G1|NGs] ) :-
 	join_guard_and_path__( Gs , P  , NGs ).


	
% join_guard_and_path([[inter_all=analyze],[inter_all=check_assertions,check_config_ana=on]],[inter_all=analyze, menu_level=naive], A ).


guard_included_in_path( [] , P , P ).
guard_included_in_path( [G|Gs] , P , NG ) :-
	G = (Flag=Value),
	member( (Flag=V), P ),
	!,
	V == Value,
	guard_included_in_path( Gs , P , NG ).
guard_included_in_path( [G|Gs] , P , [G|NP] ) :-
	guard_included_in_path( Gs , P , NP ).
	




display_options( ask( A , _ ) ) :-
	!,
	display( A ).
display_options( Options ) :-
	display_olist( Options ).


display_olist( [] ).
display_olist( [A] ) :-
	!,
	display( A ).
display_olist( [ A | As ] ) :-
	display( A ),
	display( ',' ),
	display_olist( As ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  THIS IS NEEDED FOR WRITING THE GUARDS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred eq( Type , A , B )

# "@var{Type} is the value returned by the 2nd arg of
  @pred{uni_type}. @var{A} and @var{B} are whatever terms. This
  predicate success if they are equal (like A=B).".

eq( true  , V , V ).
eq( false , Y , V ) :-	Y == V.


:- pred neq( Type , A , B )

# "@var{Type} is the value returned by the 2nd arg of
  @pred{uni_type}. @var{A} and @var{B} are whatever terms. The
  semantic is similar to A \== B.".

neq( true  , [true|_] , [] ) :- !.
neq( false , Y      , [] ) :- !, Y \== [].
neq( true  , neq(V) , V ).
neq( false , Y      , V ) :- Y \== V.


:- pred uni_type( Var , Type )

# "@var{Var} should be the argument passed to the menu
  guard. @var{Type} is an abstract type that decides how unifications
  should be done in @pred{eq/3} and @pred{neq/3}.".

uni_type( X , true ) :-
	( var( X )  ;  member( Y , X ), var( Y )),
	!.
uni_type( _ , false ).


/*
val(K,V,[K=V1|_]) :-
	( V == V1 -> ! ; V = V1 ).
val(K,V,[I|Is]) :-
	nonvar(I),
	val(K,V,Is).
*/

:- pred vmember( Var , List )

# "It is @pred{member} equivalent predicate to be used in guards.".


vmember(K=neq(V),[K=V1|_]) :-
	V == V1,
	!,
	fail.
vmember(K=V,[K=V1|_]) :-
	nonvar( V1 ),
	V1 = neq(A),
	A  == V,
	!,
	fail.
vmember(K=V,[K=V1|_]) :-
	( V == V1 -> ! ; V = V1 ).
vmember(K,[I|Is]) :-
	nonvar(I),
	vmember(K,Is).


%% PROBAR:

%% join( [menu_js( a , _ , _ , _ , [[[[=(menu_level,naive),=(inter_optimize,spec)]],true]] )], menu_js( a , _ , _ , _ , [[[[=(menu_level,naive),=(inter_optimize,spice)]],true]] ) , X ).

