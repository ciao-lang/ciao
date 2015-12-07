:- module(emacs,[emacs_edit/1,emacs_edit_nowait/1,
	         emacs_eval/1,emacs_eval_nowait/1,
		 elisp_string/1],
  	        [assertions,regtypes,isomodes,fsyntax,hiord]).

%% For checking below...
:- use_module(library(terms_check), [instance/2]).

:- use_module(library(lists), [list_concat/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(process)).

:- doc(title,"Calling emacs from Prolog").

:- doc(author,"The CLIP Group").

:- doc(module,"This library provides a @index{prolog-emacs
   interface}. This interface is complementary to (and independent
   from) the @concept{emacs mode}, which is used to develop programs
   from within the @apl{emacs} editor/environment. Instead, this library
   allows calling @apl{emacs} from a running Prolog program. This
   facilitates the use of  @apl{emacs} as a ``user interface'' for a Prolog
   program. Emacs can be made to:

   @begin{itemize}   

   @item Visit a file, which can then be edited.
 
   @item Execute arbitrary @index{emacs lisp} code, sent from Prolog.
 
   @end{itemize}   

   @noindent
   In order for this library to work correctly, the following is needed: 

   @begin{itemize}   

   @item You should be running the @apl{emacs} editor on the same machine 
         where the executable calling this library is executing. 
 
   @item This @apl{emacs} should be running the @index{emacs server}. This 
         can be done by including the following line in your @file{.emacs} 
         file:  

@begin{verbatim}   
;; Start a server that emacsclient can connect to.
(server-start)
@end{verbatim}   

         @noindent Or typing @tt{M-x server-start} within @apl{emacs}.
         
   @end{itemize}   

   @noindent @bf{Examples:}

   @noindent Assuming that a @tt{.pl} file loads this library, then:

   @begin{description}   

   @item{@tt{..., emacs_edit('foo'), ...}} Opens file @tt{foo} for
   editing in @apl{emacs}.

   @item{@tt{..., emacs_eval_nowait(""(run-ciao-toplevel)""), ...}}
   Starts execution of a Ciao top-level within @apl{emacs}.

   @end{description}   

   ").

%---------------------------------------------------------------------------
:- pred emacs_edit(+filename) 
   # "Opens the given file for editing in @apl{emacs}. Waits for editing to 
      finish before continuing.". 

emacs_edit(File) :-
	check_type(File,filename,'emacs_edit/1',1),
	emacs_edit_file(File,wait).

%---------------------------------------------------------------------------
:- pred emacs_edit_nowait(+filename) 
   # "Opens the given file for editing in @apl{emacs} and continues
      without waiting for editing to finish.". 

emacs_edit_nowait(File) :-
	check_type(File,filename,'emacs_edit_nowait/1',1),
	emacs_edit_file(File,nowait).

%---------------------------------------------------------------------------
:- pred emacs_eval(+elisp_string) 
   # "Executes in emacs the lisp code given as argument. Waits for the 
      command to finish before continuing.". 

emacs_eval(Command) :-
	check_type(Command,elisp_string,'emacs_eval/1',1),
	emacs_eval_expression(Command,wait).

%---------------------------------------------------------------------------
:- pred emacs_eval_nowait(+elisp_string) 
   # "Executes in emacs the lisp code given as argument and continues
      without waiting for it to finish.". 

emacs_eval_nowait(Command) :-
	check_type(Command,elisp_string,'emacs_eval_nowait/1',1),
	emacs_eval_expression(Command,nowait).

%---------------------------------------------------------------------------
:- regtype filename(F) 
   # "@var{F} is an atom which is the name of a file.". 

filename(L) :- atm(L).
%---------------------------------------------------------------------------
:- regtype elisp_string(L) 
   # "@var{L} is a string containing @apl{emacs} lisp code.". 

elisp_string(L) :- string(L).
%---------------------------------------------------------------------------

emacs_edit_file(File,Wait) :-
	emacs_call([File],Wait).

emacs_eval_expression(Command,Wait) :-
	atom_codes(CommandA, Command),
	emacs_call(['-e', CommandA],Wait).

emacs_call(Args0,Wait) :-
	( Wait == wait ->
	    Args = Args0
	; Args = ['--no-wait'|Args0]
	),
	process_call(path('emacsclient'), Args,
	             [stderr(string(Errors)), status(S)]),
	( S = 0, Errors == [] -> true
	; atom_codes(AErrors,Errors),
	  %% delete_file(TmpFile),
	  throw(error(AErrors,emacs/1))
	).

% TODO: This should be imported from (the equivalent at) rtchecks:
check_type(Arg,Type,Pred,ArgN) :-
	\+ \+ system_dependent_disentailed(Type,Arg),
	!,
	%% The quote is just a kludge...
	throw(error(type_error(Type, Arg),^(Pred-ArgN))).
check_type(_,_,_,_).
	
%%% This is all from rt_checks:

% This is correct and useful in any system
system_dependent_disentailed(Prop,Arg):-
	system_dependent_incompatible(Prop,Arg),!.
% This is only correct for complete solvers (such as herbrand)
system_dependent_disentailed(Prop,Arg):-
	copy_term(Arg,OrigArg),
	Prop(Arg),
	!,
	\+(instance(OrigArg,Arg)),
	instance(Arg,OrigArg).

% This definition works in any system
system_dependent_incompatible(Prop,Arg):-
	\+(Prop(Arg)).
