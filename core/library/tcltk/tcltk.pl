:- module(tcltk,
	[
	    tcl_new/1,
	    tcl_eval/3,
	    tcl_delete/1,
	    tcl_event/3,
	    tclInterpreter/1,
	    tclCommand/1,
	    tk_event_loop/1,
	    tk_main_loop/1,
	    tk_new/2,
	    tk_next_event/2
	],
	[assertions,isomodes,regtypes]).

%%----------------------------------------------------------------------
:- doc(title, "The Tcl/Tk interface").

:- doc(author,"Montse Iglesias Urraca").
:- doc(author, "The CLIP Group").
:- doc(address, "@tt{http://www.clip.dia.fi.upm.es/}").
:- doc(address, "Facultad de Inform@'{a}tica").
:- doc(address, "Universidad Polit@'{e}cnica de Madrid").

:- use_module(tcltk_low_level, 
	[
	    new_interp/2,
	    delete/1,
	    tcltk/2,
	    tcltk_raw_code/2,
	    send_term/2,
	    receive_result/2,
	    receive_list/2,
	    receive_event/2,
	    receive_confirm/2
	]).

:- use_module(engine(internals)).
:- use_module(library(write)).
:- use_module(engine(hiord_rt), ['$meta_call'/1]). 

:- set_prolog_flag(multi_arity_warnings, off).

%%------------------------------------------------------------------------
:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary, "This document includes the reference manual of
   the Ciao Prolog @index{Tcl/Tk bidirectional interface}. This
   library provides interfacing between a @em{Tcl/Tk}-based
   @concept{graphical interface} and @em{Prolog} programs.").

:- doc(module,"The @lib{tcltk} library package is a bidirectional
   interface to the @em{Tcl} language and the @em{Tk} toolkit. Tcl is
   an interpreted scripting language with many extension packages,
   particularly the graphical interface toolkit, Tk.  The interaction
   between both languages is expressed in terms of an interface
   between the Tcl/Tk process and the Prolog process. This approach
   allows the development of mixed applications where both sides,
   Tcl/Tk and Prolog, can be combined in order to exploit their
   respective capabilities.

   This library uses two sockets to connect both the Tcl and the
   Prolog processes: @em{event_socket} and @em{term_socket}. There are
   also two Tcl global variables: @em{prolog_variables} and
   @em{terms}. The value of any of the bound variables in a goal will
   be stored in the array @tt{prolog_variables} with the variable name
   as index.  @em{Terms} is the string which contains the printed
   representation of prolog @em{terms}.

               

@noindent @bf{Prolog to Tcl}

    The Tcl/Tk side waits for requests from the Prolog side, and
    executes the Tcl/Tk code received. Also, the Tcl/Tk side handles
    the events and exceptions which may be raised on its side, passing
    on control to the Prolog side in case it is necessary.

    To use Tcl, you must create a @em{Tcl interpreter} object and send
    commands to it. A @em{Tcl} command is specified as follows:

@begin{verbatim}
      Command         --> Atom  @{ other than [] @}
                        | Number
                        | chars(PrologString)
                        | write(Term)
                        | format(Fmt,Args)
                        | dq(Command)
                        | br(Command)
                        | sqb(Command)
                        | min(Command)
                        | ListOfCommands
      ListOfCommands  --> []
                        |[Command|ListOfCommands]
@end{verbatim}

where:

@begin{description}

@item{@tt{Atom}} denotes the printed representation of the atom.

@item{@tt{Number}} denotes their printed representations.

@item{@tt{chars(PrologString)}} denotes the string represented by
     @em{PrologString} (a list of character codes).

@item{@tt{write(Term)}} denotes the string that is printed by the
     corresponding built-in predicate.

@item{@tt{format(Term)}} denotes the string that is printed by the
     corresponding built-in predicate.

@item{@tt{dq(Command)}} denotes the string specified by @em{Command},
     enclosed in double quotes.

@item{@tt{br(Command)}} denotes the string specified by @em{Command},
     enclosed in braces.

@item{@tt{sqb(Command)}} denotes the string specified by @em{Command},
     enclosed in square brackets.

@item{@tt{min(Command)}} denotes the string specified by @em{Command},
     immediately preceded by a hyphen.

@item{@tt{ListOfCommands}} denotes the strings denoted by each element,
     separated by spaces.

@end{description}

The predicates to use Tcl from Prolog are @pred{tcl_new/1},
@pred{tcl_delete/1}, @pred{tcl_eval/3}, and @pred{tcl_event/3}.

@noindent An example of use with Prolog as master and Tcl as slave,
consisting of a GUI to a program which calculates the factorial of a
number:

@noindent
@begin{verbatim}
@includeverbatim{tcltk/doc_files/tcl_factorial.pl}
@end{verbatim}


@noindent @bf{Tcl to Prolog}

    This is the usual way to build a GUI application. The slave,
    Prolog, behaves as a server that fulfills eventual requests from
    the master side, Tcl. At some point, during the user interaction
    with the GUI, an action may take place that triggers the execution
    of some procedure on the slave side (a form submit, for
    example). Thus, the slave is invoked, performs a service, and
    returns the result to the GUI through the socket connection.

    This library includes two main specific Tcl commands:

@begin{description} 
	
 @item{@tt{prolog} @em{Goal}} @em{Goal} is a string containing the
 printed representation of a Prolog goal. The goal will be
 called in the user module unless it is prefixed with another
 module name. The call is always deterministic and its can be
 either of the following:
 
 @begin{description} 
 
 @item{@em{1}, in case of success} The value of any of the variables in
 the goal that is bound to a term will be returned to Tcl in the
 array prolog_variables with the variable name as index.

 @item{@em{0}, if the execution fails}  The Prolog exception Tcl
 exception is raised. The error message will be \"Prolog Exception: \"
 appended with a string representation of such exception.

 @end{description}

 @item{@tt{prolog_event} @em{Term}} Adds the new @em{term} to the
 @em{terms} queue. These can be later retrieved through predicates
 @pred{tcl_event/3} and @pred{tk_next_event/2}.

@end{description}

Additionally, seven extra Tcl commands are defined.

@begin{description}
	
 @item{@tt{prolog_delete_event}} Deletes the first @em{term} of the
 @em{terms} queue.

@item{@tt{prolog_list_events}} Sends all the @em{terms} of the
 @em{terms} queue through the @em{event_socket}. The last element is
 @em{end_of_event_list}.

@item{@tt{prolog_cmd} @em{Command}} Receives as an argument the Tcl/Tk
 code, evaluates it and returns through the @em{term_socket} the term
 @em{tcl_error} in case of error or the term @em{tcl_result} with the
 result of the command executed. If the command is @em{prolog}, upon
 return, the goal run on the prolog side is received. In order to get
 the value of the variables, predicates are compared using the
 @em{unify_term} command.  Returns 0 when the sript runs without
 errors, and 1 if there is an error.

@item{@tt{prolog_one_event} @em{Term}} Receives as an argument the
 @em{term} associated to one of the Tk events. Sends the @em{term}
 through the @em{event_socket} and waits for its unification. Then
 @em{unify_term} command is called to update the
 @em{prolog_variables} array.

@item{@tt{prolog_thread_event} @em{Term}} Receives as an argument the
 @em{term} associated to one of the Tk events. Sends the @em{term}
 through the @em{event_socket} and waits for its unification. Then
 @em{unify_term} command is called to update the @em{prolog_variables}
 array. In this case the @em{term_socket} is non blocking.

@item{@tt{convert_variables} @em{String}} Its argument is a string
 containing symbols that can not be sent through the sockets. This
 procedure deletes them from the input string and returns the new
 string.

@item{@tt{unify_term} @em{Term1} @em{Term2}} Unifies @em{Term1} and
 @em{Term2} and updates the the @em{prolog_variables} array.

@end{description}

The predicates to use Prolog from Tcl are @pred{tk_event_loop/1},
@pred{tk_main_loop/1}, @pred{tk_new/2}, and @pred{tk_next_event/2}.

@noindent An example of use with Tcl as master and Prolog as slave,
implementing the well known \"Hello, world!\" dummy program (more can
be seen in directory examples):

Prolog side:

@noindent
@begin{verbatim}
@includeverbatim{tcltk/doc_files/simple.pl}
@end{verbatim}

Tcl side (simple.tcl):

@noindent
@begin{verbatim}
@includeverbatim{tcltk/doc_files/simple.tcl}
@end{verbatim}

").

%%------------------------------------------------------------------------


:- regtype tclInterpreter(I) # "@var{I} is a reference to a @em{Tcl}
   interpreter.".

tclInterpreter(_).

%%------------------------------------------------------------------------
:- pred tcl_new(-TclInterpreter) :: tclInterpreter # "Creates a new
   interpreter, initializes it, and returns a handle to it in
   @var{TclInterpreter}.".
%%------------------------------------------------------------------------

tcl_new(I) :-
        new_interp(I, []).

%%------------------------------------------------------------------------
:- pred tcl_delete(+TclInterpreter) :: tclInterpreter # "Given a
   handle to a Tcl interpreter in variable @var{TclInterpreter}, it
   deletes the interpreter from the system.".
%%------------------------------------------------------------------------

tcl_delete(I) :-
        delete(I).

%%------------------------------------------------------------------------
:- pred tcl_eval(+TclInterpreter,+Command,-Result) 

        :: tclInterpreter * tclCommand * string

        # "Evaluates the commands given in @var{Command} in the Tcl
          interpreter @var{TclInterpreter}. The result will be stored
          as a string in @var{Result}. If there is an error in
          @em{Command} an exception is raised. The error messages will
          be @em{Tcl Exception:} if the error is in the syntax of the
          Tcl/Tk code or @em{Prolog Exception:}, if the error is in
          the prolog term.".
%%------------------------------------------------------------------------

tcl_eval_result(X, Result, FromModule) :-
        ( member(execute(Goal),[Result]) -> 
	  (catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error)) ->
	   send_term(Goal,X)
	  ;
	   send_term(fail, X)
          )
	;
            true
        ).

:- meta_predicate tcl_eval(?,?,addmodule).
:- impl_defined(tcl_eval/3).

tcl_eval(I, Command, Result, FromModule) :-
	tcltk_raw_code("prolog_set_tcl_eval_mode", I),
        tcltk_raw_code("prolog_cmd {",I),
        tcltk(Command,I),
        tcltk_raw_code("}",I),
	tcl_eval_aux(Command, I, Result, [], FromModule),
	tcltk_raw_code("prolog_unset_tcl_eval_mode", I).

tcl_eval_aux(Command, I, Result, AccResult, FromModule) :-
        (receive_result(Result1,I) ->
	 tcl_eval_result(I, Result1, FromModule),
	 (not_waiting_for_more(Result1) ->
	  AccResult = Result
	 ;
	  tcl_eval_aux(Command, I, Result, Result1, FromModule)
	 )
 	;
 	 true).


not_waiting_for_more(tcl_eval_finished).

:- pred tcl_event(+TclInterpreter,+Command,-Events)

        :: tclInterpreter * tclCommand * list

        # "Evaluates the commands given in @var{Command} in the Tcl
          interpreter whose handle is provided in
          @var{TclInterpreter}. @var{Events} is a list of terms stored
          from Tcl by @em{prolog_event}. Blocks until there is
          something on the event queue".
%%------------------------------------------------------------------------

tcl_event(I,Command,EventList):-
        tcltk(Command,I),
        tcltk_raw_code("prolog_list_events ",I),
        receive_list(EventList,I).

%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- regtype tclCommand(C) # "@var{C} is a @em{Tcl} command.".

tclCommand(_).

%%-------------------------------------------------------------
%%  TK
%%-------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tk_new(+Options,-TclInterpreter)

        :: list * tclInterpreter

        # "Performs basic Tcl and Tk initialization and creates the
          main window of a Tk application.@var{Options} is a list of
          optional elements according to:

@begin{description}

@item{@tt{name(+ApplicationName)}} Sets the Tk main window title to
@var{ApplicationName}. It is also used for communicating between
Tcl/Tk applications via the Tcl @em{send} command. Default name is an
empty string.

@item{@tt{display(+Display)}} Gives the name of the screen on which to
create the main window. Default is normally determined by the
@tt{DISPLAY} environment variable.

@item{@tt{file}} Opens the sript @tt{file}. Commands will not be read
from standard input and the execution returns back to Prolog only
after all windows (and the interpreter) have been deleted.

@end{description}

".
%%------------------------------------------------------------------------

tk_new(Options, Interp):-
        tk_options(Options,_,Appname,_,Display,_,File),
        !,
        tk_new(Interp,Appname,Display,File).


tk_options(Option,_,_,_,_,_,_):-
        var(Option),
        !,
        fail.
tk_options([],App,App,Disp,Disp,File,File).
tk_options([Option|Options],App0,App,Disp0,Disp,File0,File):-
        nonvar(Option),
        tk_option(Option,App0,App1,Disp0,Disp1,File0,File1),
        tk_options(Options,App1,App,Disp1,Disp,File1,File).

tk_option(file(File),App0,App,Disp0,Disp,_,Filename):-
        App=App0, 
        Disp=Disp0,
        Filename = File.
tk_option(name(Name),_,App,Disp0,Disp,File0,File):-
        App=Name,
        Disp=Disp0,
        File=File0.
tk_option(display(Display),App0,App,_,Disp,File0,File):-
        App=App0,
        Disp=Display,
        File=File0.

% Need to put in the second condition of the if case no display
tk_new(Interp,Appname,Display,File):-
        ( nonvar(Appname) ->
	     Opts = ['-name',Appname|Opts0]
	; Opts = Opts0
	),
        ( nonvar(Display) ->
	    Opts0 = ['-display',Display|Opts1]
	; Opts0 = Opts1
	),
        ( nonvar(File) ->
	    Opts1 = [File|Opts2]
	; Opts1 = Opts2
	),
	Opts2 = [],
        new_interp(Interp,Opts).

%%------------------------------------------------------------------------
:- pred tk_event_loop(+TclInterpreter) :: tclInterpreter 

# "Waits for an event and executes the goal associated to it. Events
   are stored from Tcl with the @em{prolog} command. The unified term
   is sent to the Tcl interpreter in order to obtain the value of the
   tcl array of @em{prolog_variables}.  If the term received does not
   have the form @tt{execute(Goal)}, the predicate silently exits.  If
   the execution of @var{Goal} raises a Prolog error, the interpreter
   is deleted and an error message is given.".

%%------------------------------------------------------------------------

:- meta_predicate tk_event_loop(addmodule).

tk_event_loop(X,FromModule):-
        receive_event(Event,X),
 ( 
        member(execute(Goal), Event) -> 
        (
            Goal = exit_tk_event_loop ->       % Leave event loop

            tcl_delete(X),
            true
        ;
            (
                Goal = exit_tk_event_loop(G1) ->
                send_term(Goal,X),
                tcl_delete(X),
%               catch(do_call(G1,FromModule),Error,tcl_loop_exit(X,Error)),
                catch(do_call(G1,_FromModule),Error,_), true
            ;
                catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error)),
                send_term(Goal,X) ,
                tk_event_loop(X,FromModule)
            )
        )
  %   (do_call(Goal,FromModule), tk_event_loop(X,FromModule)) 
 ;      %% Unknown command --- raise exception
        %     throw(unknown_command_in_tk_event_loop(X,FromModule))
        ( 
            Event = [end_of_file] ->
            true
        ;
            unknown_command_in_tk_event_loop(X)
        )
 ).

unknown_command_in_tk_event_loop(X) :-
        write('Prolog exception: '),
        write('The term must have the form execute(goal)'),
        tcl_delete(X),
        fail.

%predicate used to exit closing the main window
tcl_loop_exit(X,E):-
        write('Prolog exception: '),
        write(E),
        tcl_delete(X),
        fail.

do_call(Module:Goal,_FromModule) :-
        module_concat(Module,Goal,NewGoal), !,
        '$meta_call'(NewGoal).
do_call(Goal,FromModule) :-
        module_concat(FromModule,Goal,NewGoal),
        !,'$meta_call'(NewGoal).

%%------------------------------------------------------------------------
:- pred tk_next_event(+TclInterpreter,-Event) 
        
        :: tclInterpreter * string 

# "Processes events until there is at least one Prolog event
   associated with @var{TclInterpreter}. @var{Event} is the term
   correspondig to the head of a queue of events stored from Tcl with
   the @em{prolog_event} command.".
%%------------------------------------------------------------------------


tk_next_event(X,Event) :-
        tcl_event(X,[],[Event1|_]),
        Event1 == end_of_event_list,!,
        tk_next_event(X,Event).
tk_next_event(X,Event) :-
        tcl_event(X,[],[Event|_]),
        tcltk_raw_code("prolog_delete_event ",X),
        receive_confirm(_,X).

tk_next_event(_,_).

%%------------------------------------------------------------------------
:- pred tk_main_loop(+TclInterpreter) 
        
        :: tclInterpreter  
        
        # "Passes control to Tk until all windows are gone.".
%%------------------------------------------------------------------------

:- meta_predicate tk_main_loop(addmodule).


tk_main_loop(X,FromModule):-
        main_loop_tk_next_event(X,Event),
        tcltk_raw_code("prolog_delete_event ",X),
	nonvar(Event),
        ( member(end_of_event_list,[Event]) -> 
          ( tk_main_loop(X,FromModule) )
        ;
	    tcl_eval_if_needed(Event, X, FromModule),
            tk_main_loop(X,FromModule)
        ).
tk_main_loop(_,_).


main_loop_tcl_event(I,Command,EventList):-
        tcltk(Command,I),
        receive_list(EventList,I).

main_loop_tk_next_event(X,Event) :-
        main_loop_tcl_event(X,[],[Event1|_]),
        (Event1 == end_of_event_list ->
	 !, main_loop_tk_next_event(X,Event)
	;
	 Event = Event1).

tcl_eval_if_needed(Event, X, FromModule) :-
	Event = execute(_Goal), !,
	tcl_eval_result(X, Event, FromModule).
tcl_eval_if_needed(end_of_file,_,_).
tcl_eval_if_needed(Event, X, FromModule) :-
	catch(do_call(Event, FromModule), Error, tcl_loop_exit(X, Error)).
