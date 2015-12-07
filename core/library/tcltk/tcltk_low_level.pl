%%------------------------------------------------------------------------
%% TCL/TK low level library
%%------------------------------------------------------------------------

:- module(tcltk_low_level,
	[
	    new_interp/2,
%%	    new_interp_file/2,
	    tcltk/2,
	    tcltk_raw_code/2,
	    receive_result/2,
	    send_term/2,
	    receive_event/2,
	    receive_list/2,
	    receive_confirm/2,
	    delete/1
	],
	[assertions,isomodes,regtypes]).

:- use_module(library(terms)).

%%------------------------------------------------------------------------
:- doc(title,
        "Low level interface library to Tcl/Tk").

:- doc(author,"Montse Iglesias Urraca").
%%------------------------------------------------------------------------

:- use_module(library(sockets)).
:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(strings)).
:- use_module(library(format), [format/3]).

:- set_prolog_flag(multi_arity_warnings, off).

%%-----------------------------------------------------------------------

:- doc(module,"The @lib{tcltk_low_level} library defines the low
   level interface used by the @lib{tcltk} library. Essentially it
   includes all the code related directly to the handling of sockets
   and processes.

   This library should normally not be used directly by user programs,
   which must use @lib{tcltk} instead. On the other hand in some cases
   it may be useful to learn how this library works in order to
   understand possible problems in programs that use the @lib{tcltk}
   library.").

%%------------------------------------------------------------------------

:- regtype tclInterpreter(I) # "@var{I} is a reference to a @em{Tcl}
   interpreter.".

tclInterpreter(_).

:- regtype tclCommand(C) # "@var{C} is a @em{Tcl} command.".

tclCommand(_).

%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
%% CONSTRUCTOR / DESTRUCTOR
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------

:- pred new_interp(-TclInterpreter,+Options) :: tclInterpreter * list

        # "Creates two sockets, the term socket and the event socket,
           and opens a pipe to process @em{wish} in a new shell
           invoked with the specified @var{Options}.".

%% :- pred new_interp_file(+FileName,-TclInterpreter) :: string * tclInterpreter
%% 
%%         # "Creates two sockets, the term socket and the event socket,
%%            and opens a pipe to process @em{wish} in a new shell
%%            invoked with a @var{FileName}. @var{FileName} is treated as
%%            a name of a script file".

%%------------------------------------------------------------------------

new_interp(X, _) :-
        nonvar(X),
        !,
        fail.
new_interp('$wish$'(P,Strm,TermStream,EventStream), Options) :-
        current_host(Host),
        bind_socket(Port,1,Socket),
        number_codes(Port,PortCode),
        process_call(path('wish'), Options, [stdin(stream(Strm)), background(P)]),
	%
        write_string(Strm,"set prolog_host "),
        write(Strm,Host),nl(Strm),flush_output(Strm),
        write_string(Strm,"set prolog_port "),
        write_string(Strm,PortCode),nl(Strm),
        flush_output(Strm),
        bind_socket(EPort,1,ESocket),
        number_codes(EPort,EPortCode),
        write_string(Strm,"set event_port "),
        write_string(Strm,EPortCode),nl(Strm),
        flush_output(Strm),
        send_initial_code(Strm),
        socket_accept(Socket,TermStream),
        socket_accept(ESocket,EventStream),
        true.

%% (Outdated)
%%
%% new_interp_file(_,X) :-
%%         nonvar(X),
%%         !,
%%         fail.
%% 
%% new_interp_file(File,'$wish$'(Strm,TermStream,EventStream)) :-
%%         current_host(Host),
%%         bind_socket(Port,1,Socket),
%%         number_codes(Port,PortCode),
%%         atom_concat(['bltwish <',File,' &'],Command),
%%         popen(Command,write,Strm), % TODO: zombie process! (use process_call, close, and process_join)
%%         write_string(Strm,"set prolog_host "),
%%         write(Strm,Host),nl(Strm),flush_output(Strm),
%%         write_string(Strm,"set prolog_port "),
%%         write_string(Strm,PortCode),nl(Strm),
%%         flush_output(Strm),
%%         bind_socket(EPort,1,ESocket),
%%         number_codes(EPort,EPortCode),
%%         write_string(Strm,"set event_port "),
%%         write_string(Strm,EPortCode),nl(Strm),
%%         flush_output(Strm),
%%         send_initial_code(Strm),
%%         socket_accept(Socket,TermStream),
%%         socket_accept(ESocket,EventStream),
%%         true.

%%------------------------------------------------------------------------
:- pred delete(+TclInterpreter) :: tclInterpreter 

        # "Terminates the @em{wish} process and closes the pipe, term
          socket and event socket. Deletes the interpreter
          @var{TclInterpreter} from the system".
%%------------------------------------------------------------------------

delete('$wish$'(P,Strm,TermStrm,EventStrm)) :-
% was commented
        write_string(Strm,"uplevel 0 exit"),
        nl(Strm),
        flush_output(Strm),
% end comment
        close(TermStrm),
        close(Strm),
        close(EventStrm),
	%
	process_join(P).

%%------------------------------------------------------------------------
send_initial_code(Strm) :-
        core(String),
        write_string(Strm,String),
        nl(Strm),
        flush_output(Strm),
        fail.

send_initial_code(_).

%%------------------------------------------------------------------------
%% SEND BASIC TCLTK CODE ITEMS TO WISH
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tcltk_raw_code(+String,+TclInterpreter) :: string * tclInterpreter 

        # "Sends the tcltk code items of the @var{Stream} to the
          @var{TclInterpreter}".
%%------------------------------------------------------------------------

tcltk_raw_code(Str,'$wish$'(_,Strm,_,_)) :-
        string(Str,String,""),
        !,
        write_string(Strm,String),
        nl(Strm),
        flush_output(Strm).

/*
copy_stdin('$wish$'(_,Strm,_,_)) :-
        !,
        copy_stdin_aux(Strm).

copy_stdin_aux(Strm) :-
        get_code(Byte),
        Byte =\= -1,
        !,
        put_code(Strm,Byte),
        flush_output(Strm),
        copy_stdin_aux(Strm).

copy_stdin_aux(_).
*/

%%------------------------------------------------------------------------
%% MACRO IN ORDER TO SEND TCL/TK CODE TO WISH
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tcltk(+Code,+TclInterpreter) :: tclCommand * tclInterpreter 

        # "Sends the @var{Code} converted to string to the @var{TclInterpreter}".
%%------------------------------------------------------------------------

tcltk(Code,'$wish$'(_,Strm,_,_)) :-
        !,
        nl(Strm),
        send_code(Code,Strm).

%%------------------------------------------------------------------------

send_code([],Strm) :-
        !,
        nl(Strm),
        flush_output(Strm).

send_code([Number|Nc],Strm) :-
        number(Number),
        !,
        number_codes(Number,NumberAsCode),
        write_string(Strm,NumberAsCode),
        write(Strm,' '),
        send_code(Nc,Strm).
        
send_code([chars(String)|Nc],Strm) :-
        !,
        send_code([String|Nc],Strm).

send_code([dq(Code)|Nc],Strm) :-
        write(Strm,'\"'),
        send_code(Code,Strm),
        write(Strm,'\" '),
        !,
        send_code(Nc,Strm).

send_code([sqb(Code)|Nc],Strm) :-
        write(Strm,'['),
        send_code(Code,Strm),
        write(Strm,'] '),
        !,
        send_code(Nc,Strm).

send_code([br(Code)|Nc],Strm) :-
        write(Strm,'{'),
        send_code(Code,Strm),
        write(Strm,'} '),
        !,
        send_code(Nc,Strm).

send_code([min(Code)|Nc],Strm) :-
        atom(Code),
        !,
        write(Strm,'-'),
        write(Strm,Code),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([format(Fmt,Args)|Nc],Strm) :-
        format(Strm,Fmt,Args),
        !,
        send_code(Nc,Strm).

send_code([write(Term)|Nc],Strm) :-
        write(Strm,Term),
        !,
        send_code(Nc,Strm).

send_code([tcl(Var)|Nc],Strm) :-
        number_codes(Var,Str),
        atom_codes(Atom,Str),
        write(Strm,Atom),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([Atom|Nc],Strm) :-
        atom(Atom),
        !,
        write(Strm,Atom),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([Str|Nc],Strm) :-
        string(Str,String,""),
        !,
        write_string(Strm,String),
        write(Strm,' '),
        send_code(Nc,Strm).

send_code([_|Nc],Strm) :-
        !,
        send_code(Nc,Strm).

send_code(Not_a_list,Strm) :-
        !,
        send_code([Not_a_list],Strm).

%%------------------------------------------------------------------------
%% SEND A PROLOG TERM TO TCL/TK
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred send_term(+String,+TclInterpreter) :: string * tclInterpreter 

        # "Sends the goal executed to the
          @var{TclInterpreter}. @var{String} has the predicate with
          unified variables".
%%------------------------------------------------------------------------

send_term(Term,'$wish$'(_,_,Stream,_)) :-
        write_term(Stream,Term,[]),
        nl(Stream),
	flush_output(Stream).
        

%%------------------------------------------------------------------------
%% READ A PROLOG TERM FROM TCL/TK
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
:- pred receive_result(-Result,+TclInterpreter) :: string * tclInterpreter 

        # "Receives the @var{Result} of the last @em{TclCommand} into
           the @var{TclInterpreter}. If the @em{TclCommand} is not
           correct the @em{wish} process is terminated and a message
           appears showing the error".
%%------------------------------------------------------------------------
receive_result(Result,I) :-
        receive_term(Term,I),
        get_result(Term,Result,I).

get_result(:(tcltk_low_level,tcl_result(X)),Result,_) :- 
        Result = X.
get_result(:(tcltk_low_level,tcl_error(_)),_,I) :-
        delete(I),
        fail.

receive_term(Term,'$wish$'(_,_,Stream,_)) :-
	catch(read_term(Stream,Term,[]), Error, tcl_read_error(Error)),
        exceptions(Term).

% receive_term(Term,VarNames,'$wish$'(_,_,Stream,_)) :-
%         catch(read_term(Stream,Term,[variable_names(VarNames)]), Error, tcl_read_error(Error)).

%%------------------------------------------------------------------------
%% EXCEPTIONS FROM TCL/TK
%%------------------------------------------------------------------------

exceptions(Term) :-
        catch(Term, E, tcl_error(E)).

tcl_error(_).

tcl_result(_).
%	display(tcl_result(A)), nl.

%%________________________________________________________________________
%% READ A PROLOG LIST OF TERMS FROM TCLTK
%%________________________________________________________________________

%%------------------------------------------------------------------------
:- pred receive_event(-Event,+TclInterpreter) :: list * tclInterpreter 

        # "Receives the @var{Event} from the event socket of the
          @var{TclInterpreter}.".
%%------------------------------------------------------------------------

receive_event([Term],'$wish$'(_,_,_,Stream)) :-
        read_term(Stream,Term,[]).

%%------------------------------------------------------------------------
:- pred receive_list(-List,+TclInterpreter) :: list * tclInterpreter 

        # "Receives the @var{List} from the event socket of the
          @var{TclInterpreter}.The @var{List} has all the predicates
          that have been inserted from Tcl/Tk with the command
          prolog_event.  It is a list of terms.".
%%------------------------------------------------------------------------

receive_list([Term|Nt],'$wish$'(_,_,_,Stream)) :-
        catch(read_term(Stream,Term,[]), Error, tcl_read_error(Error)), 
        Term \== end_of_event_list,!,
	receive_list(Nt,'$wish$'(_,_,_,Stream)).
receive_list([end_of_event_list],_).

tcl_read_error(_) :- 
	fail.


%%------------------------------------------------------------------------
:- pred receive_confirm(-String,+TclInterpreter) :: string * tclInterpreter 

        # "Receives the @var{String} from the event socket of the
          @var{TclInterpreter} when a term inserted into the event
          queue is managed.".
%%------------------------------------------------------------------------
receive_confirm(Term,'$wish$'(_,_,_,Stream)) :-
        catch(read_term(Stream,Term,[]), Error, tcl_read_error(Error)).

%%------------------------------------------------------------------------
%% INITIAL CODE
%%------------------------------------------------------------------------

:- doc(doinclude, core/1).

:- pred core(-String) :: string  

        # "@pred{core/1} is a set of facts which contain @var{String}s
           to be sent to the Tcl/Tk interpreter on startup.  They
           implement miscelaneous Tcl/Tk procedures which are used by
           the Tcl/Tk interface.".

%%------------------------------------------------------------------------

core("set event_socket [socket $prolog_host $event_port]").
%core("puts $event_port").
%core("puts $event_socket").

core("set term_socket [socket $prolog_host $prolog_port]").
%core("puts $prolog_port").
%core("puts $term_socket").

%core("[fconfigure $event_socket -blocking false]").


core("set tcl_eval_mode 0").
core("set prolog_variables(X) 1").
core("set terms [list] ").

core("proc prolog_unset_tcl_eval_mode {} {").
core("  global tcl_eval_mode").
core("  set tcl_eval_mode 0").
core("} ").

core("proc prolog_set_tcl_eval_mode {} {").
core("  global tcl_eval_mode").
core("  set tcl_eval_mode 1").
core("} ").

core("proc prolog_term {term} {").
core("global event_socket").
core(" puts  $event_socket $term. ").
core(" flush $event_socket ").
core("} ").

core("proc prolog {agoal} {").
core("  global term_socket").
%core("  global event_socket").
core("  global tcl_eval_mode").
core("  if {$tcl_eval_mode} {").
core("      puts  $term_socket tcltk_low_level:tcl_result(execute($agoal)).").
core("      flush $term_socket").
core("  } else {").
core("      prolog_event execute($agoal)"). 
core("      prolog_list_events "). 
core("      prolog_delete_event ").
core("  }").
core("  gets  $term_socket term ").
core("  set ret [unify_term execute($agoal)) $term] ").
core("  if {$term == \"fail\" || $ret == 0} {").
core("	   return 0").
core("  } else { return 1}").
core("} ").

core("proc prolog1 {agoal} {").
core(" global event_socket").
core("  puts $event_socket $agoal.").
core("  flush $event_socket").
core("  return execute($agoal) ").
core("} ").

core("proc prolog_event {term} {").
core("global terms").
core(" set terms [concat $terms $term] ").
core(" return 0 ").
core("} ").

core("proc prolog_delete_event { } {").
core("global terms").
core("global event_socket").
core(" set terms [lreplace $terms 0 0 ] ").
core(" puts  $event_socket \"end_of_event_list.\" ").
core(" flush $event_socket ").
%core(" puts  finprologdelete ").
core(" return 0 ").
core("} ").

core("proc prolog_list_events { } {").
core("global event_socket").
core("global terms").
core(" set nterms [llength $terms] ").
core(" set x 0 ").
core(" while {$x<$nterms} { ").
core("    puts $event_socket [lindex $terms $x]. ").
%core("    set terms [list]").
core("    flush $event_socket ").
core("    incr x ").
core(" } ").
core(" puts  $event_socket \"end_of_event_list.\" ").
core(" flush $event_socket ").
core(" return 0 ").
core("} ").

% Execute command and sendresults back to prolog
% This is internally used by tcl_eval/3.
% return 0 when the scripts runs without errors, and 1 if there is an error

core("proc prolog_cmd {command} {").
core(" global term_socket").
core("   set error [catch {set result [uplevel $command]} var]").
core("   if {$error} {").
core("       set var [convert_variable $var ]").
core("       puts  $term_socket tcltk_low_level:tcl_error(\\""$var\\"").").
core("       flush $term_socket").
core("       return $error").
core("   } else { ").
core("       set result_aux [string compare [string range $result 0 6 ] execute ]").
core("       if { $result_aux } {").
core("          puts  $term_socket tcltk_low_level:tcl_result(\\""$result\\"").").
core("          flush $term_socket").
core("       } else { ").
core("          gets  $term_socket term ").
core("          set ret [unify_term $result $term]").
core("          set error $ret").
core("          } ").
core("       puts  $term_socket tcltk_low_level:tcl_result(tcl_eval_finished).").
core("       flush $term_socket").
core("       return $error").
core("   } ").
core("} ").


% Execute command and send results back to prolog

core("proc prolog_one_event {a_term} {").
core(" global event_socket").
core(" global term_socket").
%% new global variable prolog_variable, wich contain the result of the unification
core(" global prolog_variables").
core("   set result 0").
core("   set result_var 0 ").
%core("   puts $a_term ").
%core("   puts $result ").
core("   puts  $event_socket $a_term.").
core("   flush $event_socket").
%core(" puts antesdegets ").
core("   gets  $term_socket result").
%core(" puts despuesdegets ").
core("   set ret [unify_term $a_term $result]").
%core(" puts despuesdeunify ").
%core("   gets  $event_socket $result_var").
%core("   puts $result_var").
core("} ").

% Execute command and send results back to prolog

core("proc prolog_thread_event {a_term} {").
core(" global event_socket").
core(" global term_socket").

%% new global variable prolog_variable, wich contain the result of the unification
core(" global prolog_variables").
core("   fconfigure $term_socket -blocking false").
core("   set result 0").
core("   set result_var 0 ").
core("   puts  $event_socket $a_term.").
core("   flush $event_socket").
core("   gets  $term_socket result").
core("   set ret [unify_term $a_term $result]").
core("} ").

core("proc convert_variable {var} {").
core("   set result \" \" ").
core("   while (1) { ").
core("      set long [string length $var] ").
core("      set pos [string first \"\\""\" $var] ").
core("      if { $pos == -1 } { ").
core("           set result ${result}${var} ").
core("           return $result } ").
core("      incr pos -1 ").
core("      set new [string range $var 0 $pos] ").
core("      incr pos ").
core("      incr pos ").
core("      set var [string range $var $pos $long] ").
core("      set result ${result}${new}'' ").
core("  } ").
core("} ").

% the value of the unification have to be introduced in the global array 
% prolog_variables with the variable name as index
core("proc unify_term {term result} {").
core(" global prolog_variables").
% core("             puts $term ").
% core("             puts $result ").
core("   set long [string length $term] ").
core("  incr long -3 ").
core("   set term [string range $term 8 $long] ").
% the term without execute
core("   set ret [unify_term_aux $term $result ]").
%core("parray prolog_variables").
core("   if { $ret == 0 } { ").
core("        return 0 } ").
%core("   else {          ").
core("        return 1 ").
%core("     } ").
core("} ").

core("proc unify_term_aux {term result} {").
core(" global prolog_variables").

core("   set n_c_t -1").
core("   set n_c_r -1").

core("   set pos_t [string first \"(\" $term] ").
core("   set pos_r [string first \"(\" $result] ").
% the name of the predicate has to be the same
% first returns -1 if dosent found the braquet
core("   if { $pos_t == -1 } { ").
core("      if { $pos_r == -1 } { ").
core("         set comp [string compare $term $result] ").
% compare returns 0 if the strings are equal
core("         if { $comp == 0 } { ").
core("            return 1 }").
%core("         else { ").
%core("            return 0 }").
core("            return 0 ").
core("         }").
core("      }").
core("   if { $pos_t == $pos_r } { ").
core("        incr pos_t 1  ").
core("        incr pos_r 1  ").
core("        set long_t [string length $term] ").
core("        set long_r [string length $result] ").
core("        set term_aux [string range $term $pos_t $long_t] ").
core("        set result_aux [string range $result $pos_r $long_r] ").
core("        set long_t [string length $term_aux] ").
core("        set long_r [string length $result_aux] ").
core("        while {$long_t !=  0} { ").
core("             set long_t_1 [string first \",\" $term_aux] ").
core("             set long_r_1 [string first \",\" $result_aux] ").
core("             if { $long_t_1 == -1 || $long_r_1 == -1} { ").
core("                  set long_t_1 [string first \")\" $term_aux] ").
core("                  set long_r_1 [string first \")\" $result_aux] ").
% Modified code --------
core("                  set n_t [expr $long_t_1+1] ").
core("                  set n_r [expr $long_r_1+1] ").
core("                  set n_c_t [string first \")\" $term_aux $n_t] ").
core("                  set n_c_r [string first \")\" $result_aux $n_r]").

core("             } ").

core("             if {$n_c_t == -1} { ").
core("                  incr long_t_1 -1} else {set n_c_t -1}").
core("             if {$n_c_r == -1} { ").
core("                  incr long_r_1 -1} else {set n_c_r -1} ").
% ----------------------
%core("             puts original_variable($term_aux) ").
core("             set term_aux_1 [string range $term_aux 0 $long_t_1] ").
core("             set result_aux_1 [string range $result_aux 0 $long_r_1] ").
core("             set prolog_variables($term_aux_1) $result_aux_1 ").
%core("             puts setting_variable($term_aux_1) ").
core("             incr long_t_1 2  ").
core("             incr long_r_1 2  ").
core("             if {$long_t <= $long_t_1 || $long_r <= $long_r_1 } { ").
core("                  return 0 }").
core("             set term_aux [string range $term_aux $long_t_1 $long_t] ").
core("             set result_aux [string range $result_aux $long_r_1 $long_r] ").
core("             set long_t [string length $term_aux] ").
core("             set long_r [string length $result_aux] ").
%core("             puts worked").
core("        } ").
core("   } ").
core("} ").
