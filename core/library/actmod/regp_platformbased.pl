:- module(regp_platformbased, [], [actmod, assertions]).

:- doc(title, "The ``platformbased'' registry protocol").

:- doc(module, "The @tt{platformbased} registry is based on another
   active module working as a @concept{name server}. It accepts
   queries for publishing and locating other active module
   instances. The address of this registry active moduel is given by
   the @tt{--reg-addr} option.

   @subsection{Example name server}

   See @file{examples/nameserver/nameserver.pl} for an example
   implementation of a name server. In this example the addresses of
   active modules are saved by the name server in a persistent
   database (subdirectory @tt{actmod_db} of the directory where you
   start it). This allows to restart the server right away if it dies
   (since it saves its state).  This directory should be cleaned up
   regularly of addresses of active modules which are no more
   active. To do this, stop the server ---by killing it (its pid is in
   @tt{PATH/FILE}), and restart it after cleaning up the files in the
   above mentioned directory.

   The name server has to be compiled as an active module itself:

@begin{verbatim}
ciaoc nameserver
@end{verbatim}

   It has to be started in the server machine before the application and
   its active modules are compiled.").

% Note: it uses the 'platformserver_mod' active module, located using
% the special 'platformserver' registry protocol.

:- use_module(library(actmod/regp_platformserver)).
:- use_module(platformserver_mod, [tell_address/4, ask_address/3],
     [active, reg_protocol(platformserver)]).

% ---------------------------------------------------------------------------

:- impl(actmod_publish, platformbased).

(platformbased as actmod_publish).save_addr(ActRef, DMod, Address, Pid, Opts) :-
	% Save platform address and current active module name
	( member(reg_addr(RegAddr), Opts) ->
	    set_platform_addr(RegAddr)
	; throw(error(unknown_platform_addr, save_addr/5))
	),
	% Publish in the registry
        ( tell_address(ActRef, DMod, Address, Pid) ->
	    true
	; throw(unable_to_register_at_platform(ActRef, DMod, Address, Pid))
	).

% ---------------------------------------------------------------------------

:- impl(actmod_locate, platformbased).

(platformbased as actmod_locate).remote_address(ActRef, DMod, Address) :-
        ( ask_address(ActRef, DMod, Address0) -> true
	; throw(no_address_for(ActRef))
	),
	Address = Address0.

(platformbased as actmod_locate).cleanup_actI(_ActRef).
