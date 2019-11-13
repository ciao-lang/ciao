:- module(regp_webbased, [], [actmod, assertions]).

:- doc(title, "The ``webbased'' registry protocol").

:- doc(module, "The @tt{webbased} registry is also based on another
   active module working as a @concept{name server}. The address of
   the registry must be specified in an application file named
   @tt{webbased_common.pl} (see section below).

   An application using a name server for active modules must have a
   file named @tt{webbased_common.pl} that specifies where the name
   server resides. It must have the @tt{URL} and the path which
   corresponds to that @tt{URL} in the file system of the server
   machine (the one that hosts the @tt{URL}) of the file that will
   hold the name server address.

   The current distribution provides a file @tt{webbased_common.pl}
   that can be used (after proper setting of its contents) for a
   server of active modules for a whole installation. Alternatively,
   particular servers for each application can be set up (see below).

   The example implementation @file{examples/nameserver/nameserver.pl}
   can also be used for this registry protocol.

   To set up a name server edit @tt{webbased_common.pl} to change its
   contents appropriately as described above (@tt{URL} and
   corresponding complete file path). Then recompile this library
   module:

@begin{verbatim}
ciaoc -c webbased_common
@end{verbatim}
").

% Note: it uses the 'webserver_mod' active module, located using the
% special 'webserver' registry protocol.

:- use_module(library(actmod/regp_webserver)).
:- use_module(webserver_mod, [tell_address/4, ask_address/3],
     [active, reg_protocol(webserver)]).

% ---------------------------------------------------------------------------

:- impl(actmod_publish, webbased).

(webbased as actmod_publish).save_addr(ActRef, DMod, Address, Pid, _Opts) :-
    ( tell_address(ActRef, DMod, Address, Pid) ->
        true
    ; throw(unable_to_register_at_platform(ActRef, DMod, Address, Pid))
    ).

% ---------------------------------------------------------------------------

:- impl(actmod_locate, webbased).

(webbased as actmod_locate).remote_address(ActRef, DMod, Address) :-
    ( ask_address(ActRef, DMod, Address0) -> true
    ; throw(no_address_for(ActRef))
    ),
    Address = Address0.

(webbased as actmod_locate).cleanup_actI(_ActRef).
