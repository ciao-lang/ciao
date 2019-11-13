:- module(regp_filebased, [], [assertions]).

:- doc(title, "The ``filebased'' registry protocol").

:- doc(module, "The @tt{filebased} saves the IP address and socket
   number of the server in a @em{<module_name>}@tt{.addr} file at the
   address file directory or in the directory that a @tt{.addr} file,
   if it exists, specifies.

   The address file directory can be selected with the
   @pred{set_reg_dir/1} predicate at @lib{filebased_common} or the
   @tt{--reg-dir} option for server startup.

   By default, this directory is set to the temporary directory (see
   @pred{get_tmp_dir/1} at @lib{system}).

   Note that servers and clients can be started in different machines,
   provided this directory is shared (e.g., by NFS or Samba), or the
   file can be moved to an appropriate directory on a different
   machine --provided the full path is the same.").

:- use_module(library(system), [pause/1, file_exists/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).

:- include(library(actmod/actmod_hooks)).
:- use_module(library(actmod/filebased_common)).

% ---------------------------------------------------------------------------

:- impl(actmod_publish, filebased).

(filebased as actmod_publish).save_addr(ActRef, DMod, Address, Pid, Opts) :-
    % Set reg_dir if needed
    ( member(reg_dir(Path), Opts) ->
        set_reg_dir(Path)
    ; true
    ),
    get_reg_dir(BaseDir),
    actI_to_addrpath(BaseDir, ActRef, AddrPath),
    file_save_addr(0o022, AddrPath, DMod, Address, Pid).

% ---------------------------------------------------------------------------

:- impl(actmod_locate, filebased).

(filebased as actmod_locate).remote_address(ActRef, DMod, Address) :-
    get_reg_dir(BaseDir),
    actI_to_addrpath(BaseDir, ActRef, AddrPath),
    % TODO: move to actI_get_addr/2?
    ( '$dist_addr_retry'(Retry),
      wait_for_file(AddrPath, Retry) ->
        true
    ; true % file_load_addr/3 will throw exception % throw(error(not_ready(ActRef), my_actmod_ready/1))
    ),
    file_load_addr(AddrPath, DMod, Address).

% TODO: spin-lock! use a registry actmod (or inotify, or ciao-serve, etc.)
wait_for_file(_F, Timeout) :-
    Timeout =< 0, !, fail.
wait_for_file(F, Timeout) :-
    ( file_exists(F) ->
        true
    ; pause(1), % TODO: too much! also see '$dist_addr_retry'/1
      Timeout1 is Timeout - 1,
      wait_for_file(F, Timeout1)
    ).

% TODO: used now to cleanup before spawn; use to obtain a free Id?
%   prealloc a bunch of them?
(filebased as actmod_locate).cleanup_actI(ActRef) :-
    get_reg_dir(BaseDir),
    actI_to_addrpath(BaseDir, ActRef, AddrPath),
    del_file_nofail(AddrPath).

