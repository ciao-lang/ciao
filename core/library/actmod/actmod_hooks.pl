% (included file)

:- use_package(traits).

% ---------------------------------------------------------------------------
% Registry protocols

:- trait(actmod_publish, [
    % save_addr(+ActRef, +DMod, +Address, +Pid, +Opts) # "Publish the address @var{Address} of active module @var{DMod} instance @var{ActRef}"
    save_addr/5
]).

:- trait(actmod_locate, [
    % remote_address(+ActRef, -DMod, -Address) # "obtain @var{Address} and @var{DMod} module of active module instance @var{ActRef}.".
    remote_address/3,
    cleanup_actI/1 % TODO: temporary?
]).

% ---------------------------------------------------------------------------
% Active module flags, props, etc.

% '$dist_addr_retry'(Count): number of retries for loading .addr files % TODO: make it an option of registry protocols (actmod based registry does not need it)
:- multifile '$dist_addr_retry'/1.

% '$dmod_reg_protocol'(DMod, RegProtocol): Registry protocol used by DMod
:- multifile '$dmod_reg_protocol'/2.
:- discontiguous('$dmod_reg_protocol'/2).

% '$dmod_src'(DMod, ModSpec): Source (ModSpec) for DMod
:- multifile '$dmod_src'/2.
:- discontiguous('$dmod_src'/2).

% '$dmod_prop'(DMod, Prop): Other properties for DMod
:- multifile '$dmod_prop'/2.
:- discontiguous('$dmod_prop'/2).

% '$local_actmod'(DMod): DMod is a (local) actmod
:- multifile '$local_actmod'/1.
:- discontiguous('$local_actmod'/1).

% '$static_named_actRef'(ActRef, DMod): ActRef is a static named
% active module instance of DMod
:- multifile '$static_named_actRef'/2.
:- discontiguous('$static_named_actRef'/2).

% TODO: document
:- multifile '$dmod_proxy'/2.
:- discontiguous('$dmod_proxy'/2).

% ---------------------------------------------------------------------------

% (expand actmod serve goals)
% '$actmod_exe'(Head, M, MHead)
:- multifile '$actmod_exe'/3.
:- meta_predicate('$actmod_exe'(?, ?, fact)).

% ---------------------------------------------------------------------------
% Query protocols

:- trait(qprot, [
    collect/2,
    dec/3,
    enc/2,
    % prepares a remote query (request and binder) (on the local side)
    prepare_query/3,
    % connects answer to current execution (on the local side)
    join_answers/6
]).


