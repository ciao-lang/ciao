% (included file)

:- use_package(traits).

% ---------------------------------------------------------------------------

% '$curr_mod'(Mod): Mod is local (the code, not only the stub, is in
% this process)
% TODO: ask module system instead?
:- multifile '$curr_mod'/1.

% ---------------------------------------------------------------------------
% TODO: move to fnct_hooks.pl?
% TODO: runtime assertions (rtasr) trait?

% Trait for functor symbols
:- trait(fnct, [
    decl_at_mod/1, % (declared at module)
    prop/1 % (properties) (ala '$pred_property'/2)
]).

:- multifile '$fnct_stub_rename'/2.
:- discontiguous('$fnct_stub_rename'/2).

% ---------------------------------------------------------------------------
% Hooks for the fiber scheduler

:- discontiguous('$handle_stream'/2).
:- multifile '$handle_stream'/2.

% '$current_msg'(-,-,-)
:- discontiguous('$current_msg'/3).
:- multifile '$current_msg'/3.

% TODO: merge 'gsusp' and 'async' traits
:- trait(gsusp, [
    % guard(+TopSched, +Msg, -Mode)
    %   Guard for a fiber to wake. TopSched is `yes` iff the top
    %   io_sched_nested/2 was started for this fiber.
    %   `Msg` is available message.
    %   `Mode` is `u` iff the fiber wake code is on the success
    %   continuation to io_sched_nested/2
    guard/3,
    % run(+Msg)
    run/1
]).

% ---------------------------------------------------------------------------
% TODO: runtime assertions (rtasr) trait?

% Trait for suspendable predicates
:- trait(async, [
    ftypes/1, % (mapping between arguments and context data)
    % --
    % NOTE: arguments include context data passed as arguments so arity
    %   may differ (like addmodule in meta_predicates)
    run/1 % (run and obtain a continuation)
]).

:- multifile '$fiber_susp_hook'/2.
:- discontiguous('$fiber_susp_hook'/2).

% Trait for "transient" data (saved in actI suspensions)
:- trait(transient, [
    decl/0 % (declaration)
]).

