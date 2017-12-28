% Declaration of async commands

% :- trait async { ... }.

% (declaration)
:- multifile 'async.decl'/1.
:- discontiguous 'async.decl'/1.
% (mapping between arguments and context data)
:- multifile 'async.ftypes'/2.
:- discontiguous 'async.ftypes'/2.
% (comp properties of cmd)
:- multifile 'async.prop'/2.
:- discontiguous 'async.prop'/2.
% (code, returning a continuation)
% NOTE: arguments include context data passed as arguments so arity
%   may differ (like addmodule in meta_predicates)
:- multifile 'async.run'/2.
:- discontiguous 'async.run'/2.

% TODO: use async.decl, etc? (service_name -- or top module of actmod
% -- that exports this action)
:- discontiguous 'async.exported'/2.
:- multifile 'async.exported'/2.
% (comp properties of cmd -- stub version)
:- multifile 'async.stub_prop'/2.
:- discontiguous 'async.stub_prop'/2.

% :- trait actmod { ... }.

% (detect that the active module runs in the current process)
% TODO: move to active modules
:- multifile 'actmod.in_process'/1.

% (call in_process active modules)
% TODO: move to active modules
:- multifile 'actmod.in_process_call'/2.

% :- trait transient { decl. }.

% 'transient.decl'(Fact): Data fact that is passed around across async calls
:- discontiguous 'transient.decl'/1.
:- multifile 'transient.decl'/1.
