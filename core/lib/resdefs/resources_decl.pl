:- package(resources_decl).

:- use_package(assertions).
:- use_package(nativeprops).
:- use_package(basicmodes).
:- use_package(argnames).

:- multifile '$def$ru'/1.
:- multifile '$def$gru'/1.

:- pred resource_usage(+Res,-Value) => num(Value) + no_rtcheck
 # "Unifies @var{Value} with the current usage for the resource @var{Res}.".

:- multifile resource_usage/2.

:- pred global_resource_usage(+Res, -Value) => num(Value) + no_rtcheck
 # "Unifies @var{Value} with the current global usage for the resource
   @var{Res}.".

:- multifile global_resource_usage/2.

%  Directive to define resources
:- new_declaration(resource/1).
:- op(1170, fx, (resource)).
%  Directive to define the cost function associated with heads
:- new_declaration(head_cost/3).
:- op(1170, fx, (head_cost)).
%  Directive to define the cost function associated with literals
:- new_declaration(literal_cost/3).
:- op(1170, fx, (literal_cost)).
%  Directive to configure default costs for builtins or library predicates
%  in the analyzer
:- new_declaration(trust_default/1).
:- op(1170, fx, (trust_default)).

%  Directive to define the set of resources on which the granularity
%  analysis is applied
:- new_declaration(granularity_resources/1).
:- op(1170, fx, (granularity_resources)).

%  Java directives
:- new_declaration(java_mode/2).
:- op(1170, fx, (java_mode)).
:- new_declaration(java_measure/2).
:- op(1170, fx, (java_measure)).

:- argnames litinfo(literal, approx, extra).

%  Directive to load cost predicates
:- new_declaration(load_resource_module/1).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: This add_mod_to_directives/3 is a ugly kludge
:- load_compilation_module(library(resdefs/resources_tr)).
:- add_sentence_trans(resources_tr:add_mod_to_directives/3, 820).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- new_declaration(platform/1).
:- op(1170, fx, (platform)).
:- new_declaration(compound_resource/2).
:- new_declaration(platform_constants/3).
:- new_declaration(platform_constants/4).
