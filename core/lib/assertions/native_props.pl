:- module(native_props, [], [noprelude, assertions, regtypes]).

% TODO: Split this module:
% 1. Separate into include files.
% 2. Work separately on documentation.
% 3. Work in parallel on splitting into modules.
% 4. Now, nativeprops is just a bunch of use_modules. Move nativeprops.pl to lib.
% 5. Put everything together.

:- doc(title, "Properties which are native to analyzers").
:- doc(author, "Francisco Bueno").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Edison Mera").
:- doc(author, "Amadeo Casas").
:- doc(author, "Jose F. Morales").

:- doc(module, "@cindex{properties, native} This library contains
   a set of properties which are natively understood by the different
   program analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp}
   on output and they can also be used as properties in assertions.

   Some of the properties can also be used as runtime-checks. See
   @lib{native_props_rtc} for the runtime-check implementation of such
   properties.").

:- doc(usage, "@tt{:- use_module(library(assertions/native_props))}

   or also as a package @tt{:- use_package(nativeprops)}.

   Note the slightly different names of the library and the package.").

:- doc(bug, "EMM: specify if run-time checks of some properties are
   complete (exhaustive), incomplete, not possible, or unimplemented").

:- doc(bug, "MH: Some of these properties should be moved to rtchecks
   or testing libs.").

:- doc(bug, "MH: Missing test cases and examples.").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_basic)).
:- use_module(engine(arithmetic)).

:- if(defined(optim_comp)).
:- else.
:- use_module(engine(hiord_rt)). % call/?
:- endif.

% --------------------------------------------------------------------------
:- include(library(assertions/native_props_shfrg)).
:- include(library(assertions/native_props_polyhedral)).
:- include(library(assertions/native_props_nfdet)).
:- include(library(assertions/native_props_exceptions)).
:- include(library(assertions/native_props_sideff)).
:- include(library(assertions/native_props_cardinality)).
:- include(library(assertions/native_props_cost)).

% ===========================================================================
:- doc(section, "Meta-properties: instance and compat").
% TODO: should be at the beginning? in assertions?

:- doc(bug, "MH: Also defined (much better) in basic_props!!").

:- if(defined(optim_comp)).
:- else.
:- export(compat/1).
:- meta_predicate compat(goal).
:- prop compat(Prop) + no_rtcheck
   # "Use @var{Prop} as a compatibility property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

compat(_). % processed in rtchecks_basic
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "MH: The idea was to call it inst/2 (to avoid confusion
   with instance/2). Note that it is also defined --this way-- in  
   basic_props!!"). 
:- doc(bug, "MH: inst/1 not really needed since it is the default? ").

:- if(defined(optim_comp)).
:- else.
:- export(instance/1).
:- meta_predicate instance(goal).
:- prop instance(Prop) + no_rtcheck
# "Use Prop as an instantiation property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

instance(_). % processed in rtchecks_basic
:- endif.

% --------------------------------------------------------------------------

:- doc(section, "Other properties").

:- doc(bug, "JF: move somewhere else, only for oo_types and java_cha").

:- if(defined(optim_comp)).
:- else.
:- doc(tau(Types), "@var{Types} contains a list with the type associations
   for each variable, in the form @tt{V/[T1,..,TN]}.").

:- export(tau/1).
:- prop tau(TypeInfo) + native
   # "@var{Types} is a list of associations between variables and list of types".

tau([]).
tau([Var/Type|R]) :-
    var(Var),
    list(Type),
    valid_type(Type),
    tau(R).

valid_type([Type]) :-
    atom(Type).
valid_type([Type|Rest]) :-
    atom(Type),
    valid_type(Rest).
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "Should be in unittest_props library?").

:- if(defined(optim_comp)).
:- else.
:- export(user_output/2).
:- meta_predicate user_output(goal, ?).
:- prop user_output(Goal, S)
   # "Calls of the form @var{Goal} write @var{S} to standard output.".
:- impl_defined(user_output/2).
:- endif.

%% :- export(user_error/2).
%% :- prop user_error(Goal, S) #
%%      "Calls of the form @var{Goal} write @var{S} to standard error.".
%% 
%% :- meta_predicate user_error(goal, ?).
%% :- impl_defined(user_error/2).

% --------------------------------------------------------------------------

:- doc(bug, "MH: not really clear why this should be here.").

% Built-in in CiaoPP
% if you change this declaration, you have to change ciaoPP:
:- export(entry_point_name/2).
:- doc(hide, entry_point_name/2).
:- meta_predicate entry_point_name(goal, ?).
:- prop entry_point_name/2 + no_rtcheck.
:- if(defined(optim_comp)).
:- '$props'(entry_point_name/2, [impnat=indefinable]).
:- else.
:- impl_defined(entry_point_name/2).
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "succeeds/1 is only used in 2 modules, its name conflicts
   with other notions in assertions, it should be renamed (e.g. to
   test_succeeds/1 or something along that line)").

:- doc(bug, "We probably need a succeeds/1 comp property. It actually
   appears in the CiaoPP tutorial at ciaopp/doc/tutorial.tex").

:- if(defined(optim_comp)).
:- else.
:- export(succeeds/1). % TODO: very crazy. % TODO: rename!
:- meta_predicate succeeds(goal).
:- prop succeeds(Goal) + no_rtcheck # "A call to @var{Goal} succeeds.".

:- impl_defined(succeeds/1).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(test_type/2).
:- meta_predicate test_type(goal, ?).
:- prop test_type(X, T) # "Indicates the type of test that a predicate
   performs.  Required by the nonfailure analyisis.".

test_type(Goal, _) :- call(Goal).
:- endif.
