% (included file)

:- doc(section, "Timingmodel bundle").
% Timingmodel (a simple WAM for time analysis)

% TODO: split in two bundles: miniprolog and timingmodel

%% % TODO: Add as help for custom_run on this bundle
%%
%% 	showcmd(gen_timingmodel, "contrib", [
%%           %1_______________________________________________
%%           "Benchmark and estimate timing model for",
%% 	  "'miniprolog'"
%%         ]),
%% 	showcmd(clean_timingmodel, "contrib", [
%%           %1_______________________________________________
%%           "Clean results of gen_timingmodel"
%%         ]),
%% 	showcmdsep.

:- use_module(library(system), [copy_file/2, copy_file/3, file_exists/1]).

'$builder_hook'(timingmodel:prebuild_nodocs) :-
	% do_timingmodel % TODO: must be called explicitly using custom_run
	% (needed even if miniprolog is not compiled)
	copy_mp_auto.

% (called from 'ciao_builder', called from 'ciaobot' as a test)
% TODO: rename by timingmodel:custom_run(gen, []) and call with 'ciao custom_run contrib/timingmodel gen'
'$builder_hook'(custom_run(gen_timingmodel, [])) :- !,
	do_timingmodel,
	copy_mp_auto.

timingmodel_cmd := bench|estimate.

do_timingmodel :-
	normal_message("Compiling mini prolog engine", []),
	invoke_gmake_miniprolog(all),
	copy_file(~bundle_path(contrib, 'library/timingmodel/miniprolog/bin/timingmodel_auto.pl'),
	          ~bundle_path(contrib, 'library/timingmodel/timingmodel_pre.pl'),
		  [overwrite]),
	%
	normal_message("Generating timing model for mini prolog", []),
	( % (failure-driven loop)
	  timingmodel_cmd(Cmd),
	    invoke_gmake_timingmodel(Cmd),
	    fail
	; true
	).

% This extra step is to ensure the generation of timingmodel_auto.pl
% even if miniprolog has not been configured
copy_mp_auto :-
	Orig = ~bundle_path(contrib, 'library/timingmodel/timingmodel_pre.pl'),
	( file_exists(Orig) ->
	    copy_file(Orig,
	              ~bundle_path(contrib, 'library/timingmodel/timingmodel_auto.pl'),
		      [overwrite])
	; true
	).

benchmp :-
	normal_message("running timing model benchmarks", []),
	invoke_gmake_timingmodel('bench').

estimatemp :-
	normal_message("running timing model estimate", []),
	invoke_gmake_timingmodel('estimate').

'$builder_hook'(custom_run(clean_timingmodel, [])) :- !,
	normal_message("cleaning timingmodel", []),
	invoke_gmake_miniprolog(clean),
	invoke_gmake_timingmodel(clean).

:- use_module(ciaobld(builder_aux), [invoke_gmake/2]).

invoke_gmake_miniprolog(Cmd) :-
	invoke_gmake(~bundle_path(contrib, 'library/timingmodel/miniprolog'),
	             ['-s', '-j1',
		      ~atom_concat('MPARCH=', ~get_platform),
		      Cmd]).

invoke_gmake_timingmodel(Cmd) :-
	invoke_gmake(~bundle_path(contrib, 'library/timingmodel'),
	             ['-s', '-j1',
		      ~atom_concat('MPARCH=', ~get_platform),
		      Cmd]).
