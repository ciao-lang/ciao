% (included file)

:- doc(section, "CiaoJava bundle").

:- use_module(ciaobld(config_common), [with_docs/1]).

with_java_interface := ~get_bundle_flag(core:with_java_interface).

'$builder_hook'(java:build_nodocs) :- !,
	( with_java_interface(yes) ->
	    invoke_gmake_javall(build)
	; true
	).
'$builder_hook'(java:build_docs) :- !,
	% TODO: missing installation of docs
	( with_java_interface(yes) ->
	    ( with_docs(yes) ->
	        invoke_gmake_javall(docs)
	    ; true
	    )
	; true
	).

'$builder_hook'(java:clean_norec) :-
	( with_java_interface(yes) ->
	    invoke_gmake_javall(distclean) % TODO: 'clean' or 'distclean'?
	; true
	).

:- use_module(ciaobld(builder_aux), [invoke_gmake/2]).

invoke_gmake_javall(Cmd) :-
	invoke_gmake(~fsR(~javalibs_dir), [Cmd]).

javalibs_dir := bundle_src(core)/library/javall.
