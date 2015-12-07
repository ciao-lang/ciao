% (included file)

:- doc(section, "Profiler (part)").

:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).

% (Special target, already in 'library')
% (called from 'ciao_builder' (for ciaobot))
'$builder_hook'(profiler:build_nodocs) :- build_libs(contrib, 'library/profiler'). % TODO: 
'$builder_hook'(profiler:build_docs) :- !.
'$builder_hook'(profiler:clean_norec) :- !,
	clean_tree(~fsR(bundle_src(contrib)/library/profiler)).

