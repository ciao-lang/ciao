% (included file)

:- doc(section, "Profiler (part)").

:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).

'$builder_hook'(profiler:lib('library/profiler')).
'$builder_hook'(profiler:clean_bin) :- !,
	% TODO: Remove hook if lib/2 is treated by clean_bin
	clean_tree(~bundle_path(contrib, 'library/profiler')).

