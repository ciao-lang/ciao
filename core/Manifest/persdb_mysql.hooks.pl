% (included file)

:- doc(section, "MySQL bundle").

:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(library(system), [file_exists/1]).

with_mysql := ~get_bundle_flag(core:with_mysql).
mysql_client_directory := ~get_bundle_flag(core:mysql_client_directory).

'$builder_hook'(persdb_mysql:item_prebuild_nodocs) :-
	( with_mysql(yes) ->
	    wr_template(origin,
	        bundle_src(core)/'library'/(~mysql_directory),
	        'linker_opts_auto.pl',
	        ['where_mysql_client_lives' = ~mysql_client_directory]),
	    % TODO: why?
	    ( file_exists(~fsR(bundle_src(core)/library/(~mysql_directory_op))) ->
	        wr_template(origin,
		    bundle_src(core)/'library'/(~mysql_directory_op),
		    'linker_opts_auto.pl',
		    ['where_mysql_client_lives' = ~mysql_client_directory])
	    ; true
	    )
	; true
	).

mysql_directory := 'persdb_mysql'.
mysql_directory_op := 'persdb_mysql_op'.

