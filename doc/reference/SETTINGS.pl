:- module(_, [], [lpdoclib(doccfg)]).

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

output_name := 'ciao'.

filepath := at_bundle(core, 'shell').
filepath := at_bundle(core, 'ciaoc').
filepath := at_bundle(core, 'engine').
filepath := at_bundle(core, 'cmds').
filepath := at_bundle(core, 'library/pillow/dist/doc').
filepath := ~ciaofilepath_common.
%
filepath := at_bundle(ciao_emacs, 'elisp').
% TODO: needed in CiaoMode.lpdoc because of @include{README_CIAOPP.lpdoc}
filepath := at_bundle(ciaopp, 'doc/readmes'). % TODO: should not be here
% TODO: move to a separate manual?
filepath := at_bundle(contrib, 'cmds').
filepath := at_bundle(contrib, 'library').

doc_structure := 
        ciao-[
	  (~docstr_getstarted),
	  'DevEnv'-(~docstr_devenv),
	  'Builtins'-(~docstr_refcomponents),
	  'IsoProlog'-(~docstr_isoprolog),
	  'ClassicProlog'-(~docstr_classicprolog),
	  'AnnotatedProlog'-(~docstr_annotatedprolog),
	  'MiscProlog'-(~docstr_miscprolog),
	  'ExtendProlog'-(~docstr_extendprolog),
	  'Interfaces'-(~docstr_interfaces),
	  'ADTs'-(~docstr_adts),
	  'Contrib'-(~docstr_contrib),
	  'ciao-contrib-utilities'-(~docstr_utilscontrib),
 	  'Append'-(~docstr_installation)
        ].

docstr_getstarted :=
	['GetStartUnix',
	 'GetStartWin32'].

docstr_installation :=
	['Install',
	 'InstallWin32bin',
	 'BeyondInstall'].

docstr_devenv :=
	['ciaoc',
	 'toplevel/toplevel_doc',
	 'debugger/debugger_doc'-['debugger/debugger'],
	 'ciao-shell',
	 'ciao-utilities'-(~docstr_utils),
	 'libpaths',
	 'CiaoMode'].

% (those are part of the development environment)
docstr_utils :=
	['ciaodump',
%        'xrefs_doc',
	 'xrefs/callgraph',
	 % 'show_deps'?
%	 'get_deps',
	 'pldiff'].

docstr_refcomponents :=
	['modules', % engine
	 'loading_code', % engine
	 'basiccontrol', % engine
	 'builtin_directives', % engine
	 'basic_props', % engine
	 'term_typing', % engine
	 'term_basic', % engine
	 'term_compare', % engine
	 'atomic_basic', % engine
	 'arithmetic', % engine
	 'streams_basic', % engine
	 'io_basic', % engine
	 'exceptions', % engine
	 'prolog_flags', % engine
	 'data_facts', % engine
	 'syntax_extensions', % engine
	 'io_aux', % engine
	 'attr/attr_doc' - ['attr/attr_rt'],
	 'attributes', % engine
	 'system_info', % engine
	 'condcomp/condcomp_doc',
	 'default_predicates'].

% Should not be used, so we do not document them
% 	'internals'

% Other
% 	'mexpand'

docstr_isoprolog :=
	['iso_doc',
	 'aggregates',
	 'dynamic_rt',
	 'read',
	 'write',
	 'operators',
	 'iso_char',
	 'iso_misc',
	 'iso_incomplete'].

docstr_classicprolog :=
	['dcg/dcg_doc'-['dcg/dcg_phrase_doc'],
	 %
	 'format',
	 'lists',
	 'sort',
	 'compiler/compiler',
	 'between',
	 'system',
	 'prolog_sys',
	 'dec10_io',
	 'old_database',
	 'ttyout',
	 'runtime_ops/runtime_ops_doc'].

%    'classic_doc'

docstr_annotatedprolog :=
	['assertions/assertions_doc',
	 'assertions/assertions_props',
	 'regtypes/regtypes_doc',
	 'assertions/native_props',
	 %
	 'isomodes/isomodes_doc',
	 'basicmodes/basicmodes_doc',
	 'rtchecks/rtchecks_doc',
	 'unittest'-
               ['unittest/unittest_props',
		'unittestdecls_doc',
		% 'unittest/unittest_utils',
		'unittest/unittest_statistics',
		'unittest/unittest_examples'
	       ]
	].

% 'fdtypes'
%	'metatypes'
%	'meta_props'

docstr_miscprolog :=
	['benchmarks/ecrc',
	 'getopts',
	 'llists',
	 'streams',
	 'dict',
	 'strings',
	 'messages',
	 'io_alias_redirection',
	 'read_from_string',
	 'ctrlcclean',
	 'errhandle',
	 'fastrw',
	 'pathnames',
	 'symfnames/symfnames',
	 'file_utils',
	 'file_locks/file_locks',
	 'formulae',
	 'terms',
	 'terms_check',
	 'terms_vars',
	 'cyclic_terms',
	 %
	 'pretty_print',
	 %
	 'assertions/assrt_write',
	 %
	 'librowser/librowser',
	 %
	 'expansion_tools',
	 %
	 'concurrency/concurrency',
	 'conc_aggregates',
	 %
	 'sockets/sockets',
	 'sockets/sockets_io',
         %
	 % TODO: nest
	 'system_extra',
	 'process/process'-['process/process_channel'],
	 %
	 'glob',
	 'source_tree/source_tree',
	 %
	 'archive_files',
	 %
	 'text_template'
	 ].

%    'tokenize',
%     'assrt_lib',
%     'byrd',
% 	'traces',
%     'events',
%     'fly',
%     'listing',
%     'loops',
%     'parse_spec',
%     'prompt'

% TODO: Document: those libraries may change the 'theory'
docstr_extendprolog :=
	['pure/pure_doc',
	 'indexer/indexer_doc',
	 'hiord_rt', % engine
	 'hiordlib',
	 'argnames/argnames_doc',
	 'fsyntax/fsyntax_doc',
	 'global',
	 'andorra/andorra_doc',
         %
	 'det_hook/det_hook_doc',
	 'det_hook/det_hook_rt',
         %
	 'odd',
	 'mutables',
	 'block/block_doc',
	 'freeze/freeze',
	 'when/when',
	 'actmods/actmods_doc',
	 'agent/agent_doc',
	 'bf/bf_doc',
	 'id/id_doc',
	 'clpq/clpq_doc',
	 'clpr/clpr_doc'
	 % 'fuzzy/fuzzy_doc', (moved to its own repo)
         ].

% 'remote_doc',
% 'mattr_global_doc'

with_mysql := ~get_bundle_flag(core:with_mysql).

% TODO: menu is not an interface! (this is for interfaces to other languages)
docstr_interfaces :=
	['foreign_interface/foreign_interface_doc',
	 'foreign_interface/foreign_interface_properties',
	 'foreign_compilation',
	 'foreign_interface/build_foreign_interface',
	 %
	 'menu/menu_doc',
	 'menu/menu_generator',
	 %
	 'davinci/davinci',
	 %
	 'tcltk/tcltk'-['tcltk/tcltk_low_level'],
	 %
%  'window_class_doc',
%    'widget_class_doc',
%      'menu_class_doc',
%      'canvas_class_doc',
%      'button_class_doc',
%      'checkbutton_class_doc',
%      'radiobutton_class_doc',
%      'entry_class_doc',
%      'label_class_doc',
%      'menubutton_class_doc',
%      'menu_entry_class_doc',
%    'shape_class_doc',
%      'arc_class_doc',
%      'oval_class_doc',
%      'poly_class_doc',
%      'line_class_doc',
%      'text_class_doc',
	 'pillow/pillow_doc'-[
	   'pillow/html',
	   'pillow/http',
	   'pillow/pillow_types',
	   'pillow/json'
         ],
         %
	 'persdb/persdb_rt'-[
	   'persdb/persdb_examples'
         ],
         %
	 'factsdb/factsdb_doc'-[
	   'factsdb/factsdb_rt'
         ],
	 ~docstr_persdb_mysql_docs(~with_mysql),
	 % TODO: nest
	 'persdb_sql_common/sqltypes',
	 'persdb_sql_common/persdb_sql_tr',
	 'persdb_sql_common/pl2sqlinsert',
         %
	 'emacs/emacs',
	 'linda'].

%    persdb_sql_common',
%	db_client',

docstr_persdb_mysql_docs(yes) := [
	% TODO: nest
	'persdb_mysql/persdb_mysql_rt',
	'persdb_mysql/pl2sql',
	'persdb_mysql/mysql_client',
	'persdb_mysql/db_client_types'
  ].
docstr_persdb_mysql_docs(no) := [].

docstr_adts :=
	['arrays',
	 'assoc',
	 'counters',
	 'idlists',
	 'numlists',
%	 'patterns',

	 'graphs/graphs',
	 'graphs/ugraphs',
	 'graphs/wgraphs',
	 'graphs/lgraphs',

	 'queues',
	 'random/random',
	 'sets',
	 'vndict'].

%     'bitcodesets',
%     'formulae',
%     'keys',
%     'llists',
%     'lsets'

% (chartlib moved to library.development/)
% 'chartlib/chartlib'-[
%   'chartlib/bltclass',
%   'chartlib/chartlib_errhandle',
%   'chartlib/color_pattern',
%   'chartlib/genbar1',
%   'chartlib/genbar2',
%   'chartlib/genbar3',
%   'chartlib/genbar4',
%   'chartlib/gengraph1',
%   'chartlib/gengraph2',
%   'chartlib/genmultibar',
%   'chartlib/table_widget1',
%   'chartlib/table_widget2',
%   'chartlib/table_widget3',
%   'chartlib/table_widget4',
%   'chartlib/test_format'
% ],
	 
docstr_contrib :=
	['ddlist/ddlist',
%        'debugpred',
	 %
	 'zeromq/zeromq',
	 %
         'dht/dht_doc'-[
	   'dht/dht_client',
	   'dht/dht_server'-[
	     'dht/dht_s2c',
	     'dht/dht_s2s',
	     'dht/dht_logic'-[
	       'dht/dht_routing',
	       'dht/dht_logic_misc',
	       'dht/dht_rpr',
	       'dht/dht_storage'
             ]
           ],
	   % Common modules for both DHT server and client
	   'dht/dht_config',
	   'dht/dht_misc'
         ],
         %
	 'clpfd/clpfd_doc' -[
	   'clpfd/clpfd_rt'],
	 'fd/fd_doc',
	 'gendot/gendot',
	 'gnuplot/gnuplot',
	 'lazy/lazy_doc',
         % 'modtester',
	 'mycin/mycin_doc',
	 'profiler/profiler_doc',
         %
	 'regexp/regexp_doc'-['regexp/regexp_code'],
	 %
	 'tester/tester',
	 % 'time_analyzer/time_analyzer',
	 'xdr_handle/xdr_handle',
	 'xml_path/doc/xml_path_doc'].

docstr_utilscontrib :=
	['cleandirs'].

%doc_mainopts := no_patches.
doc_mainopts := _ :- fail. % Allow patches in main changelog (those are the release notes)
% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_propuses.

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
