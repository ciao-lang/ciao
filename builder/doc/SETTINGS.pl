:- module(_, [], [doccfg]).

:- include(core_docsrc(docpaths)).

output_name := 'ciao_builder'.

% TODO: use parent_bundle to share those defs
filepath := '../src'.
filepath := '../cmds'.
filepath := ~ciaofilepath_common.

doc_structure := 
    'ciao_builder_ref'-[
      % TODO: Missing lib/bundle/* modules
      % TODO: Missing bundlehooks (include bintgt, docstgt?)
      %
      % Driver of commands on bundles
      'ciao_builder'-[
        'ciaocl_parser', % TODO: This is for internals
        'ciaocl_help' % TODO: This is for internals
      ],
      'manifest_compiler'-[
        'bundlehooks_holder'
      ],
      % Command driver and definitions
      'builder_cmds'-[
        % Grades (default)
        'grade_bin',
        'grade_docs',
        % Target names and resolution
        'builder_targets',
        % Build
        'third_party_install',
        'third_party_custom',
        'ciaoc_aux',
        'lpdoc_aux',
        'car_maker',
        'eng_defs',
        'builder_aux',
        'install_aux',
        'cmake_aux',
        % Installation
        'info_installer',
        'register_in_script',
        % Other operations on source
        'bundle_hash',
        'detcheader',
        % Auxiliary
        'messages_aux',
        'interactive_aux'
      ],
      % Configuration
      'bundle_configure',
      % Fetching
      'bundle_fetch',
      % Packaging
      'pbundle_generator'-[
        'pbundle_meta',
        'pbundle_gen_bin',
        'pbundle_gen_mac',
        'pbundle_gen_rpm',
        'pbundle_gen_src',
        'pbundle_gen_win32'
      ]
    ].

%doc_mainopts := no_patches.
doc_mainopts := _ :- fail. % Allow patches in main changelog (those are the release notes)
% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_propuses.

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
