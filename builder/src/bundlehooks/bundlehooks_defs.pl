% ============================================================================
% Definitions for builder hooks

% (interface via multifiles)
:- multifile m_bundlehook_decl/3.
:- multifile m_bundlehook_do/3.

% :- pred desc_name(X) # "@var{X} is the textual name of the bundle".
% :- pred readme_path(D) # "@var{D} is the (bundle relative) path
%    for a README file (in lpdoc format)".
% % (nondet, default: fail)
% :- pred manual_dir(D) # "@var{D} is the (bundle relative)
%    subdirectory where each manual is found (containing a SETTINGS.pl
%    file)".
% % (nondet, default: fail)

% :- pred prebuild_nodocs/0 # "Prepare source for build_nodocs".
% :- pred build_nodocs/0 # "Build cmds and libs (includes prebuild_nodocs)".
% :- pred build_bin/0 # "Build cmds".
% :- pred build_libraries/0 # "Build libs".

% :- pred item_prebuild_nodocs/0 # "prebuild_nodocs for item".
% :- pred item_build_nodocs/0 # "build_nodocs for item".
% :- pred item_clean_norec/0 # "clean_norec for item".

% :- pred prebuild_docs/0 # "Prepare source for build_docs". 

% % Hooks for bundle (un)installation
% :- pred install/0 # "Install".
% :- pred uninstall/0 # "Uninstall".

% % Hooks for bundle (un)registry
% :- pred register/0 # "Register (in bash,csh,emacs,etc.)".
% :- pred unregister/0 # "Unregister (in bash,csh,emacs,etc.)".

% % Hooks for bundle testing/benchmarking
% :- pred runtests/0 # "Tests".
% :- pred runbenchmarks/0 # "Benchmarks".

% % Hooks for bundle custom commands
% :- pred custom_run/1 # "Run custom command".

% % Hooks for bundleitem definition
% :- pred item_def/1 # "Definition of some bundleitem".

% % Subtargets (experimental)
% :- pred item_subs/1 # "".

% % Hooks for bundleitem (un)installation
% :- pred item_install/0 # "install for item".
% :- pred item_uninstall/0 # "uninstall for item".

% % Hooks for bundleitem (un)registry
% :- pred item_register/0 # "register for item".
% :- pred item_unregister/0 # "unregister for item".

% % Hooks for third-party components
:- discontiguous m_third_party_name/1.
:- multifile m_third_party_name/1.
:- discontiguous m_third_party_version/2.
:- multifile m_third_party_version/2.
:- discontiguous m_third_party_source_url/2.
:- multifile m_third_party_source_url/2.
:- discontiguous m_third_party_source_md5/2.
:- multifile m_third_party_source_md5/2.
:- discontiguous m_third_party_patch/2.
:- multifile m_third_party_patch/2.
:- discontiguous m_third_party_build_system/2.
:- multifile m_third_party_build_system/2.
:- discontiguous m_third_party_option1/2.
:- multifile m_third_party_option1/2.
:- discontiguous m_third_party_option2/3.
:- multifile m_third_party_option2/3.
:- discontiguous m_third_party_custom_configure/2.
:- multifile m_third_party_custom_configure/2.
:- discontiguous m_third_party_custom_build/2.
:- multifile m_third_party_custom_build/2.
:- discontiguous  m_third_party_custom_install/2.
:- multifile m_third_party_custom_install/2.
