% ============================================================================
% Definitions for builder hooks

% (interface via multifiles)
:- multifile m_bundlehook_decl/3.
:- multifile m_bundlehook_do/3.

% :- pred prepare_build_bin/0 # "Prepare source for build_bin".
% :- pred build_bin/0 # "Build cmds and libs (includes prepare_build_bin)".
% :- pred prepare_build_docs/0 # "Prepare source for build_docs".
% :- pred build_docs/0 # "Build documentation (includes prepare_build_docs)".
% :- pred install_bin/0 # "Install".
% :- pred uninstall_bin/0 # "Uninstall".
% :- pred install_docs/0 # "Install".
% :- pred uninstall_docs/0 # "Uninstall".
% :- pred register/0 # "Register (in bash,csh,emacs,etc.)".
% :- pred unregister/0 # "Unregister (in bash,csh,emacs,etc.)".
% :- pred test/0 # "Run tests".
% :- pred bench/0 # "Run benchmarks".
% :- pred custom_run/2 # "Run custom command".

% % Hooks for item_nested
% :- pred item_nested/1 # "Bundle parts (E.g., ciaobase.eng(...))".
%
% :- pred <PrimTgt>/? # "Primitive targets". % (see primtgt/1)
% TODO: complete
% :- pred lib(...).
% :- pred cmd(...).
% :- pred readme(Path, Props) # "@var{Props} contains a @tt{main=D}
%    term, where @var{D} is the (bundle relative) path for a README
%    file (in lpdoc format).  README files are expected to be part of
%    the sources. They are updated to @var{Path} (bundle relative)
%    during documentation build, and they are not removed during
%    clean.".
% % (nondet, default: fail)
% :- pred manual(Name, Props) # "@var{Props} contains a @tt{main=D}
%    term, where @var{D} is the (bundle relative) path for a manual
%    (usually a SETTINGS.pl file). The output manual name is
%    @var{Name}".
% % (nondet, default: fail)

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

% % Hooks for configuration rules

% m_bundle_config_call(<Mod>,G) :- call(G).
% m_bundle_config_entry(<Mod>,Name,Props) :- '$bundleconfig_entry'(Name,<Mod>,Props).
:- multifile m_bundle_config_call/2.
:- multifile m_bundle_config_entry/3.
% m_bundle_foreign_config_tool(<Mod>,Name,Path)
:- multifile m_bundle_foreign_config_tool/3.
% m_bundle_foreign_dep(<Mod>,Kind,Name,Desc)
:- multifile m_bundle_foreign_dep/4.
