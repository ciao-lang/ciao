% ============================================================================
% Definitions for builder hooks

% (interface via multifiles)
:- multifile m_bundlehook_decl/3.
:- multifile m_bundlehook_do/3.

% :- pred prebuild_nodocs/0 # "Prepare source for build_nodocs".
% :- pred build_nodocs/0 # "Build cmds and libs (includes prebuild_nodocs)".

% :- pred prebuild_docs/0 # "Prepare source for build_docs". 

% % Hooks for bundle (un)installation
% :- pred install/0 # "Install".
% :- pred uninstall/0 # "Uninstall".

% % Hooks for bundle (un)registry
% :- pred register/0 # "Register (in bash,csh,emacs,etc.)".
% :- pred unregister/0 # "Unregister (in bash,csh,emacs,etc.)".

% % Hooks for bundle testing/benchmarking
% :- pred test/0 # "Run tests".
% :- pred runbenchmarks/0 # "Run benchmarks".

% % Hooks for bundle custom commands
% :- pred custom_run/2 # "Run custom command".

% % Hooks for bundleitem definition
% :- pred item_def/1 # "Definition of some bundleitem".

% % Hooks for bundle_def definition
% :- pred bundle_def/1 # "Definition of this bundle (libs, cmds)".
%
% (contents of bundle_def)
%
% TODO: complete
%
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

% % Subtargets (experimental)
% :- pred item_subs/1 # "".

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
