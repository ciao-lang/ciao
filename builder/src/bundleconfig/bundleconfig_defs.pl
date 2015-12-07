% m_bundle_config_call(<Mod>,G) :- call(G).
% m_bundle_config_entry(<Mod>,Name,Props) :- '$bundleconfig_entry'(Name,<Mod>,Props).
:- multifile m_bundle_config_call/2.
:- multifile m_bundle_config_entry/3.
% m_bundle_foreign_config_tool(<Mod>,Name,Path)
:- multifile m_bundle_foreign_config_tool/3.
