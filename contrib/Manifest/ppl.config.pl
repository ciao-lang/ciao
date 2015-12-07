% (included file)

:- doc(section, "Options for PPL Library").

:- bundle_flag(with_ppl, [
    comment("Enable PPL bindings"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended],
      % .....................................................................
      "Set to \"yes\" if you wish to interface with PPL")
]).
		% rule_default(verify_ppl(WithPPL), WithPPL),
%  default_comment("PPL >= 0.9 available"),
%  default_value_comment("PPL has not been detected."),

m_bundle_foreign_config_tool(contrib, ppl, 'ppl-config').

% TODO: it should consider auto_install option!
%% ppl_installed :-
%% 	find_executable(~m_bundle_foreign_config_tool(contrib, ppl), _).
%% 
%% % TODO: not used... reactivate?
%% verify_ppl(Value) :-
%% %	( ppl_installed, ppl_version(V) ->
%% %	    Value = yes
%% %	    message([V])
%% 	( ppl_installed, ppl_version(_) ->
%% 	    Value = yes
%% 	; Value = no
%% 	).
%% 
%% ppl_version(Version, Str) :-
%% 	foreign_config_version(ppl, Str).

:- bundle_flag(auto_install_ppl, [
    comment("Auto-install PPL (third party)"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended],
      % .....................................................................
      "Set to \"yes\" if you want to auto-install PPL (third party)")
]).

