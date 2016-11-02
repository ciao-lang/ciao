% (included file)

% TODO: Depends on LPdoc options!
:- doc(section, "PiLLoW Options").

:- bundle_flag(pillow_base_htmldir, [
    comment("Base for PiLLoW HTML assets"),
    details(
      % .....................................................................
      "Base directory for PiLLoW HTML assets (by default same as for lpdoc)."),
    rule_default(DefValue, flag(lpdoc:htmldir(DefValue))),
    %
    interactive
]).

:- bundle_flag(pillow_base_htmlurl, [
    comment("Base URL for PiLLoW HTML assets"),
    details(
      % .....................................................................
      "Base URL for PiLLoW HTML assets (by default same as for lpdoc)."),
    rule_default(DefValue, flag(lpdoc:htmlurl(DefValue))),
    %
    interactive
]).
