:- package(shallow_rt_client_safe).
% Flags for the 'client_safe' mode and shallow run-time checking

:- use_package(library(assertions/pp)).
:- pp_opt(rtchecks_shallow).
:- pp_opt(output).

:- pp_pre([
        (:- use_package(termhide)),   % Do termhide expansion only on ciaopp side (the output will be expanded)
        (:- new_declaration(hide/1)) % is this needed? --NS
          ]).

% :- pp_post([
%    % Options for rtchecks
%    %     (:- set_prolog_flag(rtchecks_level,exports)) % DEPRECATED
% ]).
