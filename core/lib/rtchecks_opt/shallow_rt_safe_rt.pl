:- package(shallow_rt_safe_rt).
% Flags for the 'safe_rt' mode and shallow run-time checking

:- use_package(library(assertions/pp)).
:- pp_opt(rtchecks_shallow).
:- pp_opt(output).

:- pp_pre([
        (:- use_package(termhide)), % Do termhide expansion only on ciaopp side (the output will be expanded)
        (:- new_declaration(hide/1))
]).

% :- pp_post([
%    % Options for rtchecks
%         % (:- set_prolog_flag(rtchecks_level,inner)) % DEPRECATED
% ]).
