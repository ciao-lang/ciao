:- package(shallow_rt_unsafe).
% Flags for the 'unsafe' mode and shallow run-time checking

:- use_package(library(assertions/pp)).
%:- pp_opt(rtchecks_shallow). % TODO: see below
:- pp_opt(output).

:- pp_pre([
        (:- use_package(termhide)), % Do termhide expansion only on ciaopp side (the output will be expanded)
        (:- new_declaration(hide/1))
]).

% TODO: should work without using the rtchecks_shallow package even!
:- pp_post([
        (:- new_declaration('$hidden'/1))
]).
