% property versions implementation database
% IMPORTANT: keep in sync with all _rtc files, so that every _rtc
% property version is listed here.

% TODO: similar to core/lib/foreign_interface
%       probably merge alike code parts at some point?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(assertions/native_props_rtc)).

:- rtc_impl(native_props:succeeds/1, native_props_rtc:rtc_succeeds/1).

% ----------------------------------------------------------------------
% :- rtc_impl(native_props:mshare/2, native_props_rtc:rtc_mshare/2).
% This property is treated as a special case in rtchecks_tr.pl (see reasons there).

:- rtc_impl(native_props:covered/2, native_props_rtc:rtc_covered/2).

:- rtc_impl(native_props:linear/1, native_props_rtc:rtc_linear/1).

:- use_module(library(assertions/native_props), [indep/1, indep/2]).

:- rtc_impl(native_props:indep/1, native_props:indep/1).

:- rtc_impl(native_props:indep/2, native_props:indep/2).

% ----------------------------------------------------------------------

:- rtc_impl(native_props:det/1, native_props_rtc:rtc_det/1).
% fails/1 is defined in basic_props but we keep the rtc version in native_props_rtc
:- rtc_impl(basic_props:fails/1, native_props_rtc:rtc_fails/1).
:- rtc_impl(native_props:semidet/1, native_props_rtc:rtc_semidet/1).
:- rtc_impl(native_props:multi/1, native_props_rtc:rtc_multi/1).

:- compilation_fact(old_nfdet). % comment this to disable old properties

:- if(defined(old_nfdet)).
:- rtc_impl(native_props:is_det/1, native_props_rtc:rtc_is_det/1).
:- rtc_impl(native_props:non_det/1, native_props_rtc:rtc_non_det/1).
:- rtc_impl(native_props:not_fails/1, native_props_rtc:rtc_not_fails/1).
:- endif.

% ----------------------------------------------------------------------

:- rtc_impl(native_props:num_solutions/2, native_props_rtc:rtc_num_solutions/2).

:- rtc_impl(native_props:relations/2, native_props_rtc:rtc_relations/2).

:- rtc_impl(native_props:solutions/2, native_props_rtc:rtc_solutions/2).

:- rtc_impl(native_props:no_choicepoints/1, native_props_rtc:rtc_no_choicepoints/1).

:- rtc_impl(native_props:leaves_choicepoints/1, native_props_rtc:rtc_leaves_choicepoints/1).

% ----------------------------------------------------------------------

:- rtc_impl(native_props:exception/2, native_props_rtc:rtc_exception/2).

:- rtc_impl(native_props:exception/1, native_props_rtc:rtc_exception/1).

:- rtc_impl(native_props:no_exception/2, native_props_rtc:rtc_no_exception/2).

:- rtc_impl(native_props:no_exception/1, native_props_rtc:rtc_no_exception/1).

% ----------------------------------------------------------------------

:- rtc_impl(native_props:signal/1, native_props_rtc:rtc_signal/1).

:- rtc_impl(native_props:signal/2, native_props_rtc:rtc_signal/2).

:- rtc_impl(native_props:no_signal/1, native_props_rtc:rtc_no_signal/1).

:- rtc_impl(native_props:no_signal/2, native_props_rtc:rtc_no_signal/2).

% ----------------------------------------------------------------------

:- rtc_impl(native_props:constraint/1, native_props_rtc:rtc_constraint/1).

:- rtc_impl(native_props:user_output/2, native_props_rtc:rtc_user_output/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(engine(basic_props_rtc)).

% :- rtc_impl(basic_props:term/1, basic_props_rtc:rtc_term/1).

:- rtc_impl(basic_props:int/1, basic_props_rtc:rtc_int/1).

:- rtc_impl(basic_props:nnegint/1, basic_props_rtc:rtc_nnegint/1).

:- rtc_impl(basic_props:num/1, basic_props_rtc:rtc_num/1).

:- rtc_impl(basic_props:atm/1, basic_props_rtc:rtc_atm/1).

:- rtc_impl(basic_props:struct/1, basic_props_rtc:rtc_struct/1).

:- rtc_impl(basic_props:gnd/1, basic_props_rtc:rtc_gnd/1).

:- rtc_impl(basic_props:cgoal/1, basic_props_rtc:rtc_cgoal/1).
