% property versions implementation database
% IMPORTANT: keep in sync with all _rtc files, so that every _rtc
% property version is listed here.

% TODO: similar to core/lib/foreign_interface
%       probably merge alike code parts at some point?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(assertions/native_props_rtc)).


:- rtc_impl(native_props:succeeds/1, native_props_rtc:rtc_succeeds/1).

% ----------------------------------------------------------------------
:- rtc_impl(native_props:mshare/1, native_props_rtc:rtc_mshare/1).

:- rtc_impl(native_props:covered/2, native_props_rtc:rtc_covered/2).

:- rtc_impl(native_props:linear/1, native_props_rtc:rtc_linear/1).

% ----------------------------------------------------------------------

:- rtc_impl(native_props:is_det/1, native_props_rtc:rtc_is_det/1).

:- rtc_impl(native_props:non_det/1, native_props_rtc:rtc_non_det/1).

% ----------------------------------------------------------------------

:- rtc_impl(native_props:not_fails/1, native_props_rtc:rtc_not_fails/1).

:- rtc_impl(native_props:fails/1, native_props_rtc:rtc_fails/1).

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

:- rtc_impl(native_props:user_output/2, native_props_rtc:rtc_user_output/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(engine(term_typing_rtc)).

:- rtc_impl(term_typing:ground/1, term_typing_rtc:rtc_ground/1).
