:- use_package(inliner).

:- inline get_profile_info/3.
:- unfold get_profile_info(yes, no, no).
:- inline get_flat_info/3.
:- unfold get_flat_info(yes, no, no).
:- inline get_info_pred_from_list/4.
:- inline get_info_item/3.
:- unfold get_info_item(yes, no, no).
:- inline get_profile_flat_total/3.
:- unfold get_profile_flat_total(yes, no, no).
:- inline get_info_total/3.
:- inline get_info_total(yes, no, no).
:- inline get_ecc_info/3.
:- unfold get_ecc_info(yes, no, no).
:- inline get_profile_cc_summary_total/3.
:- unfold get_profile_cc_summary_total(yes, no, no).
:- inline get_profile_cc_summary_total_total/3.
:- unfold get_profile_cc_summary_total_total(yes, no, no).
:- inline get_cc_summary/3.
:- unfold get_cc_summary(yes, no, no).
:- inline get_profile_cc_data/5.
:- unfold get_profile_cc_data(yes, no, no, no, no).
:- inline get_ecc_item/3.
:- unfold get_ecc_item(yes, no, no).
:- inline get_info_pred/3.
:- unfold get_info_pred(yes, no, no).
:- inline get_profile_cc_summary_total_total_item/3.
