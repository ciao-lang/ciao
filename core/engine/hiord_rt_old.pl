:- module(hiord_rt_old, ['$dummy_hiord_rt_old'/0], [noprelude]).
% TODO: remove once hiord_old is deprecated
:- if(defined(optim_comp)).
:- '$props'('$dummy_hiord_rt_old'/0, [impnat=indefinable]).
:- else.
:- impl_defined('$dummy_hiord_rt_old'/0).
:- endif.