% (see improlog_ops)

:- op(50, fx, [(~)]).

:- op(980, xfx, [(<-)]). % priority between (::) and (,)

% Infix dot as method/attribute accessor
:- op(40, yfx, [(.)]).
:- push_prolog_flag(read_infix_dot, on).

% Deref + accessor % TODO: like PASCAL to avoid conflicts with Prolog "->"
:- op(40, yfx, [(^.)]).
% Deref % TODO: like PASCAL?
:- op(40, yf, [(^)]).

% Postfix blocks enable X[I] syntax
:- push_prolog_flag(read_postfix_blocks, on).
% TODO: the following operator definitions allows postfix read of
%   [...] and {...}  Use other way to specify it?

% NOTE: operator name inside a list, to avoid confusing it as an empty
% list of opcodes
:- op(40, yf, ['[]']).
:- op(40, yf, ({})).

% (see pmrule.pl)
% TODO: conflic with priority in assertions! (fix assertions?)
:- op(1150, xfx, [(=>)]). % between (:-) and (;)
