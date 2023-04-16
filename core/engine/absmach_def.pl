% (included file)

% ---------------------------------------------------------------------------
% Abstract machine definition (emugen version)
% NOTE: See core/engine_oc/absmach_def.pl for ImProlog version

% Author: Jose F. Morales (based on the original code in C)

% Instruction set and auxiliary data structures for the bytecode
% emulator. The specification in this file is translated to C files,
% which are included in the engine runtime code to generate a working
% bytecode emulator.

% ---------------------------------------------------------------------------

% TODO: Better translation
%  - document 'emugen'
%  - add loop detection
%  - support lvalues, rvalues
% TODO: Missing from optim_comp:
%  - automatic alignment (q versions)
%  - instruction merging
%  - instruction specialization

% ---------------------------------------------------------------------------
%! # C syntax and control constructs

:- rkind(fmtbb/0, [grammar]).
fmtbb => [[ indent(N) ]], tk_bb(N).

:- rkind(fmtinc/1, [grammar]).
fmtinc(I) =>
    [[ indent(N) ]],
    [[ N1 is N + I ]],
    [[ update(indent(N1)) ]].

:- rkind(stmtend/0, [grammar]).
stmtend => tk(';'), tk_nl.

:- rkind('{'/0, [grammar]).
'{' => tk('{').
:- rkind('}'/0, [grammar]).
'}' => tk('}').
:- rkind('('/0, [grammar]).
'(' => tk('(').
:- rkind(')'/0, [grammar]).
')' => tk(')').
:- rkind(' '/0, [grammar]).
' ' => tk(' ').
:- rkind(';'/0, [grammar]).
';' => tk(';').

:- rkind(paren/1, [grammar]).
paren(Exp) => '(', Exp, ')'.

:- rkind(blk/1, [grammar]).
blk(Code) => '{', fmtinc(2), tk_nl, Code, fmtinc(-2), fmtbb, '}', tk_nl.

:- rkind(for/2, [grammar]).
for(Range, Code) => tk('for'), ' ', paren(Range), ' ', blk(Code), tk_nl.

:- rkind(foreach/4, [grammar]).
foreach(Ty, range(To), V, Code) => % V in [0,..., To-1]
    for((localv(Ty, V, 0), (V < To), ';', assign0(V+1)), Code).
foreach(Ty, revrange(From), V, Code) => % V in [From,...,0+1] (reverse)
    for((localv(Ty, V, From), (V > 0), ';', assign0(V-1)), Code).
foreach(Ty, revrange(From,To), V, Code) => % V in [From,...,To+1] (reverse)
    for((localv(Ty, V, From), (V > To), ';', assign0(V-1)), Code).
foreach(Ty, revrangeq(From,To,Step), V, Code) => % V in [From,...,To] by Step (reverse)
    for((localv(Ty, V, From), (V >= To), ';', assign0(V-Step)), Code).

:- rkind(do_while/2, [grammar]).
do_while(Code, Cond) =>
    tk('do'), ' ', blk(Code), ' ', tk('while'), ' ', paren(expr(Cond)), stmtend.

:- rkind(if/2, [grammar]).
if(Cond, Then) =>
    tk('if'), ' ', paren(expr(Cond)), ' ', blk(Then), tk_nl.

:- rkind(if/3, [grammar]).
if(Cond, Then, Else) =>
    tk('if'), ' ', paren(expr(Cond)), ' ', blk(Then), ' ', tk('else'), ' ',
    ( [[ Else = if(_,_) ]] -> Else
    ; [[ Else = if(_,_,_) ]] -> Else
    ; blk(Else), tk_nl
    ).

:- rkind(switch/2, [grammar]).
switch(Expr, Cases) =>
    tk('switch'), ' ', paren(expr(Expr)), ' ', blk(Cases).

:- rkind(vardecl/2, [grammar]).
vardecl(Type, V) =>
    ( [[ Type = extern(Type0) ]] ->
        tk('extern'), ' ', vardecl(Type0, V)
    ; ty(Type), ' ', V, stmtend
    ).

:- rkind(vardecl/3, [grammar]).
vardecl(Type, V, A) => ty(Type), ' ', V, ' ', tk('='), ' ', A, stmtend.

:- rkind(argdecl/2, [grammar]).
argdecl(Type, V) => ty(Type), ' ', V.

:- rkind((<-)/2, [grammar]).
(A <- B) => A, ' ', tk('='), ' ', B, stmtend.

:- rkind(assign/1, [grammar]).
assign(X) => assign0(X), stmtend.

:- rkind(assign0/1, [grammar]).
assign0(X+Y), [[ Y = 1 ]] => X, tk('++').
assign0(X+Y) => X, tk('+='), Y.
assign0(X-Y), [[ Y = 1 ]] => X, tk('--').
assign0(X-Y) => X, tk('-='), Y.
assign0(X*Y) => X, tk('*='), Y.
assign0(X/\Y) => X, tk('&='), Y.

:- rkind(label/1, [grammar]).
label(A) => tk(A), tk(':'), tk_nl.

:- rkind(case/1, [grammar]).
case(A), [[ atom(A) ]] => tk('case'), ' ', tk(A), tk(':'), tk_nl.
case(A) => tk('case'), ' ', A, tk(':'), tk_nl.

:- rkind(goto/1, [grammar]).
goto(A) => tk('goto'), ' ', tk(A), stmtend.

:- rkind(break/0, [grammar]).
break => tk('break'), stmtend.

:- rkind(return/0, [grammar]).
return => tk('return'), stmtend.

:- rkind(return/1, [grammar]).
return(A) => tk('return'), ' ', A, stmtend.

:- rkind(call0/1, [grammar]).
call0(X) => tk(X), stmtend.

:- rkind(call/2, [grammar]).
callstmt(X, Args) => '$fcall'(X, Args), stmtend.

% new id for a variable
:- rkind(var_id/1, []).
var_id(Id) => [[ newid(vr,Id) ]].

:- rkind(label_blk/2, []).
label_blk(Label, Code) => label(Label), maybe_blk(Code).

:- rkind(cond_label_blk/2, []).
% A block whose label is removed if unused (see 'use_label')
ulabel_blk(Label, Code) =>
    cond_blk(use_label(Label), label(Label)),
    maybe_blk(Code).

% Goto to a ulabel
ugoto(Label) => prim('$decl'(use_label(Label))), goto(Label).

:- '$decl'(use_label/1).
use_label(Label) :- put(use_label(Label), true).

:- rkind(case_blk/2, []).
case_blk(X, Code) => case(X), maybe_blk(Code).

:- rkind(localv/2, []).
localv(Ty, V) => var_id(V), vardecl(Ty, V).

:- rkind(localv/3, []).
localv(Ty, V, Val) => var_id(V), vardecl(Ty, V, Val).

% Block only if vardecls where emited
maybe_blk(G) =>
    ( [[ emugen_id_counter(N0) ]] -> true ; [[ N0 = 0 ]] ),
    '$tr'(G, X),
    [[ emugen_id_counter(N) ]],
    [[ Nd is N-N0 ]],
    ( [[ Nd = 0 ]] -> X ; blk(X) ).

:- rkind(field/2, [grammar]). % (structure field initialization)
field(Name, Val) => tk('.'), tk(Name), ' ', tk('='), ' ', Val.

% priority(expr, 170).
% priority(assignment, 160).
% priority(conditional, 150).
% %
% priority(logical_OR, 140).
% priority(logical_AND, 130).
% priority(inclusive_OR, 120).
% priority(exclusive_OR, 110).
% priority(bitwise_AND, 100).
% priority(equality, 90).
% priority(relational, 80).
% priority(shift, 70).
% priority(additive, 60).
% priority(multiplicative, 50).
% priority(unary, 30).
% priority(postfix, 20).
% priority(primary, 10).
% %
% priority(revdecl, 20).
% priority(arraydecl, 10).

:- rkind(expr/1, [grammar]).
expr(X) => expr_prior(170, X).

% TODO: partially move to emugen_tr to speed up
% expression parenthesis, if needed
:- rkind(expr_q/2, [grammar]).
expr_q(Prio, X), [[ prio(CurrPrio), Prio =< CurrPrio ]] => X.
expr_q(_Prio, X) => paren(expr(X)).

:- rkind(expr_prior/2, [grammar]).
expr_prior(Prio, X) => '$with'(prio(Prio), X).

:- rkind(unary_op/4, [grammar]).
unary_op(Op, X, Prio, Assoc) =>
    op_ass(Assoc, _, Prio, PrioX),
    expr_q(Prio, (tk(Op), expr_prior(PrioX, X))).

:- rkind(bin_op/5, [grammar]).
bin_op(Op, X, Y, Prio, Assoc) =>
    op_ass(Assoc, PrioX, Prio, PrioY),
    expr_q(Prio, (expr_prior(PrioX, X), tk(Op), expr_prior(PrioY, Y))).

% TODO: duplicated from library(operators)
op_ass(fy, 0, Prec, Prec) => true.
op_ass(fx, 0, Prec, Less) => [[Less is Prec-1]].
op_ass(yfx, Prec, Prec, Less) => [[Less is Prec-1]].
op_ass(xfy, Less, Prec, Prec) => [[Less is Prec-1]].
op_ass(xfx, Less, Prec, Less) => [[Less is Prec-1]].
op_ass(yf, Prec, Prec, 0) => true.
op_ass(xf, Less, Prec, 0) => [[Less is Prec-1]].

% TODO: (use write_c.pl operators, see compiler_oc/write_c.pl too)
% TODO: get prio of X,Y, add paren only if needed
X+Y => bin_op('+', X, Y, 60, yfx).
X-Y => bin_op('-', X, Y, 60, yfx).
X*Y => bin_op('*', X, Y, 50, yfx).
%:- rkind(('\006\postfix_block')/2, []). % both [_] and {_}
X[Y] => expr_q(20, (expr_prior(20, X), tk('['), expr(Y), tk(']'))).
% deref+access operator ("->" in C)
X^.Y => expr_q(20, (expr_prior(20, X), tk('->'), tk(Y))).
%:- rkind(('\006\dot')/2, []). % TODO: functor name is not '.' here
(X.Y) => expr_q(20, (expr_prior(20, X), tk('.'), tk(Y))).
% deref operator ("*" in C)
X^ => unary_op('*', X, 30, fy).
% address
addr(X) => unary_op('&', X, 30, fy).
% type cast
cast(Ty,X) => expr_q(30, (paren(ty(Ty)), expr_prior(30, X))).

not(X) => unary_op('!', X, 30, fy).
X<Y => bin_op('<', X, Y, 80, yfx).
X>Y => bin_op('>', X, Y, 80, yfx).
X=<Y => bin_op('<=', X, Y, 80, yfx).
X>=Y => bin_op('>=', X, Y, 80, yfx).
X==Y => bin_op('==', X, Y, 90, yfx).
X\==Y => bin_op('!=', X, Y, 90, yfx).
X/\Y => bin_op('&', X, Y, 100, yfx).
X\/Y => bin_op('|', X, Y, 120, yfx).
logical_and(X,Y) => bin_op('&&', X, Y, 130, yfx).
logical_or(X,Y) => bin_op('||', X, Y, 140, yfx).

% $emu_globals and other constants
:- rkind((~)/1, [grammar]).
~(X) => '$with'(f, '$unfold'(X)). % (unfold for e.g., 'true' vs prim)

w, [[f]]=> tk(w).
g, [[f]]=> tk('G').
s, [[f]]=> tk('S').
e, [[f]]=> tk('E').
b, [[f]]=> tk('B').
p, [[f]]=> tk('P').
null, [[f]]=> tk('NULL').
true, [[f]]=> tk('TRUE').
false, [[f]]=> tk('FALSE').
func, [[f]]=> tk('Func').

:- rkind(is_null/1, []).
is_null(X) => X == tk('NULL').
:- rkind(not_null/1, []).
not_null(X) => X \== tk('NULL').

:- rkind('$unreachable'/0, []).
% Make sure that no mode dependant code appears next
% TODO: better way?
'$unreachable' => [[ update(mode('?')) ]].

% TODO: write a 'mode merge' too

call_fC(Ty,F,Args) => paren(cast(Ty,F)), '$fcall'('',[(~w)|Args]).

cfun_eval(Name,Args) => '$fcall'(Name, [(~w)|Args]).

cbool_succeed(Name,Args) => '$fcall'(Name, [(~w)|Args]).

cvoid_call(Name,Args) => callstmt(Name, [(~w)|Args]).

% ---------------------------------------------------------------------------
%! # C preprocessor macros

% C preprocessor

:- rkind(cpp_define/2, [grammar]).
cpp_define(Name, Value) => tk('#define'), ' ', tk(Name), ' ', Value, tk_nl.

:- rkind(cpp_if_defined/1, [grammar]).
cpp_if_defined(Name) => tk('#if'), ' ', tk('defined'), paren(tk(Name)), tk_nl.

:- rkind(cpp_endif/0, [grammar]).
cpp_endif => tk('#endif'), tk_nl.

% ---------------------------------------------------------------------------
%! # C types

ty(int) => tk('int').
ty(intmach) => tk('intmach_t').
ty(tagged) => tk('tagged_t').
ty(try_node) => tk('try_node_t').
ty(goal_descriptor) => tk('goal_descriptor_t').
ty(definition) => tk('definition_t').
ty(bcp) => tk('bcp_t').
ty(choice) => tk('choice_t').
ty(frame) => tk('frame_t').
ty(worker) => tk('worker_t').
ty(sw_on_key_node) => tk('sw_on_key_node_t').
ty(sw_on_key) => tk('sw_on_key_t').
ty(instance_clock) => tk('instance_clock_t').
%
ty(cbool0) => tk('cbool0_t').
ty(cbool1) => tk('cbool1_t').
ty(cbool2) => tk('cbool2_t').
ty(cbool3) => tk('cbool3_t').
ty(ctagged1) => tk('ctagged1_t').
ty(ctagged2) => tk('ctagged2_t').
%
ty(ftype_ctype(f_i_signed)) => '$fcall'('FTYPE_ctype', [tk('f_i_signed')]).
ty(ftype_ctype(X)) => '$fcall'('FTYPE_ctype', [X]).
%
ty(ptr(Ty)) => ty(Ty), ' ', tk('*').

sizeof(tagged) => '$fcall'('sizeof', [ty(tagged)]).
sizeof(sw_on_key_node) => '$fcall'('sizeof', [ty(sw_on_key_node)]).

% ---------------------------------------------------------------------------
%! # C functions

% Worker state
x(Xn) => '$fcall'('X', [Xn]).
y(Yn) => '$fcall'('Y', [Yn]).

bc_off(A,B), [[f]]=> '$fcall'('BCoff', [A, B]).
bc_fetch_opcode, [[f]]=> '$fcall'('BcFetchOPCODE', []).

% (see op_macros/0)
bcp(f_b,N), [[f]]=> '$fcall'('BcP', [tk('f_t'),N]). % TODO: treated as f_t just for casting, better way?
bcp(f_E,N), [[f]] => '$fcall'('BcP', [tk('f_p'),N]). % TODO: treated as f_p
bcp(f_g,N), [[f]] => '$fcall'('BcP', [tk('f_l'),N]). % TODO: treated as f_l (for addr)
bcp(FType,N), [[f]] => [[ Id = tk(FType) ]], '$fcall'('BcP', [Id,N]).

tagp(hva,Ptr), [[f]]=> '$fcall'('Tagp', [tk('HVA'),Ptr]).
tagp(sva,Ptr), [[f]]=> '$fcall'('Tagp', [tk('SVA'),Ptr]).
tagp(cva,Ptr), [[f]]=> '$fcall'('Tagp', [tk('CVA'),Ptr]).
tagp(str,Ptr), [[f]]=> '$fcall'('Tagp', [tk('STR'),Ptr]).
tagp(lst,Ptr), [[f]]=> '$fcall'('Tagp', [tk('LST'),Ptr]).

tagp_ptr(lst,T), [[f]]=> '$fcall'('TagpPtr', [tk('LST'),T]).

tagged_is(cva,T), [[f]]=> '$fcall'('TaggedIsCVA', [T]).
tagged_is(hva,T), [[f]]=> '$fcall'('TaggedIsHVA', [T]).
tagged_is(str,T), [[f]]=> '$fcall'('TaggedIsSTR', [T]).
tagged_is(sva,T), [[f]]=> '$fcall'('TaggedIsSVA', [T]).

is_atomic(T), [[f]]=> '$fcall'('IsAtomic',[T]).
is_var(T), [[f]]=> '$fcall'('IsVar',[T]).

bind(hva, T0, T1) => callstmt('BindHVA', [T0,T1]).
bind(cva, T0, T1) => callstmt('BindCVA', [T0,T1]).
bind(sva, T0, T1) => callstmt('BindSVA', [T0,T1]).

tagged_to_arg(T,N), [[f]]=> '$fcall'('TaggedToArg', [T, N]).
tagged_to_functor(T), [[f]]=> '$fcall'('TaggedToFunctor', [T]).
tagged_to_head_functor(T), [[f]]=> '$fcall'('TaggedToHeadfunctor', [T]).
tagged_to_instance(T), [[f]]=> '$fcall'('TaggedToInstance', [T]).
tagged_to_pointer(T), [[f]]=> '$fcall'('TaggedToPointer', [T]).
tagged_to_root(T), [[f]]=> '$fcall'('TaggedToRoot', [T]).

cond_stack_var(A), [[f]]=> '$fcall'('CondStackvar', [A]).
younger_stack_var(A,B), [[f]]=> '$fcall'('YoungerStackVar', [A,B]).

choice_arity(A), [[f]]=> '$fcall'('ChoiceArity', [A]).
choice_cont0(A,B), [[f]]=> '$fcall'('ChoiceCont0', [A,B]).
choice_from_tagged(A), [[f]]=> '$fcall'('ChoiceFromTagged', [A]).
choice_offset(A,B), [[f]]=> '$fcall'('ChoiceOffset', [A,B]).
choice_to_tagged(A), [[f]]=> '$fcall'('ChoiceToTagged', [A]).
choice_younger(A,B), [[f]]=> '$fcall'('ChoiceYounger', [A,B]).

heap_char_available(A), [[f]]=> '$fcall'('HeapCharAvailable', [A]).
heap_char_difference(A,B), [[f]]=> '$fcall'('HeapCharDifference', [A,B]).
heap_next(A), [[f]]=> '$fcall'('HeapNext', [A]).
heap_offset(A,B), [[f]]=> '$fcall'('HeapOffset', [A,B]).

is_blocking(A), [[f]]=> '$fcall'('IS_BLOCKING', [A]).

is_deep, [[f]]=> '$fcall'('IsDeep',[]).
is_shallow_try, [[f]]=> '$fcall'('IsShallowTry', []).

frozen_chpt(A), [[f]]=> '$fcall'('FrozenChpt', [A]).

node_global_top(A), [[f]]=> '$fcall'('NodeGlobalTop', [A]).
node_local_top(A), [[f]]=> '$fcall'('NodeLocalTop', [A]).

find_definition(A,B,C,D), [[f]]=> '$fcall'('find_definition', [A,B,C,D]).

test_cint_event, [[f]]=> '$fcall'('TestCIntEvent', []).
test_event_or_heap_warn_overflow(A), [[f]]=> '$fcall'('TestEventOrHeapWarnOverflow', [A]).
get_wake_count, [[f]]=> '$fcall'('WakeCount', []).

get_atom(A), [[f]]=> '$fcall'('GET_ATOM', [A]).
get_small(T), [[f]]=> '$fcall'('GetSmall', [T]).
large_size(B), [[f]]=> '$fcall'('LargeSize',[B]).
make_small(A), [[f]]=> '$fcall'('MakeSmall', [A]).
set_arity(A,B), [[f]]=> '$fcall'('SetArity', [A,B]).
is_nonvar_lst(T), [[f]]=> '$fcall'('TermIsLST', [T]).

pointer_to_term(A), [[f]]=> '$fcall'('PointerToTerm',[A]).
term_to_pointer_or_null(Ty, X), [[f]]=> '$fcall'('TermToPointerOrNull', [ty(Ty), X]).

off_stacktop(A,B), [[f]]=> '$fcall'('OffStacktop', [A,B]).
offset(A,B), [[f]]=> '$fcall'('Offset', [A,B]).
sw_on_key_node_from_offset(Htab, T), [[f]]=> '$fcall'('SW_ON_KEY_NODE_FROM_OFFSET', [Htab, T]).
stack_char_offset(A, EnvSize), [[f]]=> '$fcall'('StackCharOffset', [A,EnvSize]).
trail_top_unmark(A), [[f]]=> '$fcall'('TrailTopUnmark', [A]).
trail_younger(A, B), [[f]]=> '$fcall'('TrailYounger', [A,B]).

alloc0(E) => callstmt('CODE_ALLOC', [E]).
choice_new0(B,Alt,H) => callstmt('CODE_CHOICE_NEW0', [B, Alt, H]).
choice_patch(Chpt,Alt) => callstmt('CODE_CHOICE_PATCH', [Chpt, Alt]).

get_frame_top(A,B,C) => callstmt('GetFrameTop', [A,B,C]).

constr_hva(H) => callstmt('ConstrHVA', [H]).
heap_push0(H,X) => callstmt('HeapPush', [H, X]).

load_cva(A,H) => callstmt('LoadCVA', [A, H]).
load_hva(A,H) => callstmt('LoadHVA', [A, H]).
preload_hva(A, H) => callstmt('PreLoadHVA', [A, H]).
load_sva(A) => callstmt('LoadSVA', [A]).
push_ref_heap_next(Pt1,Pt2) => callstmt('PushRefHeapNext', [Pt1,Pt2]).
ref_heap(T,S) => callstmt('RefHeap', [T,S]).
ref_heap_next0(A,S) => callstmt('RefHeapNext', [A,S]).
ref_sva(T0,T1) => callstmt('RefSVA', [T0,T1]).

set_choice(B) => callstmt('SetChoice', [B]).
set_deep => callstmt('SetDeep', []).
set_e(E) => callstmt('SetE', [E]).
set_event => callstmt('SetEvent', []).
set_shallow_retry => callstmt('SetShallowRetry', []).
set_func(Func) => callstmt('Setfunc', [Func]).

conc_chpt_cleanup(A,B) => callstmt('ConcChptCleanUp', [A,B]).
cond_var_wait(A,B) => callstmt('Cond_Var_Wait', [A,B]).
incr_counter(Counter) => callstmt('INCR_COUNTER', [Counter]).
neck_retry_patch(B) => callstmt('NECK_RETRY_PATCH', [B]).
pred_trace0(Kind, Func) => callstmt('PredTrace', [Kind, Func]).
release_lock(A) => callstmt('Release_lock', [A]).
reset_wake_count => callstmt('ResetWakeCount', []).
serious_fault(A) => callstmt('SERIOUS_FAULT', [A]).
setup_pending_call(E,Func) => callstmt('SETUP_PENDING_CALL', [E,Func]).
trace_chpt_cut(B) => callstmt('TRACE_CHPT_CUT', [B]).
trail_push_check(A,B) => callstmt('TrailPushCheck', [A,B]).
unset_event => callstmt('UnsetEvent', []).
wait_acquire_lock(A) => callstmt('Wait_Acquire_lock', [A]).
abort => callstmt('abort', []).
push_choicept(A,B) => callstmt('push_choicept', [A,B]).

deref(T0,X) => callstmt('DEREF', [T0,X]).

% ---------------------------------------------------------------------------
%! # WAM loop state
%  (including compile-time state for versioning (read/write mode))

% TODO: make mode spec optional
% TODO: define versioned decls, declargs for musttail, etc.

% Local variable declarations for the WAM loop
wam_loop_decls =>
    vardecl(bcp, tk('p')),
    vardecl(ptr(try_node), tk('alts')),
    vardecl(ptr(choice), tk('b')), % TODO:[merge-oc] B
    vardecl(ptr(frame), tk('e')), % TODO:[merge-oc] E
    vardecl(ptr(tagged), tk('cached_r_h')), % TODO:[merge-oc] H
    vardecl(ptr(tagged), tk('r_s')), % TODO:[merge-oc] S
    %
    vardecl(intmach, tk('ei')), % (parameter of switch_on_pred, switch_on_pred_sub, call4)
    vardecl(bcp, tk('ptemp'), ~null), % (parameter of escape_to_p, escape_to_p2)
    %
    tk('alts') <- ~null,
    tk('b') <- ~null,
    tk('e') <- ~null,
    tk('cached_r_h') <- ~null,
    tk('r_s') <- ~null,
    %
    tk('ei') <- tk('~0').

% mode-versioned labels
vlabel(X, Label), [[ mode(r) ]] => [[ atom_concat('r_', X, Label) ]].
vlabel(X, Label), [[ mode(w) ]] => [[ atom_concat('w_', X, Label) ]].

% Load/store local copies of worker registers
regload('H') => tk('H') <- (~w)^.heap_top.
regstore('H') => (~w)^.heap_top <- tk('H').

cachedreg('H',H), [[ mode(r) ]] => [[ H = (~w)^.heap_top ]].
cachedreg('H',H), [[ mode(w) ]] => [[ H = tk('H') ]].

% Switch the read/write mode
%  - read mode: H in memory
%  - write mode: H in register
setmode(w), [[ mode(r) ]] =>
    regload('H'),
    [[ update(mode(w)) ]].
setmode(r), [[ mode(w) ]] =>
    regstore('H'),
    [[ update(mode(r)) ]].
setmode(M), [[ mode(M) ]] => true.

% Switch mode and update H simulateneously
% (this avoids an unnecessary StoreH in w->r switch)
setmode_setH(r, NewH), [[ mode(r) ]] => (~w)^.heap_top <- NewH.
setmode_setH(r, NewH), [[ mode(w) ]] =>
    [[ update(mode(r)) ]],
    (~w)^.heap_top <- NewH.

% ---------------------------------------------------------------------------
%! # Terms

:- rkind(sw_on_heap_var/4, []).
sw_on_heap_var(Reg, HVACode, CVACode, NVACode) =>
    localv(tagged, Aux),
    callstmt('SwitchOnHeapVar', [Reg, Aux, blk(HVACode), blk(CVACode), blk(NVACode)]).

:- rkind(sw_on_var/5, []).
sw_on_var(Reg, HVACode, CVACode, SVACode, NVACode) =>
    localv(tagged, Aux),
    callstmt('SwitchOnVar', [Reg, Aux, blk(HVACode), blk(CVACode), blk(SVACode), blk(NVACode)]).

% TODO: deprecate
:- rkind(deref_sw/3, []).
deref_sw(Reg, Aux, VarCode) => callstmt('DerefSwitch', [Reg, Aux, blk(VarCode)]).
:- rkind(deref_sw0/2, []).
deref_sw0(Reg, VarCode) => callstmt('DerefSwitch0', [Reg, blk(VarCode)]).

unify_heap_atom(U,V) =>
    localv(tagged, T1, V),
    sw_on_heap_var(T1,
      bind(hva, T1, U),
      bind(cva, T1, U),
      if(T1 \== U, jump_fail)).

u_cons0(V,U) =>
    localv(tagged, T1, V),
    sw_on_var(T1,
      bind(hva, T1, U),
      bind(cva, T1, U),
      bind(sva, T1, U),
      if(T1 \== U, jump_fail)).

u_cons0_internal(Var,Atom) =>
    localv(tagged, T1, Var),
    if(T1 /\ tk('TagBitSVA'),
      (bind(sva, T1, Atom)),
      (bind(hva, T1, Atom))).

:- rkind(unify_heap_structure/3, []).
unify_heap_structure(U,V,Cont) =>
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_heap_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, ~tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, ~tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       if(logical_or(not(~tagged_is(str, T1)),
                     ~tagged_to_head_functor(T1) \== U), jump_fail),
       (~s) <- ~tagged_to_arg(T1, 1),
       Cont)),
    '$unreachable'.

:- rkind(unify_structure/3, []).
unify_structure(U,V,Cont) =>
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, ~tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, ~tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(sva, T1, ~tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       if(logical_or(not(~tagged_is(str, T1)),
                     ~tagged_to_head_functor(T1) \== U), jump_fail),
       (~s) <- ~tagged_to_arg(T1, 1),
       Cont)),
    '$unreachable'.

unify_heap_large(P, T) =>
    '{',
    localv(tagged, T1, T),
    sw_on_heap_var(T1,
      bind(hva, T1, cfun_eval('BC_MakeBlob', [P])),
      bind(cva, T1, cfun_eval('BC_MakeBlob', [P])),
      '$fcall'('BC_EqBlob', [T1, P, blk(jump_fail)])),
    '}'.

unify_large(P, T) =>
    '{',
    localv(tagged, T1), T1<-T,
    sw_on_var(T1,
      bind(hva, T1, cfun_eval('BC_MakeBlob', [P])),
      bind(cva, T1, cfun_eval('BC_MakeBlob', [P])),
      bind(sva, T1, cfun_eval('BC_MakeBlob', [P])),
      '$fcall'('BC_EqBlob', [T1, P, blk(jump_fail)])),
    '}'.

:- rkind(unify_heap_list/2, []).
unify_heap_list(V,Cont) =>
    '{',
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_heap_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, ~tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, ~tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       if(not(~is_nonvar_lst(T1)), jump_fail),
       (~s) <- ~tagp_ptr(lst, T1),
       Cont)),
    '}',
    '$unreachable'.

:- rkind(unify_list/2, []).
unify_list(V,Cont) =>
    '{',
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, ~tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, ~tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(sva, T1, ~tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       if(not(~is_nonvar_lst(T1)), jump_fail),
       (~s) <- ~tagp_ptr(lst, T1),
       Cont)),
    '}',
    '$unreachable'.

unify_local_value(T1) =>
    if(~tagged_is(sva, T1),
      (localv(tagged, T0),
       do_while((
           callstmt('RefSVA', [T0,T1]),
           if(T0==T1, (
               cachedreg('H', H),
               bind(sva, T1, ~tagp(hva, H)),
               preload(hva, T1),
               break)),
           T1 <- T0
       ), ~tagged_is(sva, T1)))),
    heap_push(T1).

% ---------------------------------------------------------------------------
%! # Auxiliary macro definitions

% Concurrency: if we cut (therefore discarding intermediate
% choicepoints), make sure we also get rid of the linked chains which
% point to the pending calls to concurrent predicates. (MCL)

% TODO: Bug: the PROFILE__HOOK_CUT should be implemented like show_nodes
%     show_nodes(w->choice, w->previous_choice);

do_cut =>
    profile_hook(cut),
    (~b) <- (~w)^.previous_choice,
    set_choice(~b),
    trace_chpt_cut((~w)^.choice),
    conc_chpt_cleanup(tk('TopConcChpt'), (~w)^.choice).

cunify(U,V) =>
    if(not('$fcall'('CBOOL__SUCCEED', [tk('cunify'),U,V])), jump_fail).

% This must not clobber  t2, X[*].  Build goal from Func(X(0),...X(arity-1))
emul_to_goal(Ret) => % (stores: Ret)
    if((~func)^.arity == 0,
      Ret <- (~func)^.printname,
      (cachedreg('H', H),
       Ret <- ~tagp(str, H),
       heap_push(~set_arity((~func)^.printname,(~func)^.arity)),
       foreach(intmach, range((~func)^.arity), I,
               (localv(tagged, T1, x(I)),
                unify_local_value(T1)))
      )).

deallocate =>
    (~w)^.next_insn <- (~e)^.next_insn,
    (~w)^.frame <- (~e)^.frame.

code_neck =>
    if(not(~is_deep), (
        do_neck,
        % OK even before allocate
        set_e((~w)^.local_top))
    ).

code_neck_proceed =>
    if(not(~is_deep),
      do_neck,
      (~w)^.local_top <- 0),
    set_e((~w)^.frame),
    (~p) <- (~w)^.next_insn,
    profile_hook(neck_proceed),
    jump_ins_dispatch.

% TODO:[oc-merge] CODE_MAYBE_NECK_TRY
do_neck => % (assume !IsDeep())
    (~b) <- (~w)^.choice,
    if(not(~is_shallow_try),
      % retry
      (neck_retry_patch(~b)), % TODO:[oc-merge] this is not in OC
      % try
      ((~b)^.next_alt <- (~w)^.next_alt, %  /* 4 contiguous moves */
       (~b)^.frame <- (~w)^.frame,
       (~b)^.next_insn <- (~w)^.next_insn,
       (~b)^.local_top <- (~w)^.local_top,
       localv(intmach, I, ~choice_arity(~b)),
       foreach(intmach, range(I), K, ((~b)^.x[K] <- (~w)^.x[K])),
       maybe_choice_overflow) % TODO:[oc-merge] check for choice overflow needed here?
    ),
    set_deep.

maybe_choice_overflow =>
    if(~choice_younger(~choice_offset(~b,tk('CHOICEPAD')), (~w)^.trail_top),
      cvoid_call('choice_overflow', [2*tk('CHOICEPAD')*sizeof(tagged),~true])).

% ---------------------------------------------------------------------------
%! # Definition of instruction format types (ftypes)

% TODO: see engine_oc/ftype.pl qs_enc/2, ql_enc/2

% ftype_def(Code, Id, Def)

% f_o opcode
:- ftype_def(f_o, 15, basic(8, 8)).
% f_e frame_size
:- ftype_def(f_e, 8, basic(8, 8)).
% f_f functor
:- ftype_def(f_f, 9, basic(5, 6)).
% f_i count
:- ftype_def(f_i, 10, basic(8, 8)).
% f_l long
:- ftype_def(f_l, 11, basic(2, 6)).
% f_g liveinfo % TODO: be careful! 
:- ftype_def(f_g, 12, str([f_l, f_i])).
% f_p bytecode pointer
:- ftype_def(f_p, 13, basic(3, 3)).
% f_t term
:- ftype_def(f_t, 14, basic(6, 6)).
% f_x x operand
:- ftype_def(f_x, 16, basic(8, 8)).
% f_y y operand
:- ftype_def(f_y, 17, basic(8, 8)).
% f_z y operand, low bit -> unsafe
:- ftype_def(f_z, 18, basic(8, 8)).
% f_C C/native code pointer
:- ftype_def(f_C, 5, basic(9, 6)).
% f_E predicate pointer
:- ftype_def(f_E, 6, basic(7, 6)).
% f_Q pad byte
:- ftype_def(f_Q, 19, basic(8, 8)).
% f_Y ::= <i>{<y>}
:- ftype_def(f_Y, 3, array(f_i, f_y)).
% f_Z ::= <i>{<z>}
:- ftype_def(f_Z, 4, array(f_i, f_z)).
% f_b blob (large number or float) (spec functor and data object)
:- ftype_def(f_b, 7, blob).

% 'Decoding' a bytecode operand as an expression
dec(op(f_x,N),R) => [[ R = '$fcall'('Xb', [N]) ]].
dec(op(f_y,N),R) => [[ R = '$fcall'('Yb', [N]) ]].
dec(op(f_b,N),R) => [[ R = addr(N) ]]. % (a reference to the blob)
dec(op(f_g,N),R) => [[ R = addr(N) ]]. % (a reference to the blob)
dec(op(_,N),R) => [[ R = N ]].

decops(Xs) => [[ format(Fs) ]], decopsf(Fs, Xs).

% TODO: explicit format (avoid it...)
decopsf(Fs, Xs) => decops_(Fs, 0, Xs).

decops_([], _, []) => true.
decops_([f_Q|Fs], Idx, Xs) => decops_(Fs, Idx+fsize(f_Q), Xs).
decops_([F|Fs], Idx, [X|Xs]) => dec(op(F,~bcp(F,Idx)),X), decops_(Fs, Idx+fsize(F), Xs).

% TODO: 'error' rewrite rule for compile-time errors?

% Move the program counter to discard an argument
shiftf(f_i), [[ format([f_Y|Format]) ]] => [[ update(format([f_Yargs|Format])) ]],
   shiftf_(f_i).
shiftf(f_i), [[ format([f_Z|Format]) ]] => [[ update(format([f_Zargs|Format])) ]],
   shiftf_(f_i).
shiftf(f_y), [[ format([f_Yargs|_]) ]] =>
   shiftf_(f_y).
shiftf(f_z), [[ format([f_Zargs|_]) ]] =>
   shiftf_(f_z).
shiftf(_) => shiftf.

shiftf =>
   [[ format([F|Format]) ]],
   [[ update(format(Format)) ]],
   shiftf_(F).

shiftf_nodec => % like shiftf but do not update P (it is going to be rewritten)
   [[ update(format('?')) ]].

shiftf_(FType) => assign((~p) + fsize(FType)).

% (fsize)
fsize(FType) => [[ Id = tk(FType) ]], '$fcall'('Fs',[Id]).

% ---------------------------------------------------------------------------
%! # (bytecode support)

% (sum of fsize)
fsize_sum([]) => 0.
fsize_sum([X]) => fsize(X).
fsize_sum([X|Xs]) => fsize(X), tk('+'), fsize_sum(Xs).

% (sum of fsize with a last f_b parameter)
fsize_sum_b([], _) => 0.
fsize_sum_b([f_b], BSize) => BSize.
fsize_sum_b([X|Xs], BSize) => fsize(X), tk('+'), fsize_sum_b(Xs, BSize).

% Jump to a given instruction keeping the same operand stream
goto_ins(Ins) =>
    [[ get(ins_spec(Ins).op, Op) ]],
    get_op_label(Op, Label),
    ugoto(Label).

% Dispatch (using implicit format)
dispatch => [[ format(Fs) ]], dispatchf(fsize_sum(Fs)).

% Dispatch (with a f_b parameter size)
dispatch_b(BSize) => [[ format(Fs) ]], dispatchf(fsize_sum_b(Fs, BSize)).

% Dispatch (jump to next instruction in the selected read/write
% mode). Skips OpsSize items from the operand stream.
dispatchf(OpsSize) =>
    assign((~p) + OpsSize),
    jump_ins_dispatch.

put_yvoid =>
    localv(tagged, T0, ~bcp(f_y,0)),
    shiftf(f_y),
    dec(op(f_y,T0),Y),
    inits(Y).

heap_push(X) =>
    cachedreg('H', H),
    heap_push0(H, X).

unsafe_var_expr(X) => not(~younger_stack_var(~tagp(sva,~offset(~e,tk('EToY0'))), X)).

ref_stack_unsafe(To,From) =>
    localv(tagged, T0),
    localv(tagged, T1),
    T0 <- From,
    if(~tagged_is(sva,T0),
      do_while((
          callstmt('RefSVA', [T1,T0]),
          if(T1==T0, (
              if(unsafe_var_expr(T0),
                 (load(hva, T0),
                  bind(sva, T1, T0))),
              break)
          ),
          T0 <- T1
      ), ~tagged_is(sva,T0))),
    To <- T0.

ref_heap_next(A) => ref_heap_next0(A, (~s)).

preload(hva, A) =>
    cachedreg('H', H),
    preload_hva(A, H).

init2h(A, B) => blk((
    cachedreg('H', H),
    localv(tagged, T),
    T <- tagp(hva, H),
    B <- T,
    A <- T,
    heap_push0(H,T)
)).

init2s(A, B) => blk((
    localv(tagged, T),
    T <- tagp(sva, addr(B)),
    B <- T,
    A <- T
)).

inits(Y) => load(sva, Y).
    
load(hva, A) =>
    cachedreg('H', H),
    load_hva(A, H).
load(sva, A) => load_sva(A).
load(cva, A) =>
    cachedreg('H', H),
    load_cva(A, H).

trail_if_conditional_sva(U) =>
    if(~cond_stack_var(U), (
        trail_push_check((~w)^.trail_top, ~tagp(sva, addr(U)))
    )).

u_val(X, Y) => cunify(X, Y).

% segfault patch -- jf
% 'U' is a 'Yb(I)' expression.
% TODO:[oc-merge] was get_first_value
u_fval(V, U) =>
    % TODO: assume U is a SVA tagged var
    trail_if_conditional_sva(U),
    U <- V.

un_voidr(X), [[ mode(r) ]] =>
    (~s) <- ~heap_offset((~s), X).
un_voidr(X), [[ mode(w) ]] =>
    localv(intmach, I, cast(ftype_ctype(f_i_signed), X)),
    do_while(constr_hva(tk('H')), (tk('--'),I)).

un_var(X), [[ mode(r) ]] =>
    ref_heap_next(X).
un_var(X), [[ mode(w) ]] =>
    load(hva,X).

un_val(X), [[ mode(r) ]] =>
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    u_val(X, T1),
    '}'.
un_val(X), [[ mode(w) ]] =>
    heap_push(X).

un_fval(Y) =>
    '{',
    localv(tagged, T0),
    un_var(T0),
    u_fval(T0, Y),
    '}'.

un_lval(X), [[ mode(r) ]] =>
    un_val(X).
un_lval(X), [[ mode(w) ]] =>
    '{',
    localv(tagged, T1, X),
    unify_local_value(T1),
    '}'.

alloc => alloc0(~e).
    
% Emit the initialization of Y variables
initfr(Count) =>
    foreach(intmach, revrangeq(Count-sizeof(tagged), tk('EToY0')*sizeof(tagged), sizeof(tagged)), T, (
        dec(op(f_y,T),Y),
        inits(Y)
    )).

% Emit the code to put a Y argument (which may be 'unsafe')
putarg(Zn,Xn) =>
    if(Zn/\1, (
        dec(op(f_y,Zn+1),Y1),
        ref_stack_unsafe(x(Xn), Y1)
    ), (
        dec(op(f_y,Zn),Y2),
        x(Xn) <- Y2)
    ).

% Pre-registered atoms
get_low_atom([]), [[f]]=> tk('atom_nil').

% ---------------------------------------------------------------------------
%! # Definition of the instruction set

inittrue => decops([N]),
    alloc,
    initfr(N),
    goto('firsttrue').

firsttrue_n => decopsf([f_i],[N]),
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I), _I0, put_yvoid),
    %
    goto('firsttrue'),
    label_blk('firsttrue', firsttrue_).

% (ins without opcode)
firsttrue_ => decopsf([f_e],[EnvSize]),
    (~e)^.next_insn <- (~w)^.next_insn,
    (~e)^.frame <- (~w)^.frame,
    (~w)^.frame <- (~e),
    % (~w)^.next_insn <- '$fcall'('PoffR', [2]), % (before)
    (~w)^.next_insn <- ~bc_off((~p), fsize_sum([f_e])),
    (~w)^.local_top <- ~stack_char_offset(~e,EnvSize),
    if(~off_stacktop(~e,tk('Stack_Warn')), set_event),
    dispatchf(fsize_sum([f_e])). % (was f_i before)

initcall => decops([_,N]),
    alloc,
    initfr(N),
    goto_ins(firstcall(s(0),f_E,f_e)).

firstcall_n => decopsf([f_i],[N]), % N>8
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,8), _I0, put_yvoid),
    %
    goto_ins(firstcall(s(8),f_E,f_e)).

firstcall(I), [[ integer(I) ]] => maybe_blk(put_yvoid), [[ I1 is I-1 ]], goto_ins(firstcall(s(I1),f_E,f_e)).
firstcall_0(PredPtr,EnvSize) =>
    (~e)^.next_insn <- (~w)^.next_insn,
    (~e)^.frame <- (~w)^.frame,
    (~w)^.frame <- (~e),
    (~w)^.next_insn <- ~bc_off((~p), fsize_sum([f_E,f_e])),
    (~w)^.local_top <- ~stack_char_offset(~e,EnvSize),
    (~p) <- PredPtr,
    if(~off_stacktop(~e,tk('Stack_Warn')), set_event),
    goto('enter_predicate').

putarg_z_shift(Xn) =>
    localv(tagged, T1, ~bcp(f_z,0)),
    shiftf(f_z),
    putarg(T1,Xn).

call_n => decopsf([f_i],[N]),
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,8), I0, putarg_z_shift(I0-1)),
    %
    goto_ins(call_8).

call_8 => maybe_blk(putarg_z_shift(7)), goto_ins(call_7).
call_7 => maybe_blk(putarg_z_shift(6)), goto_ins(call_6).
call_6 => maybe_blk(putarg_z_shift(5)), goto_ins(call_5).
call_5 => maybe_blk(putarg_z_shift(4)), goto_ins(call_4).
call_4 => maybe_blk(putarg_z_shift(3)), goto_ins(call_3).
call_3 => maybe_blk(putarg_z_shift(2)), goto_ins(call_2).
call_2 => maybe_blk(putarg_z_shift(1)), goto_ins(call_1).
call_1 => maybe_blk(putarg_z_shift(0)), goto_ins(call).
call => decops([Pred,_]),
    (~w)^.next_insn <- ~bc_off((~p), fsize_sum([f_E,f_e])),
    (~p) <- Pred,
    goto('enter_predicate').

lastcall_n => decopsf([f_i],[N]),
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,8), I0, putarg_z_shift(I0-1)),
    %
    goto_ins(lastcall_8).

lastcall_8 => putarg_z_shift(7), goto_ins(lastcall_7).
lastcall_7 => putarg_z_shift(6), goto_ins(lastcall_6).
lastcall_6 => putarg_z_shift(5), goto_ins(lastcall_5).
lastcall_5 => putarg_z_shift(4), goto_ins(lastcall_4).
lastcall_4 => putarg_z_shift(3), goto_ins(lastcall_3).
lastcall_3 => putarg_z_shift(2), goto_ins(lastcall_2).
lastcall_2 => putarg_z_shift(1), goto_ins(lastcall_1).
lastcall_1 => putarg_z_shift(0), goto_ins(lastcall).
lastcall => deallocate, goto_ins(execute).

execute => decops([Pred]),
    setmode(w),
    (~p) <- Pred,
    goto('enter_predicate').

inith(X) => load(hva,X).

put_x_unsafe_value => decops([A,B]),
    localv(tagged, T0), ref_stack_unsafe(T0,B),
    A <- T0,
    B <- T0,
    dispatch.

put_y_unsafe_value => decops([A,B]),
    ref_stack_unsafe(A,B),
    dispatch.

put_constant => decops([A,B]),
    A <- B,
    dispatch.

put_nil => decops([A]),
    A <- ~get_low_atom([]),
    dispatch.

put_large => decops([A,B]),
    [[ mode(M) ]],
    setmode(r),
    A <- cfun_eval('BC_MakeBlob', [B]),
    setmode(M),
    dispatch_b(~large_size(B^)).

put_structure => decops([A,B]),
    cachedreg('H', H),
    A <- ~tagp(str, H),
    heap_push(B),
    dispatch.

put_list => decops([A]),
    cachedreg('H', H),
    A <- ~tagp(lst, H),
    dispatch.

put_yval_yuval => decops([A,B,C,D]),
    A <- B,
    ref_stack_unsafe(C,D),
    dispatch.

put_yuval_yval => decops([A,B,C,D]),
    ref_stack_unsafe(A,B),
    C <- D,
    dispatch.

put_yuval_yuval => decops([A,B,C,D]),
    ref_stack_unsafe(A,B),
    ref_stack_unsafe(C,D),
    dispatch.

get_y_value => decops([A,B]),
    u_val(A,B),
    dispatch.

get_constant => decops([A,B]),
    u_cons0(A,B),
    dispatch.

% (x0 version: read-mode match has been done during indexing)
get_constant_x0, [[ mode(r) ]] => dispatch.
get_constant_x0, [[ mode(w) ]] => decops([A]),
    localv(tagged, T0, x(0)),
    if(~tagged_is(hva,T0),
      bind(hva,T0,A),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,A),
        bind(cva,T0,A))),
    dispatch.

get_large => decops([A,B]),
    unify_large(B,A),
    dispatch_b(~large_size(B^)).

% (x0 version: read-mode match has been done during indexing)
get_large_x0, [[ mode(r) ]] => decops([A]),
    localv(tagged, T0, x(0)),
    unify_large(A,T0),
    dispatch_b(~large_size(A^)).
get_large_x0, [[ mode(w) ]] => decops([A]),
    setmode(r),
    localv(tagged, T1, cfun_eval('BC_MakeBlob', [A])),
    setmode(w),
    localv(tagged, T0, x(0)),
    if(~tagged_is(hva,T0),
      bind(hva,T0,T1),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,T1),
        bind(cva,T0,T1))),
    dispatch_b(~large_size(A^)).

get_structure => decops([A,B]),
    unify_structure(B,A,dispatch).

% (x0 version: read-mode match has been done during indexing)
get_structure_x0, [[ mode(r) ]] =>
    localv(tagged, T0, x(0)),
    (~s) <- ~tagged_to_arg(T0, 1),
    dispatch.
get_structure_x0, [[ mode(w) ]] => decops([A]),
    cachedreg('H', H),
    localv(tagged, T1, ~tagp(str, H)),
    localv(tagged, T0, x(0)),
    if(~tagged_is(hva,T0),
      bind(hva,T0,T1),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,T1),
        bind(cva,T0,T1))),
    heap_push(A),
    dispatch.

get_nil => decops([A]),
    u_cons0(A,~get_low_atom([])),
    dispatch.

% (x0 version: read-mode match has been done during indexing)
get_nil_x0, [[ mode(r) ]] =>
    dispatch.
get_nil_x0, [[ mode(w) ]] =>
    localv(tagged, T0, x(0)),
    if(~tagged_is(hva,T0),
      bind(hva,T0,~get_low_atom([])),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,~get_low_atom([])),
        bind(cva,T0,~get_low_atom([])))),
    dispatch.

get_list => decops([A]),
    unify_list(A, dispatch).

% (x0 version: read-mode match has been done during indexing)
get_list_x0, [[ mode(r) ]] =>
    localv(tagged, T0, x(0)),
    (~s) <- ~tagp_ptr(lst, T0),
    dispatch.
get_list_x0, [[ mode(w) ]] =>
    cachedreg('H', H),
    localv(tagged, T1, ~tagp(lst, H)),
    localv(tagged, T0, x(0)),
    if(~tagged_is(hva,T0),
      bind(hva,T0,T1),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,T1),
        bind(cva,T0,T1))),
    dispatch.

get_constant_neck_proceed => decops([A,B]),
    u_cons0(A,B),
    setmode(w),
    goto_ins(neck_proceed).

get_nil_neck_proceed => decops([A]),
    u_cons0(A,~get_low_atom([])),
    setmode(w),
    goto_ins(neck_proceed).

cutb_x => decops([A]),
    (~w)^.local_top <- 0, % may get hole at top of local stack
    (~w)^.previous_choice <- ~choice_from_tagged(A),
    do_cut,
    dispatch.

cutb_x_neck => decops([A]),
    (~w)^.local_top <- 0, % may get hole at top of local stack
    (~w)^.previous_choice <- ~choice_from_tagged(A),
    shiftf,
    goto_ins(cutb_neck).

cutb_neck =>
    do_cutb_neck,
    dispatch.

cutb_x_neck_proceed => decops([A]),
    (~w)^.previous_choice <- ~choice_from_tagged(A),
    shiftf_nodec,
    % w->local_top <- 0 % done by CODE_PROCEED
    goto_ins(cutb_neck_proceed).

cutb_neck_proceed =>
    do_cutb_neck,
    goto_ins(proceed).

do_cutb_neck =>
    do_cut,
    if(not(~is_deep),
      (set_deep,
       % TODO:[merge-oc] if neck is not pending, then choice overflow has already been checked?
       maybe_choice_overflow)).

cute_x => decops([A]),
    (~w)^.previous_choice <- ~choice_from_tagged(A),
    (~w)^.local_top <- (~e), % w->local_top may be 0 here
    do_cut,
    set_e((~w)^.local_top),
    dispatch.

cute_x_neck => decops([A]),
    (~w)^.previous_choice <- ~choice_from_tagged(A),
    shiftf,
    goto_ins(cute_neck).

cute_neck =>
    (~w)^.local_top <- (~e), %  w->local_top may be 0 here.
    do_cut,
    % w->next_alt can't be NULL here
    set_deep,
    if(~choice_younger(~choice_offset(~b,tk('CHOICEPAD')), (~w)^.trail_top),
      cvoid_call('choice_overflow', [2*tk('CHOICEPAD')*sizeof(tagged),~true])),
    set_e((~w)^.local_top),
    dispatch.

cutf_x => decops([A]),
    (~w)^.previous_choice <- ~choice_from_tagged(A),
    shiftf,
    goto_ins(cutf). % TODO: check that pending 'format' after shift is the expected one

cutf =>
    do_cut,
    set_e((~w)^.frame),
    dispatch.

cut_y => decops([A]),
    localv(tagged, T1), T1 <- A,
    (~w)^.previous_choice <- ~choice_from_tagged(T1),
    do_cut,
    set_e((~w)^.frame),
    dispatch.

getchoice(A) =>
    A <- ~choice_to_tagged((~w)^.previous_choice).

choice_x => decops([X]),
    X <- ~choice_to_tagged((~w)^.previous_choice),
    dispatch.

choice_yf =>
    alloc,
    goto_ins(choice_y).

choice_y => decops([Y]),
    Y <- ~choice_to_tagged((~w)^.previous_choice),
    dispatch.

kontinue =>
    % after wakeup, write mode!
    set_func(~tagged_to_functor(y(0))),
    foreach(intmach, range((~func)^.arity), I, (x(I) <- y(I+1))),
    deallocate,
    goto('enter_predicate').

leave => goto_ins(exit_toplevel).

exit_toplevel =>
    goto('exit_toplevel').

retry_c => decops([A]),
    if(not(~is_deep),
      (neck_retry_patch(~b),
       set_deep)),
    if(not(call_fC(cbool0,A,[])), jump_fail),
    goto_ins(proceed).

move(A, B) => B <- A.
revmove(A, B) => A <- B. % TODO: swap in pl2wam? no need to have 2 versions

branch => decops([Addr]),
    (~p) <- ~bc_off((~p), Addr),
    jump_ins_dispatch.

% Call Expr function returning a tagged, goto fail on ERRORTAG
cfun_semidet(Target, Expr) =>
    localv(tagged, Res, cast(tagged, Expr)),
    Target <- Res,
    if(tk('ERRORTAG')==Res, jump_fail).

cblt_semidet(Expr) => if(not(Expr), jump_fail).

function_1 => decops([A,B,C,Li]),
    (~w)^.liveinfo <- Li,
    cfun_semidet(A, call_fC(ctagged1, C, [B])),
    dispatch.

function_2 => decops([A,B,C,D,Li]),
    (~w)^.liveinfo <- Li,
    cfun_semidet(A, call_fC(ctagged2, D, [B,C])),
    dispatch.

builtin_1 => decops([A,B]),
    cblt_semidet(call_fC(cbool1,B,[A])),
    dispatch.

builtin_2 => decops([A,B,C]),
    cblt_semidet(call_fC(cbool2,C,[A,B])),
    dispatch.

builtin_3 => decops([A,B,C,D]),
    cblt_semidet(call_fC(cbool3,D,[A,B,C])),
    dispatch.

% backtracking into clause/2
retry_instance =>
    % Take into account 'open' predicates.  (MCL)
    % If there is *definitely* no next instance, remove choicepoint
    if(logical_or(paren(logical_and(~tagged_to_root(x(tk('RootArg')))^.behavior_on_failure \== tk('DYNAMIC'),
                              % Wait and removes handle if needed
                              not(cbool_succeed('next_instance_conc', [addr((~w)^.misc^.ins)])))),
                  paren(logical_and(~tagged_to_root(x(tk('RootArg')))^.behavior_on_failure == tk('DYNAMIC'),
                              not(cbool_succeed('next_instance', [addr((~w)^.misc^.ins)]))))), (
        set_deep,
        (~b) <- (~w)^.previous_choice,
        set_choice(~b)
    )),
    if(is_null((~w)^.misc^.ins),
      % A conc. predicate has been closed, or a non-blocking call was made (MCL)
      (trace(retry_instance_debug_1),
       tk('TopConcChpt') <- ~term_to_pointer_or_null(choice, x(tk('PrevDynChpt'))),
       trace(retry_instance_debug_2),
       % But fail anyway
       jump_fail)),
    trace(retry_instance_debug_3),
    (~p) <- cast(bcp, (~w)^.misc^.ins^.emulcode),
    jump_ins_dispatch.

get_constraint => decops([A]),
    localv(tagged, T1, A),
    localv(tagged, T2), load(cva,T2),
    sw_on_var(T1,
      (bind(hva,T1,T2), A <- T2),
      bind(cva,T2,T1),
      (bind(sva,T1,T2), A <- T2),
      bind(cva,T2,T1)),
    dispatch.

unify_void(I), [[ integer(I), I>0 ]] => % (specialized case)
    ( [[ mode(r) ]] ->
        un_voidr(I),
        dispatch
    ; cachedreg('H', H),
      constr_hva(H),
      ( [[ I = 1 ]] ->
          dispatch
      ; [[ I1 is I-1 ]],
        goto_ins(unify_void(I1))
      )
    ).
unify_void(N), [[ mode(r) ]] =>
    un_voidr(N),
    dispatch.
unify_void(N), [[ mode(w) ]] =>
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,4), _I0,
      (cachedreg('H', H),
       constr_hva(H))),
    goto_ins(unify_void(4)).

unify_constant, [[ mode(r) ]] => decops([A]),
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_atom(A,T1),
    dispatch.
unify_constant, [[ mode(w) ]] => decops([A]),
    heap_push(A),
    dispatch.

unify_large, [[ mode(r) ]] => decops([A]),
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_large(A,T1),
    dispatch_b(~large_size(A^)).
unify_large, [[ mode(w) ]] => decops([A]),
    % TODO: try to switch to r mode properly (this code is tricky)
    % (this is 'heap_push and switch to read')
    cachedreg('H', H),
    (~w)^.heap_top <- ~heap_offset(H,1),
    H^ <- cfun_eval('BC_MakeBlob', [A]),
    [[ update(mode(r)) ]],
    dispatch_b(~large_size(A^)).

unify_structure, [[ mode(r) ]] => decops([A]),
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_structure(A,T1,dispatch).
unify_structure, [[ mode(w) ]] => decops([A]),
    cachedreg('H', H),
    heap_push(~tagp(str,~heap_offset(H,1))),
    heap_push(A),
    dispatch.

unify_nil, [[ mode(r) ]] =>
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_atom(~get_low_atom([]), T1),
    dispatch.
unify_nil, [[ mode(w) ]] =>
    heap_push(~get_low_atom([])),
    dispatch.

unify_list, [[ mode(r) ]] =>
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_list(T1,dispatch).
unify_list, [[ mode(w) ]] =>
    cachedreg('H', H),
    heap_push(~tagp(lst,~heap_offset(H,1))),
    dispatch.

unify_constant_neck_proceed, [[ mode(r) ]] => decops([A]),
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_atom(A,T1),
    setmode(w),
    goto_ins(neck_proceed).
unify_constant_neck_proceed, [[ mode(w) ]] => decops([A]),
    heap_push(A),
    goto_ins(neck_proceed).

unify_nil_neck_proceed, [[ mode(r) ]] =>
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_atom(~get_low_atom([]), T1),
    setmode(w),
    goto_ins(neck_proceed).
unify_nil_neck_proceed, [[ mode(w) ]] =>
    heap_push(~get_low_atom([])),
    goto_ins(neck_proceed).

bump_counter(A) => gauge_incr_counter(A).

counted_neck => decops([A,B]),
    cpp_if_defined('GAUGE'),
    if(not(~is_deep), (
      (~b) <- (~w)^.choice,
      if(not(~is_shallow_try), (
        gauge_incr_counter(A) % retry counter
      ),(
        gauge_incr_counter(B) % try counter
      ))
    )),
    cpp_endif,
    assign((~p) + fsize_sum([f_l,f_l])),
    goto_ins(neck).

fail =>
    jump_fail.

% TODO: PATCH_LIVEINFO requires f_g (we cannot expand to f_l,f_i in instruction format like in OC)

heapmargin_call => decopsf([f_l,f_i],[A,B]), % TODO: abstract code to use f_g
    cachedreg('H',H),
    if(~heap_char_difference(H, tk('Heap_End')) < cast(intmach,A),
      ([[ mode(M) ]],
       setmode(r),
       cvoid_call('explicit_heap_overflow', [cast(intmach,A)*2, cast(ftype_ctype(f_i_signed),B)]),
       setmode(M)
       )),
    dispatch.

neck =>
    code_neck,
    dispatch.

dynamic_neck_proceed => % (needs: w->misc->ins)
    u_cons0_internal(x(3),~pointer_to_term((~w)^.misc^.ins)),
    if(~is_deep, goto_ins(proceed)),
    (~b) <- (~w)^.choice,
    % (assume w->next_alt != NULL)
    if(~is_shallow_try, (
        tk('def_clock') <- tk('use_clock')+1,
        if(tk('def_clock') == 0xffff,(
            setmode(r),
            cvoid_call('clock_overflow', []),
            setmode(w)
        ))
    )),
    goto_ins(neck_proceed).

neck_proceed =>
    code_neck_proceed.

proceed =>
    (~w)^.local_top <- 0,
    set_e((~w)^.frame),
    (~p) <- (~w)^.next_insn,
    profile_hook(proceed),
    dispatch.

% TODO: this a new instruction really needed here? consider special builtin functions
restart_point =>
    setmode_setH(r, ~tagged_to_pointer((~w)^.choice^.x[0])),
    setmode(w),
    (~p) <- cast(bcp, ~tagged_to_pointer((~w)^.choice^.x[0])^),
    (~w)^.next_insn <- (~w)^.choice^.next_insn,
    cvoid_call('pop_choicept', []),
    goto('enter_predicate').

% ---------------------------------------------------------------------------
%! # WAM execution tracing

:- rkind(pred_trace/1, []).
pred_trace(Kind) => pred_trace0(Kind, ~func).

:- rkind(trace/1, []).
trace(X) => callstmt('ON_DEBUG', [blk(trace_(X))]).

trace_print(Args) => callstmt('fprintf', [tk('stderr')|Args]).

:- rkind(trace_/1, []).
trace_(wam_loop_begin) =>
    if(tk('debug_threads'),
       trace_print([tk_string("Worker state address is %p\n"), tk('desc')])).
trace_(wam_loop_exit) =>
    % trace_print([tk_string("Goal %p returning!\n"), tk('desc')]),
    true.
trace_(create_choicepoint) =>
    if(tk('debug_choicepoints'),
       trace_print([tk_string("WAM created choicepoint (r), node = %p\n"), (~w)^.choice])).
trace_(failing_choicepoint) =>
    if(tk('debug_choicepoints'),
       trace_print([tk_string("Failing: node = %p, previous_choice = %p, conc. node = %p\n"), (~w)^.choice, (~w)^.previous_choice, tk('TopConcChpt')])),
    if(logical_and((~w)^.misc^.top_conc_chpt < (~w)^.choice,
                   (~w)^.misc^.top_conc_chpt < (~w)^.previous_choice), 
       trace_print([tk_string("********** what happened here?\n")])).
trace_(deep_backtracking) =>
    if(tk('debug_choicepoints'),
       trace_print([tk_string("deep backtracking, node = %p\n"), (~w)^.choice])).
trace_(restore_xregs_choicepoint(I)) =>
    if(tk('debug_choicepoints'),
       trace_print([tk_string("Reloading %d words from node %p\n"), I, (~w)^.choice])).
trace_(worker_expansion_blt) =>
    trace_print([tk_string("wam() detected worker expanded by C predicate\n")]).
trace_(worker_expansion_cterm) =>
    trace_print([tk_string("Reallocation of wrb detected in wam()\n")]).
trace_(retry_instance_debug_1) =>
    % Extended check
    if(tk('debug_concchoicepoints'),
      if(logical_and(~tagged_to_root(x(tk('RootArg')))^.behavior_on_failure \== tk('CONC_CLOSED'),
                     ~is_blocking(x(tk('InvocationAttr')))),
         trace_print([tk_string("**wam(): failing on a concurrent closed pred, chpt=%p, failing chpt=%p .\n"),
                      (~w)^.choice,
                      tk('TopConcChpt')]))),
    if(tk('debug_conc'),
      if(logical_or(~tagged_to_root(x(tk('RootArg')))^.x2_pending_on_instance,
                    ~tagged_to_root(x(tk('RootArg')))^.x5_pending_on_instance),
         trace_print([tk_string("**wam(): failing with invokations pending from root, type = %d.\n"),
                      ~tagged_to_root(x(tk('RootArg')))^.behavior_on_failure]))).
trace_(retry_instance_debug_2) =>
    if(tk('debug_concchoicepoints'),
       trace_print([tk_string("New topmost concurrent chpt = %x\n"), tk('TopConcChpt')])).
trace_(retry_instance_debug_3) =>
    if(logical_and(tk('debug_conc'),
                   ~tagged_to_root(x(tk('RootArg')))^.behavior_on_failure \== tk('DYNAMIC')),
       trace_print([(tk_string("*** "), ' ', tk('PRIdm'), ' ', tk_string("backtracking on a concurrent predicate.\n")),
                    cast(intmach,tk('Thread_Id')),
                    cast(intmach,tk('GET_INC_COUNTER'))])),
    if(logical_and(tk('debug_concchoicepoints'),
                   ~tagged_to_root(x(tk('RootArg')))^.behavior_on_failure \== tk('DYNAMIC')),
       trace_print([tk_string("backtracking to chpt. = %p\n"), (~w)^.choice])).

% ---------------------------------------------------------------------------
%! # WAM profiling

:- rkind(profile_hook/1, []).
profile_hook(cut) => call0('PROFILE__HOOK_CUT').
profile_hook(proceed) => call0('PROFILE__HOOK_PROCEED').
profile_hook(neck_proceed) => call0('PROFILE__HOOK_NECK_PROCEED').
profile_hook(fail) => call0('PROFILE__HOOK_FAIL').
profile_hook(redo) => call0('PROFILE__HOOK_REDO').

% ---------------------------------------------------------------------------
%! # Gauge (profiling counters)

gauge_incr_counter(Counter) =>
    cpp_if_defined('GAUGE'),
    incr_counter(Counter),
    cpp_endif.

% ---------------------------------------------------------------------------
%! # Entries to generate other engine support files

% IDs for exported instructions (names visible from C code)
:- rkind(all_ins_op/0, []).
all_ins_op =>
    autogen_warning_comment,
    %
    [[ collect_and_filter(instruction_set, '$exported_ins'/2, Insns) ]],
    '$foreach'(Insns, ins_op).

ins_op('$exported_ins'(Op, Name)), [[ get(op(Op).optional, Flag) ]] =>
    cpp_if_defined(Flag),
    ins_op_(Op, Name),
    cpp_endif.
ins_op('$exported_ins'(Op, Name)) =>
    ins_op_(Op, Name).

ins_op_(Opcode, Name) =>
    [[ uppercase(Name, NameUp) ]],
    cpp_define(NameUp, Opcode).

% TODO: refactor
% Engine info (for inclusion in Makefile)
:- rkind(eng_info_mk/0, []).
eng_info_mk =>
    [[ findall(F, use_native(F, c), Cs) ]],
    [[ findall(F, use_native(F, h), Hs) ]],
    [[ findall(F, use_native(F, h_noalias), HsNoAlias) ]],
    [[ engine_stubmain(StubMain) ]],
    makefile_def('ENG_STUBMAIN', [StubMain]),
    makefile_def('ENG_CFILES', Cs),
    makefile_def('ENG_HFILES', Hs),
    makefile_def('ENG_HFILES_NOALIAS', HsNoAlias).

:- rkind(makefile_def/2, []).
makefile_def(X, Fs) =>
    tk(X), ' ', tk('='), ' ', 
    '$foreach_sep'(' ', Fs, fmt_atom),
    tk_nl.

% Engine info (for inclusion in sh scripts)
:- rkind(eng_info_sh/0, []).
eng_info_sh =>
    [[ findall(F, use_native(F, c), Cs) ]],
    [[ findall(F, use_native(F, h), Hs) ]],
    [[ findall(F, use_native(F, h_noalias), HsNoAlias) ]],
    [[ engine_stubmain(StubMain) ]],
    sh_def('ENG_STUBMAIN', [StubMain]),
    sh_def('ENG_CFILES', Cs),
    sh_def('ENG_HFILES', Hs),
    sh_def('ENG_HFILES_NOALIAS', HsNoAlias).

:- rkind(sh_def/2, []).
sh_def(X, Fs) =>
    tk(X), tk('='), tk('"'), 
    '$foreach_sep'(' ', Fs, fmt_atom),
    tk('"'),
    tk_nl.

:- rkind(fmt_atom/1, []).
fmt_atom(X) => tk(X).     

% Meta-information
:- rkind(absmachdef/0, []).
absmachdef =>
    autogen_warning_comment,
    %
    [[ get(max_op, MaxOp) ]],
    [[ NumOp is MaxOp + 1 ]],
    cpp_define('INS_OPCOUNT', NumOp),
    cpp_define('Fs(Ty)', '$fcall'('FTYPE_size', [tk('Ty')])), % (shorter name) % TODO: duplicated
    %
    [[ ftype_def(f_i, FId_i, _) ]],
    [[ ftype_def(f_o, FId_o, _) ]],
    tk('absmachdef_t'), ' ', tk('abscurr'), tk('='), ' ', '{', tk_nl,
    field('ftype_id_i', FId_i), tk(','), tk_nl,
    field('ftype_id_o', FId_o), tk(','), tk_nl,
    field('ins_info', absmach_insinfo), tk(','), tk_nl,
    field('ins_n', NumOp), tk(','), tk_nl,
    ftype_info_list,
    field('q_pad1', 128*4), tk(','), tk_nl,
    field('q_pad2', 1152*4), tk(','), tk_nl,
    field('tagged_size', tk('sizeof(tagged_t)')), tk(','), tk_nl,
    field('size_align', tk('sizeof(tagged_t)')), tk_nl,
    '}', stmtend,
    %
    insnames.

absmach_insinfo =>
    [[ get(max_op, MaxOp) ]],
    [[ range(0, MaxOp, Ops) ]],
    tk('(ftype_base_t *[])'), blk('$foreach_sep'(',\n', Ops, absmach_insinfo_)).

absmach_insinfo_(Op) =>
    ( [[ get(op(Op).format, Format) ]] ->
        ftype_info__str(Format)
    ; ftype_info__str([])
    ).

ftype_info_list =>
    [[ max_ftype(MaxFType) ]],
    [[ NumFType is MaxFType + 1 ]],
    [[ range(0, MaxFType, FTypes) ]],
    field('ftype_info', ftype_info_list_(FTypes)), tk(','), tk_nl,
    field('ftype_n', NumFType), tk(','), tk_nl.

ftype_info_list_(FTypes) =>
    tk('(ftype_base_t *[])'), blk('$foreach_sep'(',\n', FTypes, ftype_info_)).

ftype_info_(Id), [[ id_ftype(Id, FType) ]] =>
    [[ ftype_def(FType, _, Def) ]],
    ftype_info__(Def, FType).
ftype_info_(_) => ftype_info__(str([]), none).

ftype_info__(array(A,B), _FType) =>
    [[ map_ftype_id([A,B], Ys) ]],
    '$fcall'('FTYPE_ARRAY', Ys).
ftype_info__(str(Xs), _FType) => ftype_info__str(Xs).
ftype_info__(basic(SMethod,LMethod), FType) => '$fcall'('FTYPE_BASIC', [fsize(FType),SMethod,LMethod]).
ftype_info__(blob, _) => '$fcall'('FTYPE_BLOB', []).

ftype_info__str([]) => '$fcall'('FTYPE_STR0', []).
ftype_info__str(Xs) =>
    [[ length(Xs, N) ]],
    [[ map_ftype_id(Xs, Ys) ]],
    '$fcall'('FTYPE_STR', [N, '$fcall'('BRACES', Ys)]).

:- rkind(ftype_id/1, []).
ftype_id(FType) =>
    [[ ftype_def(FType, Id, _) ]],
    tk_number(Id).

:- rkind(insnames/0, []).
insnames =>
    [[ get(max_op, MaxOp) ]],
    [[ NumOp is MaxOp + 1 ]],
    [[ range(0, MaxOp, Ops) ]],
    tk('char *ins_name['), NumOp, tk('] = '), blk('$foreach_sep'(',\n', Ops, op_insname)),
    stmtend.

:- rkind(op_insname/1, []).
op_insname(Op) =>
    ( [[ get(op(Op).ins_spec, InsSpec) ]] ->
        [[ format_to_string("~w", [InsSpec], Str) ]],
        tk_string(Str)
        %tk('"'), tk(InsSpec), tk('"')
    ; tk_string("(none)")
    ).

:- rkind(autogen_warning_comment/0, []).
autogen_warning_comment =>
    tk('/***************************************************************************/'), tk_nl,
    tk('/*                             WARNING!!!                                  */'), tk_nl,
    tk('/*                      D O   N O T   M O D I F Y                          */'), tk_nl,
    tk('/*                This file is autogenerated by emugen                     */'), tk_nl,
    tk('/***************************************************************************/'), tk_nl,
    tk_nl.

% ---------------------------------------------------------------------------
%! # The WAM loop function

% KERNEL OF EMULATOR

% If the wam() local variables are changed, those on wam_private_t
% should be changed as well to reflect the current state! They should
% as well be saved and recovered in SAVE_WAM_STATE and
% RECOVER_WAM_STATE

/* --------------------------------------------------------------------------- */

:- rkind(op_macros/0, []).
op_macros =>
    cpp_define('BcOPCODE', ~bc_fetch_opcode),
    % address for a bytecode operand
    cpp_define('BcP(Ty,X)', paren(cast(ptr(ftype_ctype(tk('Ty'))), ~bc_off(tk('P'), paren(tk('X'))))^)),
    cpp_define('Fs(Ty)', '$fcall'('FTYPE_size', [tk('Ty')])). % (shorter name)

:- rkind(wam_loop_defs/0, []).
wam_loop_defs =>
    autogen_warning_comment,
    % TODO: move somewhere else
    vardecl(extern(instance_clock), tk('def_clock')),
    vardecl(extern(instance_clock), tk('use_clock')),
    %
    op_macros,
    wam__2_proto,
    wam_def,
    wam__2_def.

:- rkind(wam_def/0, []).
wam_def =>
    tk('CVOID__PROTO'), '(',
    tk('wam'), tk(','),
    argdecl(ptr(goal_descriptor), tk('desc')),
    ')', ' ',
    '{', tk_nl,
    % We separate the catch block from wam__2 to make sure that
    % the implicit setjmp in EXCEPTION__CATCH do not affect
    % negatively to the optimizations in the main engine loop.
    vardecl(ptr(definition), tk('func'), cast(ptr(definition), ~null)),
    goto('again'),
    label_blk('again', wam_def_again),
    '}', tk_nl.

exception_catch(Try, Catch) =>
    tk('EXCEPTION__CATCH'), '(',
    Try, tk(','), ' ',
    Catch,
    tk(')'), stmtend.

wam_def_again =>
    exception_catch(blk((
      tk('CVOID__CALL(wam__2, desc, func)'), stmtend,
      return
    )), blk((
      vardecl(ptr(choice), tk('b')),
      %% vardecl(ptr(frame), tk('e')),
      % (like code_neck but do not set frame)
      if(not(~is_deep), do_neck), % Force neck if not done
      %% code_neck, % Force neck if not done
      x(0) <- ~make_small(tk('ErrCode')), % Error code
      x(1) <- ~get_atom(tk('ErrFuncName')), % Builtin name
      x(2) <- ~make_small(tk('ErrFuncArity')), % Builtin arity
      x(4) <- tk('Culprit'), % Culprit arg.
      x(3) <- ~make_small(tk('ErrArgNo')), % w. number
      tk('func') <- tk('address_error'),
      goto('again')
    ))).

wam__2_proto =>
    tk('CVOID__PROTO'), '(',
    tk('wam__2'), tk(','),
    argdecl(ptr(goal_descriptor), tk('desc')), tk(','),
    argdecl(ptr(definition), tk('start_func')),
    ')', stmtend.

wam__2_def =>
    tk('CVOID__PROTO'), '(',
    tk('wam__2'), tk(','),
    argdecl(ptr(goal_descriptor), tk('desc')), tk(','),
    argdecl(ptr(definition), tk('start_func')),
    ')', ' ',
    blk(wam_loop).

wam_loop =>
    wam_loop_decls,
    code_loop_begin,
    % MISCELLANEOUS SUPPORT
    %
    label_blk('escape_to_p2', escape_to_p2),
    %
    label_blk('escape_to_p', escape_to_p),
    %
    % ENTERING A PREDICATE:  H always live.
    % Take into account attributed variables !!
    label_blk('enter_predicate', code_enter_pred),
    %
    label_blk('switch_on_pred', switch_on_pred),
    %
    label_blk('switch_on_pred_sub', code_switch_on_pred_sub),
    %
    % FAILING
    label_blk('fail', code_fail),
    %
    alt_ins_dispatcher,
    %
    label_blk('exit_toplevel', code_exit_toplevel),
    %
    label_blk('illop', code_illop).

% Begin emulation in WAM loop
code_loop_begin =>
    [[ update(mode(r)) ]],
    trace(wam_loop_begin),
    [[ mode(M) ]],
    if(not_null(tk('start_func')), (
      % Directly execute a predicate (used to call from an exception 
      % throwed from C)
      (~p) <- cast(bcp, tk('start_func')),
      (~b) <- (~w)^.choice,
      % TODO: this should not be necessary, right?
      % get_frame_top((~w)^.local_top,(~b),(~g)^.frame),
      setmode(w), % switch_on_pred expects we are in write mode, load H
      goto('switch_on_pred')
    )),
    [[ update(mode(M)) ]],
    %
    if(logical_and(not_null(tk('desc')), tk('desc')^.action /\ tk('BACKTRACKING')), (
      call0('RECOVER_WAM_STATE'),
      jump_fail % Probably...
    )),
    goto_ins(proceed).

escape_to_p2 => % (needs: ptemp)
    [[ update(mode(w)) ]],
    localv(tagged, T2),
    localv(tagged, T3),
    T2 <- ~pointer_to_term((~func)^.code.intinfo),
    emul_to_goal(T3), % (stores: T3)
    (~p) <- tk('ptemp'),
    x(0) <- T3,
    x(1) <- T2,
    goto('switch_on_pred').

escape_to_p => % (needs: ptemp)
    [[ update(mode(w)) ]],
    localv(tagged, T3),
    emul_to_goal(T3), % (stores: T3)
    (~p) <- tk('ptemp'),
    x(0) <- T3,
    goto('switch_on_pred').

code_undo(T0) =>
    [[ update(mode(r)) ]],
    (~w)^.frame <- (~b)^.frame,
    (~w)^.next_insn <- (~b)^.next_insn,
    set_e(~node_local_top(~b)),
    (~e)^.frame <- (~w)^.frame,
    (~e)^.next_insn <- (~w)^.next_insn,
    (~w)^.frame <- (~e),
    (~w)^.next_insn <- tk('failcode'),
    (~w)^.local_top <- cast(ptr(frame), ~offset(~e,tk('EToY0'))),
    setmode(w),
    x(0) <- T0,
    do_builtin_call(syscall, T0).

jump_fail => goto('fail').
code_fail => altcont0.

% Restore state and jump to next alternative instructions
% TODO:[oc-merge] altcont/1
altcont0 =>
    [[ update(mode(r)) ]],
    % The profiling code must be here
    profile_hook(fail),
    % (w->choice->next_alt!=NULL);
    trace(failing_choicepoint),
    reset_wake_count,
    (~b) <- (~w)^.choice,
    %
    untrail.

% (continues at backtrack_ or undo_goal)
untrail =>
    callstmt('ON_TABLING', [blk(tk('MAKE_TRAIL_CACTUS_STACK;'))]),
    %
    '{',
    localv(tagged, T0),
    localv(tagged, T1),
    localv(ptr(tagged), Pt2),
    Pt2 <- (~w)^.trail_top,
    T1 <- cast(tagged, ~trail_top_unmark((~b)^.trail_top)),
    if(~trail_younger(Pt2,T1), (
        do_while((
            [[ mode(M) ]],
            callstmt('PlainUntrail', [Pt2, T0, blk((
                (~w)^.trail_top <- Pt2,
                code_undo(T0)
            ))]),
            [[ update(mode(M)) ]]
        ), ~trail_younger(Pt2,T1)),
        (~w)^.trail_top <- Pt2)
    ),
    '}',
    %
    backtrack_.

backtrack_ =>
    (~w)^.heap_top <- ~node_global_top(~b),
    code_restore_args,
    profile_hook(redo),
    (~p) <- cast(bcp, (~w)^.next_alt),
    localv(ptr(try_node), Alt),
    Alt <- cast(ptr(try_node), (~p))^.next,
    [[ mode(M) ]],
    if(is_null(Alt), ( % TODO: This one is not a deep check! (see line above)
      % TODO:[oc-merge] 'altmode.jump_fail_cont'(_,no_alt)
      [[ update(mode(M)) ]],
      jump_fail_cont(no_alt, Alt)
    ), (
      [[ update(mode(M)) ]],
      jump_fail_cont(next_alt, Alt)
    )), % TODO:[oc-merge] choice_patch also modified w->choice->next_alt in OPTIM_COMP
    '$unreachable'.

jump_fail_cont(no_alt, _Alt) =>
    set_deep,
    (~b) <- (~w)^.previous_choice,
    set_choice(~b),
    callstmt('ON_TABLING', [blk((
        % To avoid sharing wrong trail - it might be associated to the
        % previous frozen choice point
        if(~frozen_chpt(~b),
           push_choicept(~w,tk('address_nd_fake_choicept')))
    ))]),
    jump_alt_code((~p)).
jump_fail_cont(next_alt, Alt) =>
    % TODO:[oc-merge] 'altmode.jump_fail_cont'(_,next_alt)
    choice_patch((~w)^.choice, Alt),
    jump_alt_code((~p)).

jump_alt_code(Alt) =>
    (~p) <- cast(ptr(try_node), Alt)^.emul_p,
    if(not(~is_var(x(0))), jump_ins_dispatch),
    setmode(w),
    jump_ins_dispatch.

code_restore_args =>
    if(~is_deep, deep_backtrack).

% TODO:[oc-merge] part of code_restore_args0
deep_backtrack =>
    % deep backtracking
    trace(deep_backtracking),
    (~w)^.frame <- (~b)^.frame,
    (~w)^.next_insn <- (~b)^.next_insn,
    (~w)^.next_alt <- (~b)^.next_alt,
    (~w)^.local_top <- ~node_local_top(~b),
    % TODO: use this syntax? I::intmach <- B^.next_alt^.arity,
    localv(intmach, I, (~b)^.next_alt^.arity),
    (~w)^.previous_choice <- ~choice_cont0((~b),I),
    % TODO:[oc-merge] set_shallow_retry here?
    set_shallow_retry,
    foreach(intmach, range(I), K, ((~w)^.x[K] <- (~b)^.x[K])).

code_enter_pred =>
    [[ update(mode(w)) ]],
    callstmt('ON_ANDPARALLEL', [blk((
      if(tk('Suspend') == tk('TOSUSPEND'), (
          tk('Suspend') <- tk('SUSPENDED'),
          wait_acquire_lock(tk('Waiting_For_Work_Lock')),
          cond_var_wait(tk('Waiting_For_Work_Cond_Var'),tk('Waiting_For_Work_Lock')),
          tk('Suspend') <- tk('RELEASED'),
          release_lock(tk('Waiting_For_Work_Lock'))))
    % if (Cancel_Goal_Exec && Safe_To_Cancel) {
    %   Cancel_Goal_Exec = FALSE;
    %   Safe_To_Cancel = FALSE;
    %   SetChoice(w->choice);
    %   goto fail;
    % }
    % 
    % if (Cancel_Goal_Exec_Handler != NULL && Safe_To_Cancel) {
    %   Cancel_Goal_Exec_Handler = NULL;
    %   Safe_To_Cancel = FALSE;
    %   // Metacut
    %   w->choice = Current_Init_ChP;
    %   w->trail_top = Current_Trail_Top;
    %   SetChoice(w->choice);
    %   goto fail;
    % }
    ))]),
    % #if defined(PARBACK)
    %   if (Suspend == CHECK_SUSP) {
    %     //Save argument registers
    %     tagged_t *Htmp = H = w->heap_top;
    %     if (HeapCharDifference(w->heap_top,Heap_End) < CONTPAD + (1 + Func->arity)*sizeof(tagged_t))
    %       explicit_heap_overflow(w, (CONTPAD + (1 + Func->arity)*sizeof(tagged_t))*2, 0);
    %     HeapPush(H,(tagged_t)P);
    %     int i;
    %     for (i = 0; i < Func->arity; i++) HeapPush(H,X(i));
    %     w->heap_top = H;
    %     push_choicept(Arg,address_nd_suspension_point);
    %     w->choice->x[0] = Tagp(HVA, Htmp);
    %     //w->choice->next_insn = w->misc->backInsn;
    % 
    %     //No nore suspensions
    %     Suspend = RELEASED;
    %     //Jump to continuation of the current goal
    %     w->next_insn = w->misc->contFrame->next_insn;
    %     w->frame = w->misc->contFrame->frame;
    %     P = w->next_insn;
    %     //SetE(w->frame);
    %     //Execute next_insn
    %     DISPATCH_R(0);
    %   }
    % #endif
    if(~test_event_or_heap_warn_overflow(tk('H')), (
      [[ WakeCount = tk('wake_cnt') ]],
      vardecl(int, WakeCount),
      %
      if(cbool_succeed('Stop_This_Goal',[]), goto('exit_toplevel')),
      %
      WakeCount <- ~get_wake_count,
      %
      if(~heap_char_available(tk('H')) =< tk('CALLPAD')+4*WakeCount*sizeof(tagged), % TODO: It was OffHeaptop(H+4*wake_count,Heap_Warn), equivalent to '<='; but '<' should work?! (also in TestEventOrHeapWarnOverflow?)
        (setup_pending_call(~e, tk('address_true')),
         setmode(r),
         cvoid_call('heap_overflow', [2*(tk('CALLPAD')+4*WakeCount*sizeof(tagged))]),
         setmode(w))),
      if(WakeCount > 0,
        if(WakeCount==1, (
          setup_pending_call(~e, tk('address_uvc')),
          cvoid_call('collect_one_pending_unification', []), % does not touch H
          localv(tagged, T0),
          deref(T0,x(1)),
          if(~tagged_is(cva,T0),
            (% X(1)=*TaggedToGoal(t0);
             x(1) <- T0,
             % patch prev. SETUP_PENDING_CALL
             set_func(tk('address_ucc'))))),
          % wake_count > 1
          ([[ update(mode(w)) ]],
           setup_pending_call(~e, tk('address_pending_unifications')),
           setmode(r),
           cvoid_call('collect_pending_unifications', [WakeCount]),
           setmode(w)))),
      if(~off_stacktop((~w)^.frame,tk('Stack_Warn')), (
         setup_pending_call(~e, tk('address_true')),
         cvoid_call('stack_overflow', []))),
      unset_event,
      if(~test_cint_event, (
         setup_pending_call(~e, tk('address_help')),
         cvoid_call('control_c_normal', []))))),
    goto('switch_on_pred').

switch_on_pred =>
    jump_switch_on_pred_sub((~func)^.enter_instr).

pred_enter_undefined =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("U")),
    tk('ptemp') <- cast(bcp,tk('address_undefined_goal')), % (arity 1)
    goto('escape_to_p').

pred_enter_interpreted =>
    [[ update(mode(w)) ]],
    % pred_trace(tk_string("I")),
    tk('ptemp') <- cast(bcp,tk('address_interpret_c_goal')), % (arity 2)
    goto('escape_to_p2').

pred_enter_c =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("C")),
    setmode(r),
    % Changed by DCG to handle errors in Prolog
    localv(intmach, I, call_fC(cbool0,(~func)^.code.proc,[])),
    [[ ExpW = tk('Expanded_Worker') ]],
    if(not_null(ExpW),
      (trace(worker_expansion_blt),
      if(is_null(tk('desc')),
        (% JFKK this is temp sometimes wam is called without gd
         trace_print([tk_string("bug: invalid WAM expansion\n")]),
         abort)),
      (~w) <- ExpW,
      tk('desc')^.worker_registers <- (~w),
      ExpW <- ~null)),
    if(I,goto_ins(proceed),jump_fail).

pred_enter_builtin_true =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    goto_ins(proceed).

pred_enter_builtin_fail =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    jump_fail.

pred_enter_builtin_current_instance =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    setmode(r),
    (~w)^.misc^.ins <- '$fcall'('CFUN__EVAL', [tk('current_instance0')]),
    if(is_null((~w)^.misc^.ins), jump_fail),
    (~p) <- cast(bcp, (~w)^.misc^.ins^.emulcode),
    jump_ins_dispatch.

pred_enter_builtin_compile_term =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    setmode(r),
    '{',
    localv(ptr(worker), NewWorker), % Temp - for changes in regbanksize
    if(not(cbool_succeed('compile_term', [addr(NewWorker)])),jump_fail),
    if(not_null(NewWorker), % TODO: use Expanded_Worker?
      (if(is_null(tk('desc')),
        (% JFKK this is temp sometimes wam is called without gd
         trace_print([tk_string("bug: invalid WAM expansion\n")]),
         abort)),
      (~w) <- NewWorker,
      tk('desc')^.worker_registers <- (~w),
      trace(worker_expansion_cterm))),
    '}',
    goto_ins(proceed).

pred_enter_builtin_instance =>
    [[ update(mode(w)) ]],
    % ASSERT: X(2) is a dereferenced integer
    pred_trace(tk_string("B")),
    load(hva, x(3)),
    (~w)^.misc^.ins <- ~tagged_to_instance(x(2)),
    (~p) <- cast(bcp, (~w)^.misc^.ins^.emulcode),
    jump_ins_dispatch.

pred_enter_builtin_geler =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    localv(tagged, T1, x(0)),
    deref_sw0(T1,';'),
    localv(tagged, T3),
    T3 <- x(1),
    deref_sw0(T3,';'),
    set_func(~find_definition(tk('predicates_location'),T3,addr((~w)^.structure),~true)),
    % suspend the goal  t3  on  t1.  Func, must be live.
    [[ mode(M) ]],
    setmode(r),
    callstmt('CVOID__CALL', [tk('SUSPEND_T3_ON_T1'), (~func), T3, T1]),
    setmode(M),
    goto_ins(proceed).

% Like pred_enter_builtin_syscall/0, but fails on undefined
pred_enter_builtin_nodebugcall =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    localv(tagged, T0, x(0)),
    deref_sw(T0,x(0),';'),
    do_builtin_call(nodebugcall, T0).

% Like pred_enter_builtin_call/0, but ignores Current_Debugger_Mode
pred_enter_builtin_syscall =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    localv(tagged, T0, x(0)), 
    deref_sw(T0,x(0),';'),
    do_builtin_call(syscall, T0).

pred_enter_builtin_call =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    localv(tagged, T0, x(0)),
    deref_sw(T0,x(0),';'),
    do_builtin_call(call, T0).

do_builtin_call(CallMode, T0) =>
    set_func(~find_definition(tk('predicates_location'),T0,addr((~w)^.structure),~false)),
    % Undefined?
    ( [[ CallMode = nodebugcall ]] ->
        if(is_null((~func)),jump_fail)
    ; ( [[ CallMode = syscall ]] -> true
      ; [[ CallMode = call ]] -> true
      ; [[ fail ]]
      ),
      if(is_null((~func)),
        (set_func(tk('address_undefined_goal')),
         goto('switch_on_pred')))
    ),
    % Debug hook?
    ( [[ CallMode = nodebugcall ]] -> true
    ; [[ CallMode = syscall ]] -> true
    ; [[ CallMode = call ]],
      if(tk('Current_Debugger_Mode') \== tk('atom_off'),
        (set_func(tk('address_trace')),
         goto('switch_on_pred')))
    ),
    %
    jump_call4((~func)^.enter_instr).

jump_call4(Enter) => tk('ei') <- Enter, goto('call4').
code_call4 =>
    switch(tk('ei'), (
        case_blk('ENTER_INTERPRETED', pred_call_interpreted),
        case_blk('BUILTIN_DIF', pred_call_builtin_dif),
        case_blk('SPYPOINT', pred_call_spypoint),
        case_blk('WAITPOINT', label_blk('call_waitpoint', pred_call_waitpoint)),
        label_blk('call5', code_call5), % TODO: move outside switch?
        label_blk('default', pred_call_default)
    )).

pred_call_interpreted =>
    [[ update(mode(w)) ]],
    % pred_trace(tk_string("I")),
    x(1) <- ~pointer_to_term((~func)^.code.intinfo),
    set_func(tk('address_interpret_goal')),
    goto('switch_on_pred').

pred_call_builtin_dif =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    localv(ptr(tagged), Pt1, (~w)^.structure),
    localv(tagged, T0), ref_heap_next0(T0,Pt1), x(0) <- T0,
    localv(tagged, T1), ref_heap_next0(T1,Pt1), x(1) <- T1,
    %goto('dif1').
    goto('dif0').

pred_call_spypoint =>
    [[ update(mode(w)) ]],
    if(not((~func)^.properties.wait), jump_call5),
    goto('call_waitpoint').

pred_call_waitpoint =>
    [[ update(mode(w)) ]],
    localv(tagged, T0),
    localv(tagged, T1),
    ref_heap(T0,(~w)^.structure),
    deref_sw(T0,T1, (
       localv(tagged, T3),
       T3 <- x(0),
       % suspend the goal  t3  on  t1.  Func, must be live.
       [[ mode(M) ]],
       setmode(r),
       callstmt('CVOID__CALL', [tk('SUSPEND_T3_ON_T1'), (~func), T3, T1]),
       setmode(M),
       goto_ins(proceed)
    )),
    jump_call5.

jump_call5 => goto('call5').
code_call5 => jump_call4((~func)^.predtyp).

pred_call_default =>
    [[ update(mode(w)) ]],
    localv(intmach, I, (~func)^.arity),
    if(I\==0, (
        localv(ptr(tagged), Pt1, (~w)^.x),
        localv(ptr(tagged), Pt2, (~w)^.structure),
        do_while(
            push_ref_heap_next(Pt1,Pt2),
            (tk('--'),I)
        )
    )),
    jump_switch_on_pred_sub(tk('ei')).

% NOTE: see prolog_dif
pred_enter_builtin_dif =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    localv(tagged, T0, x(0)), deref_sw0(T0,';'),
    localv(tagged, T1, x(1)), deref_sw0(T1,';'),
    (~w)^.structure <- ~null,
    %goto('dif1'),
    % check fast cases first
    %label('dif1'),
    %[[ update(mode(w)) ]],
    if(T0==T1,
      jump_fail,
      if(logical_and(not(~is_var(T0/\T1)), logical_or(~is_atomic(T0), ~is_atomic(T1))), (
          goto_ins(proceed)
      ), (
          x(0) <- T0,
          x(1) <- T1,
          setmode(r),
          if(not(cbool_succeed('prolog_dif', [(~func)])), jump_fail),
          goto_ins(proceed)
      ))
    ).

pred_enter_builtin_abort =>
    [[ update(mode(w)) ]],
    % cut all the way and fail, leaving wam with a return code
    pred_trace(tk_string("B")),
    localv(tagged, T0, x(0)), deref_sw0(T0,';'),
    (~w)^.misc^.exit_code <- ~get_small(T0),
    (~w)^.previous_choice <- tk('InitialChoice'),
    do_cut,
    jump_fail.

pred_enter_spypoint =>
    [[ update(mode(w)) ]],
    if(tk('Current_Debugger_Mode') \== tk('atom_off'),
      (tk('ptemp') <- cast(bcp,tk('address_trace')), % (arity 1)
       goto('escape_to_p'))),
    if(not((~func)^.properties.wait), goto('nowait')),
    goto('waitpoint').

pred_enter_waitpoint =>
    [[ update(mode(w)) ]],
    localv(tagged, T1, x(0)),
    deref_sw(T1,x(0),(
      localv(tagged, T3),
      emul_to_goal(T3), % (stores: t3)
      T1 <- x(0),
      if(~tagged_is(sva,T1), % t1 may have been globalised
        callstmt('RefSVA', [T1,x(0)])),
      % suspend the goal  t3  on  t1.  Func, must be live.
      [[ mode(M) ]],
      setmode(r),
      callstmt('CVOID__CALL', [tk('SUSPEND_T3_ON_T1'), (~func), T3, T1]),
      setmode(M),
      goto_ins(proceed)
    )),
    goto('nowait'),
    label_blk('nowait', jump_switch_on_pred_sub((~func)^.predtyp)).

pred_enter_breakpoint =>
    [[ update(mode(w)) ]],
    jump_switch_on_pred_sub((~func)^.predtyp).

pred_enter_compactcode_indexed =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("E")),
    localv(tagged, T0, x(0)),
    deref_sw(T0,x(0), jump_tryeach((~func)^.code.incoreinfo^.varcase)),
    localv(tagged, T1),
    setmode(r),
    % non variable
    if(T0 /\ tk('TagBitComplex'),
      if(T0 /\ tk('TagBitFunctor'), (
          (~s) <- ~tagged_to_arg(T0,0),
          T1 <- ~heap_next(~s)
      ), (
          (~s) <- ~tagp_ptr(lst, T0),
          jump_tryeach((~func)^.code.incoreinfo^.lstcase)
      )),
      T1 <- T0),
    %
    localv(intmach, I),
    [[ Htab = tk('Htab') ]],
    [[ HtabNode = tk('HtabNode') ]],
    vardecl(ptr(sw_on_key), Htab, (~func)^.code.incoreinfo^.othercase),
    %
    I <- 0,
    localv(tagged, T2),
    T2 <- T1,
    assign(T1 /\ Htab^.mask),
    vardecl(ptr(sw_on_key_node), HtabNode),
    do_while((
        HtabNode <- ~sw_on_key_node_from_offset(Htab, T1),
        if(logical_or(HtabNode^.key==T2, not(HtabNode^.key)), break),
        assign(I + sizeof(sw_on_key_node)),
        T1 <- (T1+I) /\ Htab^.mask
    ), ~true),
    jump_tryeach(HtabNode^.value.try_chain). % (this will break the loop)

pred_enter_compactcode =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("E")),
    jump_tryeach((~func)^.code.incoreinfo^.varcase).

jump_switch_on_pred_sub(Enter), [[ Enter = tk('ei') ]] => goto('switch_on_pred_sub').
jump_switch_on_pred_sub(Enter) =>
    tk('ei') <- Enter,
    goto('switch_on_pred_sub').

code_switch_on_pred_sub => % (needs: ei)
    switch(tk('ei'), (
        case_blk('ENTER_FASTCODE_INDEXED', goto('enter_undefined')),
        case_blk('ENTER_FASTCODE', goto('enter_undefined')),
        case_blk('ENTER_UNDEFINED', label_blk('enter_undefined', pred_enter_undefined)),
        case_blk('ENTER_INTERPRETED', pred_enter_interpreted),
        case_blk('ENTER_C', pred_enter_c),
        case_blk('BUILTIN_TRUE', pred_enter_builtin_true),
        case_blk('BUILTIN_FAIL', pred_enter_builtin_fail),
        case_blk('BUILTIN_CURRENT_INSTANCE', pred_enter_builtin_current_instance),
        case_blk('BUILTIN_COMPILE_TERM', pred_enter_builtin_compile_term),
        case_blk('BUILTIN_INSTANCE', pred_enter_builtin_instance),
        case_blk('BUILTIN_GELER', pred_enter_builtin_geler),
        case_blk('BUILTIN_NODEBUGCALL', pred_enter_builtin_nodebugcall),
        case_blk('BUILTIN_SYSCALL', pred_enter_builtin_syscall),
        label_blk('call4', code_call4), % TODO: move outside switch?
        case_blk('BUILTIN_CALL', pred_enter_builtin_call),
        case_blk('BUILTIN_DIF', label_blk('dif0', pred_enter_builtin_dif)),
        case_blk('BUILTIN_ABORT', pred_enter_builtin_abort),
        case_blk('SPYPOINT', pred_enter_spypoint),
        case_blk('WAITPOINT', label_blk('waitpoint', pred_enter_waitpoint)),
        case_blk('BREAKPOINT', pred_enter_breakpoint),
        case_blk('ENTER_PROFILEDCODE_INDEXED', goto('enter_compactcode_indexed')),
        case_blk('ENTER_COMPACTCODE_INDEXED', label_blk('enter_compactcode_indexed', pred_enter_compactcode_indexed)),
        case_blk('ENTER_PROFILEDCODE', goto('enter_compactcode')),
        case_blk('ENTER_COMPACTCODE', label_blk('enter_compactcode', pred_enter_compactcode))
    )).

code_exit_toplevel =>
    (~w)^.insn <- (~p),
    % What should we save here? MCL
    % w->choice = B;
    % w->frame = E->frame;
    if(logical_and(not_null(tk('desc')), tk('desc')^.action /\ tk('KEEP_STACKS')),
       % We may backtrack
       call0('SAVE_WAM_STATE')),
    % We may have been signaled and jumped here from enter_predicate:
    if(cbool_succeed('Stop_This_Goal', []),
       (~w)^.misc^.exit_code <- tk('WAM_INTERRUPTED')),
    trace(wam_loop_exit),
    return.

code_illop =>
    serious_fault(tk_string("unimplemented WAM instruction")).

% Alternative and instruction dispatcher
alt_ins_dispatcher =>
    alt_ins_dispatcher(r),
    alt_ins_dispatcher(w).

% Alternative and instruction dispatcher (read or write mode)
alt_ins_dispatcher(Mode) =>
    [[ update(mode(Mode)) ]],
    alt_dispatcher,
    ins_dispatcher.

% emul_p(Alts, EmulP), [[ mode(r) ]] => [[ EmulP = (Alts^.emul_p) ]]. % TODO:[merge-oc] no p2 optimization, disable X0 optimization? (it runs slower)
emul_p(Alts, EmulP), [[ mode(r) ]] => [[ EmulP = (Alts^.emul_p2) ]]. % TODO:[merge-oc] no p2 optimization, disable X0 optimization? (it runs slower)
emul_p(Alts, EmulP), [[ mode(w) ]] => [[ EmulP = (Alts^.emul_p) ]].

jump_tryeach(Alts) =>
    tk('alts') <- Alts,
    tryeach_lab(Lab),
    goto(Lab).

tryeach_lab(Lab) => vlabel('tryeach', Lab).

alt_dispatcher => % (needs: alts)
    tryeach_lab(TryEach),
    label_blk(TryEach, alt_dispatcher_).

alt_dispatcher_ =>
    [[ Alts = tk('alts') ]],
    %
    gauge_incr_counter_alts(Alts),
    %
    emul_p(Alts, EmulP),
    (~p) <- EmulP,
    % TODO:[merge-oc] try_alt/1
    (~w)^.previous_choice <- (~w)^.choice,
    %
    localv(ptr(try_node), Alt, (Alts^.next)),
    if(not_null(Alt), ( % TODO: This one is not a deep check! (see line above)
      (~b) <- (~w)^.choice,
      get_frame_top((~w)^.local_top, ~b, (~g)^.frame),
      cachedreg('H',H),
      choice_new0(~b, Alt, H),
      trace(create_choicepoint),
      % segfault patch -- jf
      maybe_choice_overflow
    ),(
      set_deep
    )),
    %
    jump_ins_dispatch.

gauge_incr_counter_alts(Alts) => % Counter in Alts
    ( [[ mode(r) ]] -> [[ EntryCounter = (Alts^.entry_counter + 1) ]] % TODO: do not use pointer arith
    ; [[ mode(w) ]], [[ EntryCounter = (Alts^.entry_counter) ]]
    ),
    gauge_incr_counter(EntryCounter).

jump_ins_dispatch =>
    ins_dispatch_label(DispatchLabel),
    goto(DispatchLabel).

ins_dispatch_label(Label) => vlabel('dispatch', Label).

ins_dispatcher =>
    ins_dispatch_label(Label),
    label_blk(Label, ins_dispatcher_).

ins_dispatcher_ =>
    [[ collect_and_filter(instruction_set, '$ins_entry'/4, Insns) ]],
    switch(tk('BcOPCODE'),
      (% (all instructions)
      '$foreach'(Insns, ins_case),
      label_blk('default', goto('illop')))).

% Instruction case
ins_case('$ins_entry'(Opcode,InsSpec,InsCode,Format)), [[ get(op(Opcode).optional, Flag) ]] =>
    % Emit optional instructions (based on C preprocessor flags)
    cpp_if_defined(Flag),
    ins_case_(Opcode,InsSpec,InsCode,Format),
    cpp_endif.
ins_case('$ins_entry'(Opcode,InsSpec,InsCode,Format)) => ins_case_(Opcode,InsSpec,InsCode,Format).

% TODO: write instruction as comment? [[ uppercase(Ins, InsUp) ]], % (do not use name, just opcode)
ins_case_(Opcode,InsSpec,InsCode,Format) =>
    get_op_label(Opcode, Label), % 'label' for the instruction
    case_blk(Opcode, ulabel_blk(Label, ins_case__(Opcode,InsSpec,InsCode,Format))).

ins_case__(Opcode, InsSpec, InsCode, Format) =>
    [[ mode(M0) ]],
    [[ update(format(Format)) ]],
    ( [[ get(op(Opcode).in_mode, M) ]] -> in_mode(M, InsSpec, InsCode)
    ; InsCode
    ),
    [[ update(mode(M0)) ]].

% Wrapper for execution of instruction G in the specified mode M
in_mode(M, _, G), [[ mode(M) ]] => G.
in_mode(M, InsSpec, _) => setmode(M), goto_ins(InsSpec).

get_op_label(Op, Label) =>
    [[ prefix_num('op', Op, Label0) ]],
    vlabel(Label0, Label).

%! # Instruction entries
% NOTE: declaration order is important (for performance)

ins_entry(Ins,Opcode,InsSpec) => ins_entry(Ins,Opcode,InsSpec,[]).

% TODO: we need more complex rules (see OC)
ins_entry(Ins,Opcode,InsSpec,Props) =>
    ispeccode(Ins,InsSpec,Format,InsCode),
    is_exported(Props,Props2,Export),
    ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,Props2).

% TODO: use := ?
is_exported(exported(Name)+Props, Props, Export) => [[ Export = yes(Name) ]].
is_exported(Props, Props, Export) => [[ Export = no ]].

% instruction spec/merge macro
ispec(X+Y, (Xf,Yf), (Xa,Ya), (Xc,Yc), Cont) =>
    ispec(X, Xf, Xa, Xc, -), % allow merge only if no explicit dispatch
    ispec(Y, Yf, Ya, Yc, Cont).
ispec(alloc, true, true, alloc, -) => true. % (no args)
ispec(un_voidr(a), f_i, N, un_voidr(N), -) => true.
ispec(un_var(F), F, X, un_var(X), -) => true.
ispec(un_val(F), F, X, un_val(X), -) => true.
ispec(un_lval(F), F, X, un_lval(X), -) => true.
ispec(un_fval(F), F, X, un_fval(X), -) => true.
ispec(u_val(Xf,Yf), (Xf,Yf), (X,Y), u_val(X,Y), -) => true.
ispec(u_fval(Xf,Yf), (Xf,Yf), (X,Y), u_fval(X,Y), -) => true.
ispec(init2h(Xf,Yf), (Xf,Yf), (X,Y), init2h(X,Y), -) => true.
ispec(init2s(Xf,Yf), (Xf,Yf), (X,Y), init2s(X,Y), -) => true.
ispec(move(Xf,Yf), (Xf,Yf), (X,Y), move(X,Y), -) => true.
ispec(revmove(Xf,Yf), (Xf,Yf), (X,Y), revmove(X,Y), -) => true.
ispec(getchoice(F), F, X, getchoice(X), -) => true.
ispec(inith(F), F, X, inith(X), -) => true.
ispec(bump_counter(F), F, X, bump_counter(X), -) => true.
ispec(unify_void(I), true, true, unify_void(I), dispatch), [[ integer(I) ]] => true.
ispec(unify_void(f_i), f_i, N, unify_void(N), dispatch) => true.
%
ispec(firstcall(f_Y,PredPtrT,EnvSizeT), (f_Y,PredPtrT,EnvSizeT), true, firstcall_n, nodecops_dispatch) => true.
ispec(firstcall(s(0),Xf,Yf), (Xf,Yf), (X,Y), firstcall_0(X,Y), dispatch) => true.
ispec(firstcall(s(I),Xf,Yf), (Fs,Xf,Yf), (X,Y), firstcall(I), nodecops_dispatch), [[ integer(I) ]] => repf(I, f_y, Fs).
%
ispec(Ins, _, _, _, _) => error(ispec(Ins)). % TODO: good error lit

% repeat I times F in Fs
repf(1, F, F) => true.
repf(I, F, (F,Fs)) => [[ I > 1, I1 is I - 1 ]], repf(I1, F, Fs).

% split first from InsSpec
% TODO: ugly, translate to lists first
spec1((A+B)+C, First, Rest+C) => spec1(A+B, First, Rest).
spec1(A+B, A, B) => true.

ispecnorm(Ins,InsSpec,InsSpec2) =>
    ( [[ InsSpec = [] ]] -> [[ InsSpec2 = Ins ]] % TODO: deprecate
    ; [[ InsSpec = [_|_] ]] -> [[ InsSpec2 = Ins ]] % TODO: deprecate
    ; [[ InsSpec2 = InsSpec ]]
    ).

ispecnormq(Ins,InsSpecQ,InsSpecQ2) =>
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ( [[ InsSpecQ = [] ]] -> [[ InsSpecQ2 = InsQ ]] % TODO: deprecate
    ; [[ InsSpecQ = [_|_] ]] -> [[ InsSpecQ2 = InsQ ]] % TODO: deprecate
    ; [[ InsSpecQ2 = q+InsSpecQ ]]
    ).

ispeccode(Ins,InsSpec,Format,InsCode) =>
    ( [[ InsSpec = [_|_] ]] -> [[ Format = InsSpec, InsCode = Ins ]]
    ; [[ InsSpec = [] ]] -> [[ Format = InsSpec, InsCode = Ins ]]
    ; ispec(InsSpec,Format1,InsArgs1,InsCode0,Cont),
      [[ conj_to_list(Format1,Format) ]],
      [[ conj_to_list(InsArgs1,InsArgs) ]],
      ( [[ Cont = dispatch ]] -> % (dispatch is explicit in ins)
          [[ InsCode = (decops(InsArgs),InsCode0) ]]
      ; [[ Cont = nodecops_dispatch ]] -> % (no decops, dispatch is explicit in ins)
          [[ InsCode = InsCode0 ]]
      ; [[ InsCode = (decops(InsArgs),InsCode0,dispatch) ]]
      )
    ).

ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,[q0|Props]) => % (see q0)
    ispecnorm(Ins,InsSpec,InsSpec2),
    ins_entry_align(Ins,Opcode,Format,Export,InsSpec,q0(InsSpec2),[],InsCode,Props).
ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,[q0w|Props]) => % (see q0w)
    ispecnorm(Ins,InsSpec,InsSpec2),
    ins_entry_align(Ins,Opcode,Format,Export,InsSpec,rw(InsCode,q0(InsSpec2)),[],InsCode,Props).
ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,[q0r|Props]) => % (see q0r)
    ispecnorm(Ins,InsSpec,InsSpec2),
    ins_entry_align(Ins,Opcode,Format,Export,InsSpec,rw(q0(InsSpec2),InsCode),[],InsCode,Props).
ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,[qall|Props]) => % (no props in Q version)
    ins_entry_align(Ins,Opcode,Format,Export,InsSpec,InsCode,[],InsCode,Props).
ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,[qqall|Props]) => % (props in both)
    ins_entry_align(Ins,Opcode,Format,Export,InsSpec,InsCode,Props,InsCode,Props).
ins_entry_(Ins,Opcode,Format,_InsCode,InsSpec,Export,[1|Props]) => 
    spec1(InsSpec, InsCode, InsCont),
    ins_entry_(Ins,Opcode,Format,i_ec(InsCode,InsCont),InsSpec,Export,Props). % (call again) % TODO: merge into e(...)
ins_entry_(Ins,Opcode,Format,InsCode,InsSpec,Export,Props) =>
    ispecnorm(Ins,InsSpec,InsSpec2),
    prim('$decl'(ins_spec(InsSpec2,Opcode))),
    ( [[ Props = [rw(R,W)|Props2] ]] -> [[ InsCode2 = i_rw(InsCode,InsSpec,R,W) ]]
    ; [[ Props = Props2, InsCode2 = InsCode ]]
    ),
    ins_entry__(InsSpec2,Opcode,Format,Export,InsCode2,Props2).

ins_entry_align(Ins,Opcode,Format,Export,InsSpec,InsCodeQ,PropsQ,InsCode,Props) =>
    [[ OpcodeQ is Opcode - 1 ]],
    ( [[ Export = yes(InsName) ]] -> [[ atom_concat(InsName, 'q', InsNameQ), ExportQ = yes(InsNameQ) ]] ; [[ ExportQ = no ]] ),
    %
    ispecnorm(Ins,InsSpec,InsSpec2),
    prim('$decl'(ins_spec(InsSpec2,Opcode))), % TODO: use spec
    %
    ispecnormq(Ins,InsSpec,InsSpecQ2),
    prim('$decl'(ins_spec(InsSpecQ2,OpcodeQ))), % TODO: use spec (add q+spec)
    %
    ins_entry__(InsSpecQ2, OpcodeQ, [f_Q|Format], ExportQ, InsCodeQ, PropsQ),
    ins_entry__(InsSpec2, Opcode, Format, Export, InsCode, Props).

ins_entry__(InsSpec,Opcode,Format,yes(InsName),InsCode,Props) =>
    prim('$exported_ins'(Opcode, InsName)),
    ins_entry__(InsSpec,Opcode,Format,no,InsCode,Props).
ins_entry__(InsSpec,Opcode,Format,no,InsCode,Props) =>
    prim('$decl'(ins_op_format(Opcode,InsSpec,Format,Props))),
    prim('$ins_entry'(Opcode,InsSpec,InsCode,Format)).

% shift padding and go to unpadded instruction
q0(InsSpec) => shiftf, goto_ins(InsSpec).

rw(_, InsCode), [[ mode(w) ]] => InsCode.
rw(InsCode, _), [[ mode(r) ]] => InsCode.

% goto in read mode, ins in write mode
i_rw(_InsCode,_InsSpec,e(InsR,0),_), [[ mode(r) ]] => goto_ins(InsR).
i_rw(InsCode,_InsSpec,_,all), [[ mode(w) ]] => InsCode.
i_rw(_InsCode,InsSpec,_,1), [[ mode(w) ]] =>
    spec1(InsSpec, InsCode, InsCont),
    i_ec(InsCode, InsCont).

% equiv+cont % TODO: missing args
i_ec(InsCode,InsCont) => InsCode, goto_ins(InsCont).

% TODO: put(Key,Val) or Key<-Val?

:- '$decl'(ins_op_format/4).
ins_op_format(Op, InsSpec, Format, Props) :-
    putmax(max_op, Op),
    put(op(Op).format, Format),
    put(op(Op).ins_spec, InsSpec),
    ins_op_props(Op, Props).

:- '$decl'(ins_op_props/2).
ins_op_props(_Op, []) :- true. % TODO: allow facts (no ":- true")
ins_op_props(Op, [Prop|Props]) :-
    ins_op_prop(Op, Prop),
    ins_op_props(Op, Props).

:- '$decl'(ins_op_prop/2).
% instruction always switches to mode M
ins_op_prop(Op, in_mode(M)) :- put(op(Op).in_mode, M).
% instruction is optional (under cpp flag)
ins_op_prop(Op, optional(Name)) :- put(op(Op).optional, Name).

% Declare the opcode for the given InsSpec (which can be a complex term)
:- '$decl'(ins_spec/2).
ins_spec(InsSpec, Op) :- put(ins_spec(InsSpec).op, Op).

%! # Instruction set switch
% NOTE: declaration order is important (for performance)

%   ci_call, 241, [f_i,f_i]
%   ci_inarg, 242, [f_i,f_i]
%   ci_outarg, 243, [f_i,f_i]
%   ci_retval, 244, [f_i,f_i]

instruction_set =>
    iset_init,
    iset_call,
    iset_put,
    iset_get1,
    iset_cut,
    iset_choice,
    iset_misc1,
    iset_get2,
    ins_entry(branch, 68, [f_i], exported(branch)+[]), % (for qread)
    iset_blt,
    ins_entry(get_constraint, 247, [f_x], exported(get_constraint)+[in_mode(w)]), % (for compile_term_aux)
    iset_unify,
    iset_u2,
    iset_misc2.

iset_init =>
    ins_entry(inittrue, 260, [f_e], [in_mode(w)]),
    ins_entry(firsttrue_n, 261, [f_Y,f_e], [in_mode(w)]),
    ins_entry(initcall, 1, [f_E,f_e], [q0,in_mode(w)]).

iset_call =>
    ins_entry(-, 21, firstcall(f_Y,f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 19, firstcall(s(8),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 17, firstcall(s(7),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 15, firstcall(s(6),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 13, firstcall(s(5),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 11, firstcall(s(4),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 9, firstcall(s(3),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 7, firstcall(s(2),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 5, firstcall(s(1),f_E,f_e), [q0,in_mode(w)]),
    ins_entry(-, 3, firstcall(s(0),f_E,f_e), [q0,in_mode(w)]),
    %
    ins_entry(call_n, 41, [f_Z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_8, 39, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_7, 37, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_6, 35, [f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_5, 33, [f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_4, 31, [f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_3, 29, [f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_2, 27, [f_z,f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call_1, 25, [f_z,f_E,f_e], [q0,in_mode(w)]),
    ins_entry(call, 23, [f_E,f_e], exported(call)+[q0,in_mode(w)]), % (for ciao_initcode and init_some_bytecode)
    %
    ins_entry(lastcall_n, 61, [f_Z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_8, 59, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_7, 57, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_6, 55, [f_z,f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_5, 53, [f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_4, 51, [f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_3, 49, [f_z,f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_2, 47, [f_z,f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall_1, 45, [f_z,f_E], [q0,in_mode(w)]),
    ins_entry(lastcall, 43, [f_E], exported(lastcall)+[q0,in_mode(w)]), % (for chat_tabling.c)
    %
    ins_entry(execute, 63, [f_E], exported(execute)+[qall]). % (for ciao_initcode and init_some_bytecode)

iset_put =>
    ins_entry(-, 69, inith(f_x), [in_mode(w)]),
    ins_entry(-, 70, init2h(f_x,f_x), [in_mode(w)]),
    ins_entry(-, 85, revmove(f_x,f_x)+revmove(f_x,f_x)),
    ins_entry(-, 71, revmove(f_x,f_x)),
    ins_entry(put_x_unsafe_value, 72, [f_x,f_x], [in_mode(w)]),
    ins_entry(-, 73, alloc+init2s(f_x,f_y), [1,in_mode(w)]),
    ins_entry(-, 74, init2s(f_x,f_y), [in_mode(w)]),
    ins_entry(-, 83, alloc+init2s(f_x,f_y)+init2s(f_x,f_y), [1,in_mode(w)]),
    ins_entry(-, 84, init2s(f_x,f_y)+init2s(f_x,f_y), [in_mode(w)]),
    ins_entry(-, 75, revmove(f_x,f_y)),
    ins_entry(put_y_unsafe_value, 76, [f_x,f_y], [in_mode(w)]),
    ins_entry(put_constant, 78, [f_x,f_t], [qqall]),
    ins_entry(put_nil, 81, [f_x]),
    ins_entry(put_large, 253, [f_x,f_b], [qqall,in_mode(w)]),
    ins_entry(put_structure, 80, [f_x,f_f], [qqall,in_mode(w)]),
    ins_entry(put_list, 82, [f_x], [in_mode(w)]),
    ins_entry(-, 86, revmove(f_x,f_y)+revmove(f_x,f_y)),
    ins_entry(put_yval_yuval, 87, [f_x,f_y,f_x,f_y], [in_mode(w)]),
    ins_entry(put_yuval_yval, 88, [f_x,f_y,f_x,f_y], [in_mode(w)]),
    ins_entry(put_yuval_yuval, 89, [f_x,f_y,f_x,f_y], [in_mode(w)]).

iset_blt =>
    ins_entry(function_1, 223, [f_x,f_x,f_C,f_g], [qqall,in_mode(r)]),
    ins_entry(function_2, 225, [f_x,f_x,f_x,f_C,f_g], [qqall,in_mode(r)]),
    ins_entry(builtin_1, 227, [f_x,f_C], [qqall,in_mode(r)]),
    ins_entry(builtin_2, 229, [f_x,f_x,f_C], [qqall,in_mode(r)]),
    ins_entry(builtin_3, 231, [f_x,f_x,f_x,f_C], [qqall,in_mode(r)]),
    ins_entry(retry_instance, 232, [], exported(retry_instance)+[in_mode(r)]).

iset_get1 =>
    % TODO:[oc-merge] u_val order in instruction 91 was reversed before oc merge
    ins_entry(-, 91, u_val(f_x,f_x), exported(get_x_value)+[in_mode(r)]), % (for cterm)
    ins_entry(-, 94, u_fval(f_x,f_y), [in_mode(r)]),
    ins_entry(-, 95, u_val(f_x,f_y), [in_mode(r)]),
    ins_entry(get_constant, 97, [f_x,f_t], exported(get_constant)+[q0,in_mode(r)]), % (for cterm)
    ins_entry(get_large, 255, [f_x,f_b], exported(get_large)+[q0,in_mode(r)]), % (for cterm)
    ins_entry(get_structure, 99, [f_x,f_f], exported(get_structure)+[q0,in_mode(r)]), % (for cterm)
    ins_entry(get_nil, 100, [f_x], exported(get_nil)+[in_mode(r)]), % (for cterm)
    ins_entry(get_list, 101, [f_x], exported(get_list)+[in_mode(r)]), % (for cterm)
    ins_entry(get_constant_neck_proceed, 112, [f_x,f_t], [q0,in_mode(r)]),
    ins_entry(get_nil_neck_proceed, 113, [f_x], [in_mode(r)]).

iset_cut =>
    ins_entry(cutb_x, 208, [f_x], [in_mode(r)]),
    ins_entry(cutb_x_neck, 210, [f_x], [in_mode(r)]),
    ins_entry(cutb_neck, 211, [], [in_mode(r)]),
    ins_entry(cutb_x_neck_proceed, 212, [f_x], [in_mode(r)]),
    ins_entry(cutb_neck_proceed, 213, [], [in_mode(r)]),
    ins_entry(cute_x, 214, [f_x], [in_mode(r)]),
    ins_entry(cute_x_neck, 216, [f_x], [in_mode(r)]),
    ins_entry(cute_neck, 217, [], [in_mode(r)]),
    ins_entry(cutf_x, 215, [f_x], [in_mode(r)]),
    ins_entry(cutf, 209, [], [in_mode(r)]),
    ins_entry(cut_y, 218, [f_y], [in_mode(r)]).

iset_choice =>
    ins_entry(-, 219, getchoice(f_x)),
    ins_entry(-, 220, alloc+getchoice(f_y), [1]),
    ins_entry(-, 221, getchoice(f_y)).

iset_misc1 =>
    ins_entry(kontinue, 233, [], exported(kontinue)+[in_mode(w)]),
    ins_entry(leave, 234, [], [in_mode(r)]),
    ins_entry(exit_toplevel, 235, [], exported(exit_toplevel)+[in_mode(r)]),
    ins_entry(retry_c, 238, [f_C], exported(retry_c)+[qqall,in_mode(r)]).

iset_get2 =>
    ins_entry(get_structure_x0, 105, [f_f], exported(get_structure_x0)+[q0w]), % (for cterm)
    ins_entry(get_large_x0, 257, [f_b], [q0w]),
    ins_entry(get_constant_x0, 103, [f_t], exported(get_constant_x0)+[q0w]), % (for cterm)
    ins_entry(get_nil_x0, 106, [], exported(get_nil_x0)+[]), % (for cterm)
    ins_entry(get_list_x0, 107, [], exported(get_list_x0)+[]), % (for cterm)
    ins_entry(-, 108, move(f_x,f_x)+move(f_x,f_x)),
    ins_entry(-, 90, move(f_x,f_x), exported(get_x_variable)+[]), % (for cterm)
    ins_entry(-, 92, alloc+move(f_x,f_y), [1]),
    ins_entry(-, 93, move(f_x,f_y), []),
    ins_entry(-, 109, alloc+move(f_x,f_y)+move(f_x,f_y), [1]),
    ins_entry(-, 110, move(f_x,f_y)+move(f_x,f_y), []).

iset_unify =>
    ins_entry(-, 114, unify_void(f_i), exported(unify_void)+[]), % (for cterm)
    ins_entry(-, 115, unify_void(1), exported(unify_void_1)+[]), % (for cterm)
    ins_entry(-, 116, unify_void(2), exported(unify_void_2)+[]), % (for cterm)
    ins_entry(-, 117, unify_void(3), exported(unify_void_3)+[]), % (for cterm)
    ins_entry(-, 118, unify_void(4), exported(unify_void_4)+[]), % (for cterm)
    ins_entry(-, 119, un_var(f_x), exported(unify_x_variable)+[]), % (for cterm)
    ins_entry(-, 120, un_val(f_x), exported(unify_x_value)+[rw(e(un_lval(f_x),0),all)]), % (for cterm)
    ins_entry(-, 121, un_lval(f_x), []),
    ins_entry(-, 122, alloc+un_var(f_y), [1]),
    ins_entry(-, 123, un_var(f_y), []),
    ins_entry(-, 124, un_fval(f_y)),
    ins_entry(-, 125, un_val(f_y), [rw(e(un_lval(f_y),0),all)]),
    ins_entry(-, 126, un_lval(f_y), []),
    ins_entry(unify_constant, 128, [f_t], exported(unify_constant)+[q0r]), % (for cterm)
    ins_entry(unify_large, 259, [f_b], exported(unify_large)+[q0]), % (for cterm)
    ins_entry(unify_structure, 130, [f_f], exported(unify_structure)+[q0r]), % (for cterm)
    ins_entry(unify_nil, 131, [], exported(unify_nil)+[]), % (for cterm)
    ins_entry(unify_list, 132, [], exported(unify_list)+[]), % (for cterm)
    ins_entry(unify_constant_neck_proceed, 134, [f_t], [q0r]),
    ins_entry(unify_nil_neck_proceed, 135, []).

iset_u2 =>
    ins_entry(-, 136, un_voidr(a)+un_var(f_x)),
    ins_entry(-, 139, alloc+un_voidr(a)+un_var(f_y), [1]),
    ins_entry(-, 140, un_voidr(a)+un_var(f_y), []),
    ins_entry(-, 137, un_voidr(a)+un_val(f_x), [rw(e(un_voidr(a)+un_lval(f_x),0),all)]),
    ins_entry(-, 138, un_voidr(a)+un_lval(f_x), []),
    ins_entry(-, 141, un_voidr(a)+un_fval(f_y)),
    ins_entry(-, 142, un_voidr(a)+un_val(f_y), [rw(e(un_voidr(a)+un_lval(f_y),0),all)]),
    ins_entry(-, 143, un_voidr(a)+un_lval(f_y), []),
    ins_entry(-, 144, un_var(f_x)+un_voidr(a)),
    ins_entry(-, 145, un_var(f_x)+un_var(f_x)),
    ins_entry(-, 148, alloc+un_var(f_x)+un_var(f_y), [1]),
    ins_entry(-, 149, un_var(f_x)+un_var(f_y), []),
    ins_entry(-, 146, un_var(f_x)+un_val(f_x), [rw(e(un_var(f_x)+un_lval(f_x),0),all)]),
    ins_entry(-, 147, un_var(f_x)+un_lval(f_x), []),
    ins_entry(-, 150, un_var(f_x)+un_fval(f_y)),
    ins_entry(-, 151, un_var(f_x)+un_val(f_y), [rw(e(un_var(f_x)+un_lval(f_y),0),all)]),
    ins_entry(-, 152, un_var(f_x)+un_lval(f_y), []),
    ins_entry(-, 153, alloc+un_var(f_y)+un_voidr(a), [1]),
    ins_entry(-, 154, un_var(f_y)+un_voidr(a), []),
    ins_entry(-, 155, alloc+un_var(f_y)+un_var(f_x), [1]),
    ins_entry(-, 156, un_var(f_y)+un_var(f_x), []),
    ins_entry(-, 157, alloc+un_var(f_y)+un_var(f_y), [1]),
    ins_entry(-, 158, un_var(f_y)+un_var(f_y), []),
    ins_entry(-, 159, alloc+un_var(f_y)+un_val(f_x), [rw(e(alloc+un_var(f_y)+un_lval(f_x),0),1)]),
    ins_entry(-, 161, alloc+un_var(f_y)+un_lval(f_x), [1]),
    ins_entry(-, 160, un_var(f_y)+un_val(f_x), [rw(e(un_var(f_y)+un_lval(f_x),0),all)]),
    ins_entry(-, 162, un_var(f_y)+un_lval(f_x), []),
    ins_entry(-, 163, alloc+un_var(f_y)+un_val(f_y), [rw(e(alloc+un_var(f_y)+un_lval(f_y),0),1)]),
    ins_entry(-, 165, alloc+un_var(f_y)+un_lval(f_y), [1]),
    ins_entry(-, 164, un_var(f_y)+un_val(f_y), [rw(e(un_var(f_y)+un_lval(f_y),0),all)]),
    ins_entry(-, 166, un_var(f_y)+un_lval(f_y), []),
    ins_entry(-, 185, un_fval(f_y)+un_voidr(a)),
    ins_entry(-, 188, un_fval(f_y)+un_var(f_x)),
    ins_entry(-, 199, un_fval(f_y)+un_fval(f_y)),
    ins_entry(-, 193, un_fval(f_y)+un_val(f_x), [rw(e(un_fval(f_y)+un_lval(f_x),0),all)]),
    ins_entry(-, 196, un_fval(f_y)+un_lval(f_x), []),
    ins_entry(-, 202, un_fval(f_y)+un_val(f_y), [rw(e(un_fval(f_y)+un_lval(f_y),0),all)]),
    ins_entry(-, 205, un_fval(f_y)+un_lval(f_y), []),
    ins_entry(-, 167, un_val(f_x)+un_voidr(a), [rw(e(un_lval(f_x)+un_voidr(a),0),all)]),
    ins_entry(-, 168, un_lval(f_x)+un_voidr(a), []),
    ins_entry(-, 169, un_val(f_x)+un_var(f_x), [rw(e(un_lval(f_x)+un_var(f_x),0),all)]),
    ins_entry(-, 170, un_lval(f_x)+un_var(f_x), []),
    ins_entry(-, 171, alloc+un_val(f_x)+un_var(f_y), [rw(e(alloc+un_lval(f_x)+un_var(f_y),0),1)]),
    ins_entry(-, 172, alloc+un_lval(f_x)+un_var(f_y), [1]),
    ins_entry(-, 173, un_val(f_x)+un_var(f_y), [rw(e(un_lval(f_x)+un_var(f_y),0),all)]),
    ins_entry(-, 174, un_lval(f_x)+un_var(f_y), []),
    ins_entry(-, 175, un_val(f_x)+un_val(f_x), [rw(e(un_val(f_x)+un_lval(f_x),0),all)]),
    ins_entry(-, 177, un_val(f_x)+un_lval(f_x), [rw(e(un_lval(f_x)+un_val(f_x),0),all)]),
    ins_entry(-, 176, un_lval(f_x)+un_val(f_x), [rw(e(un_lval(f_x)+un_lval(f_x),0),all)]),
    ins_entry(-, 178, un_lval(f_x)+un_lval(f_x), []),
    ins_entry(-, 179, un_val(f_x)+un_fval(f_y), [rw(e(un_lval(f_x)+un_fval(f_y),0),all)]),
    ins_entry(-, 180, un_lval(f_x)+un_fval(f_y), []),
    ins_entry(-, 181, un_val(f_x)+un_val(f_y), [rw(e(un_val(f_x)+un_lval(f_y),0),all)]),
    ins_entry(-, 183, un_val(f_x)+un_lval(f_y), [rw(e(un_lval(f_x)+un_val(f_y),0),all)]),
    ins_entry(-, 182, un_lval(f_x)+un_val(f_y), [rw(e(un_lval(f_x)+un_lval(f_y),0),all)]),
    ins_entry(-, 184, un_lval(f_x)+un_lval(f_y), []),
    ins_entry(-, 186, un_val(f_y)+un_voidr(a), [rw(e(un_lval(f_y)+un_voidr(a),0),all)]),
    ins_entry(-, 187, un_lval(f_y)+un_voidr(a), []),
    ins_entry(-, 189, un_val(f_y)+un_var(f_x), [rw(e(un_lval(f_y)+un_var(f_x),0),all)]),
    ins_entry(-, 190, un_lval(f_y)+un_var(f_x), []),
    ins_entry(-, 191, un_val(f_y)+un_var(f_y), [rw(e(un_lval(f_y)+un_var(f_y),0),all)]),
    ins_entry(-, 192, un_lval(f_y)+un_var(f_y), []),
    ins_entry(-, 200, un_val(f_y)+un_fval(f_y), [rw(e(un_lval(f_y)+un_fval(f_y),0),all)]),
    ins_entry(-, 201, un_lval(f_y)+un_fval(f_y), []),
    ins_entry(-, 194, un_val(f_y)+un_val(f_x), [rw(e(un_val(f_y)+un_lval(f_x),0),all)]),
    ins_entry(-, 197, un_val(f_y)+un_lval(f_x), [rw(e(un_lval(f_y)+un_val(f_x),0),all)]),
    ins_entry(-, 195, un_lval(f_y)+un_val(f_x), [rw(e(un_lval(f_y)+un_lval(f_x),0),all)]),
    ins_entry(-, 198, un_lval(f_y)+un_lval(f_x), []),
    ins_entry(-, 203, un_val(f_y)+un_val(f_y), [rw(e(un_val(f_y)+un_lval(f_y),0),all)]),
    ins_entry(-, 206, un_val(f_y)+un_lval(f_y), [rw(e(un_lval(f_y)+un_val(f_y),0),all)]),
    ins_entry(-, 204, un_lval(f_y)+un_val(f_y), [rw(e(un_lval(f_y)+un_lval(f_y),0),all)]),
    ins_entry(-, 207, un_lval(f_y)+un_lval(f_y), []).

iset_misc2 =>
    ins_entry(-, 249, bump_counter(f_l), [q0]),
    ins_entry(counted_neck, 251, [f_l,f_l], [q0]),
    ins_entry(fail, 67, [], exported(fail)+[]), % (for ciao_initcode and init_some_bytecode)
    ins_entry(heapmargin_call, 246, [f_g], exported(heapmargin_call)+[q0]), % (for compile_term_aux)
    ins_entry(neck, 65, [], []),
    ins_entry(dynamic_neck_proceed, 236, [], exported(dynamic_neck_proceed)+[in_mode(w)]),
    ins_entry(neck_proceed, 66, [], [in_mode(w)]),
    ins_entry(proceed, 64, [], []),
    ins_entry(restart_point, 262, [], exported(restart_point)+[optional('PARBACK')]). % (for ciao_initcode and init_some_bytecode)

% Set declarations (globally) from the instruction set
% TODO: hack, we need better control of context
:- [[ collect_and_filter(instruction_set, '$decl'/1, G) ]], '$exec_decls'(G).

