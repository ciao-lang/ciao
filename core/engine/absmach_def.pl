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

% (prelude)
:- '$decl'(rprop/2).
rprop(_Head, []) :- true.
rprop(Head, [K=V|Props]) :-
    put(rule(Head).K, V), % TODO: use richer structure, e.g., rule(Head).K<-V
    rprop(Head, Props).

% ---------------------------------------------------------------------------
%! # C syntax and control constructs

:- rprop(fmtbb/0, [level=grammar]).
fmtbb => [[ indent(N) ]], tk_bb(N).

:- rprop(fmtinc/0, [level=grammar]).
fmtinc =>
    [[ indent(N) ]],
    [[ N1 is N + 2 ]],
    [[ update(indent(N1)) ]].
:- rprop(fmtdec/0, [level=grammar]).
fmtdec =>
    [[ indent(N) ]],
    [[ N1 is N - 2 ]],
    [[ update(indent(N1)) ]].

:- rprop(stmtend/0, [level=grammar]).
stmtend => tk(';'), tk_nl.

:- rprop('{'/0, [level=grammar]).
'{' => tk('{').
:- rprop('}'/0, [level=grammar]).
'}' => tk('}').
:- rprop('('/0, [level=grammar]).
'(' => tk('(').
:- rprop(')'/0, [level=grammar]).
')' => tk(')').
:- rprop(' '/0, [level=grammar]).
' ' => tk(' ').
:- rprop(';'/0, [level=grammar]).
';' => tk(';').

:- rprop(paren/1, [level=grammar]).
paren(Exp) => '(', Exp, ')'.

:- rprop(blk/1, [level=grammar]).
blk(Code) => '{', fmtinc, tk_nl, Code, fmtdec, fmtbb, '}'.

:- rprop(for/2, [level=grammar]).
for(Range, Code) => tk('for'), ' ', paren(Range), ' ', blk(Code), tk_nl.

:- rprop(foreach/4, [level=grammar]).
foreach(Ty, range(To), V, Code) => % V in [0,..., To-1]
    for((localv(Ty, V, 0), (V < To), ';', assign0(V+1)), Code).
foreach(Ty, revrange(From), V, Code) => % V in [From,...,0+1] (reverse)
    for((localv(Ty, V, From), (V > 0), ';', assign0(V-1)), Code).
foreach(Ty, revrange(From,To), V, Code) => % V in [From,...,To+1] (reverse)
    for((localv(Ty, V, From), (V > To), ';', assign0(V-1)), Code).
foreach(Ty, revrangeq(From,To,Step), V, Code) => % V in [From,...,To] by Step (reverse)
    for((localv(Ty, V, From), (V >= To), ';', assign0(V-Step)), Code).

:- rprop(do_while/2, [level=grammar]).
do_while(Code, Cond) =>
    tk('do'), ' ', blk(Code), ' ', tk('while'), ' ', paren(Cond), stmtend.

:- rprop(if/2, [level=grammar]).
if(Cond, Then) =>
    tk('if'), ' ', paren(Cond), ' ', blk(Then), tk_nl.

:- rprop(if/3, [level=grammar]).
if(Cond, Then, Else) =>
    tk('if'), ' ', paren(Cond), ' ', blk(Then), ' ', tk('else'), ' ',
    ( [[ Else = if(_,_) ]] -> Else
    ; [[ Else = if(_,_,_) ]] -> Else
    ; blk(Else), tk_nl
    ).

:- rprop(switch/2, [level=grammar]).
switch(Expr, Cases) =>
    tk('switch'), ' ', paren(Expr), ' ', blk(Cases).

:- rprop(vardecl/2, [level=grammar]).
vardecl(Type, V) =>
    ( [[ Type = extern(Type0) ]] ->
        tk('extern'), ' ', vardecl(Type0, V)
    ; ty(Type), ' ', V, stmtend
    ).

:- rprop(vardecl/3, [level=grammar]).
vardecl(Type, V, A) => ty(Type), ' ', V, ' ', tk('='), ' ', A, stmtend.

:- rprop(argdecl/2, [level=grammar]).
argdecl(Type, V) => ty(Type), ' ', V.

:- rprop((<-)/2, [level=grammar]).
(A <- B) => A, ' ', tk('='), ' ', B, stmtend.

:- rprop(assign/1, [level=grammar]).
assign(X) => assign0(X), stmtend.

:- rprop(assign0/1, [level=grammar]).
assign0(X+Y), [[ Y = 1 ]] => X, tk('++').
assign0(X+Y) => X, tk('+='), Y.
assign0(X-Y), [[ Y = 1 ]] => X, tk('--').
assign0(X-Y) => X, tk('-='), Y.
assign0(X*Y) => X, tk('*='), Y.
assign0(X/\Y) => X, tk('&='), Y.

:- rprop(label/1, [level=grammar]).
label(A) => tk(A), tk(':'), tk_nl.

:- rprop(case/1, [level=grammar]).
case(A), [[ atom(A) ]] => tk('case'), ' ', tk(A), tk(':'), tk_nl.
case(A) => tk('case'), ' ', A, tk(':'), tk_nl.

:- rprop(goto/1, [level=grammar]).
goto(A) => tk('goto'), ' ', tk(A), stmtend.

:- rprop(break/0, [level=grammar]).
break => tk('break'), stmtend.

:- rprop(return/0, [level=grammar]).
return => tk('return'), stmtend.

:- rprop(return/1, [level=grammar]).
return(A) => tk('return'), ' ', A, stmtend.

:- rprop(call0/1, [level=grammar]).
call0(X) => tk(X), stmtend.

:- rprop(call/2, [level=grammar]).
callstmt(X, Args) => callexp(X, Args), stmtend.

% new id for a variable
:- rprop(var_id/1, []).
var_id(Id) => [[ newid(vr,Id) ]].

:- rprop(labeled_block/2, []).
labeled_block(Label, Code) =>
    label(Label),
    Code.

:- rprop(localv/2, []).
localv(Ty, V) => var_id(V), vardecl(Ty, V).

:- rprop(localv/3, []).
localv(Ty, V, Val) => var_id(V), vardecl(Ty, V, Val).

:- rprop(addr/1, []).
addr(X) => paren((tk('&'), X)).

:- rprop(cast/2, []).
cast(Ty,X) => paren((paren(ty(Ty)), paren(X))).

:- rprop(field/2, [level=grammar]). % (structure field initialization)
field(Name, Val) => tk('.'), tk(Name), ' ', tk('='), ' ', Val.

% TODO: (use write_c.pl operators)
% TODO: get prio of X,Y, add paren only if needed
:- rprop((+)/2, []).
X+Y => paren((X, tk('+'), Y)).
:- rprop((-)/2, []).
X-Y => paren((X, tk('-'), Y)).
:- rprop((*)/2, []).
X*Y => paren((X, tk('*'), Y)).
:- rprop(('\006\postfix_block')/2, []). % both [_] and {_}
X[Y] => X, tk('['), Y, tk(']').
:- rprop((^.)/2, []). % deref+access operator ("->" in C)
X^.Y => paren((X, tk('->'), tk(Y))).
:- rprop((^)/1, []). % deref operator ("*" in C)
X^ => tk('*'), paren((X)).
:- rprop(('\006\dot')/2, []). % TODO: functor name is not '.' here
(X.Y) => paren((X, tk('.'), tk(Y))).
:- rprop(not/1, []).
not(X) => tk('!'), paren((X)).
:- rprop((<)/2, []).
X<Y => X, tk('<'), Y.
:- rprop((>)/2, []).
X>Y => X, tk('>'), Y.
:- rprop((=<)/2, []).
X=<Y => X, tk('<='), Y.
:- rprop((>=)/2, []).
X>=Y => X, tk('>='), Y.
:- rprop((==)/2, []).
X==Y => X, tk('=='), Y.
:- rprop((\==)/2, []).
X\==Y => X, tk('!='), Y.
:- rprop((/\)/2, []).
X/\Y => paren((X, tk('&'), Y)).
:- rprop((\/)/2, []).
X\/Y => paren((X, tk('|'), Y)).
:- rprop((logical_and)/2, []).
logical_and(X,Y) => paren((X, tk('&&'), Y)).
:- rprop((logical_or)/2, []).
logical_or(X,Y) => paren((X, tk('||'), Y)).

% $emu_globals and other constants
:- rprop((~)/1, []).
~(w) => tk(w).
~(g) => tk('G').
~(s) => tk('S').
~(e) => tk('E').
~(b) => tk('B').
~(p) => tk('P').
~(null) => tk('NULL').
~(true) => tk('TRUE').
~(false) => tk('FALSE').
~(func) => tk('Func').

:- rprop(is_null/1, []).
is_null(X) => X == tk('NULL').
:- rprop(not_null/1, []).
not_null(X) => X \== tk('NULL').

:- rprop('$unreachable'/0, []).
'$unreachable' =>
    % Make sure that no mode dependant code appears next
    % TODO: better way?
    [[ update(mode('?')) ]].

% TODO: write a 'mode merge' too

call_fC(Ty,F,Args) => paren(cast(Ty,F)), callexp('',[(~w)|Args]).

cfun_eval(Name,Args) => callexp(Name, [(~w)|Args]).

cbool_succeed(Name,Args) => callexp(Name, [(~w)|Args]).

cvoid_call(Name,Args) => callstmt(Name, [(~w)|Args]).

% ---------------------------------------------------------------------------
%! # C preprocessor macros

% C preprocessor

:- rprop(cpp_define/2, [level=grammar]).
cpp_define(Name, Value) => tk('#define'), ' ', tk(Name), ' ', Value, tk_nl.

:- rprop(cpp_if_defined/1, [level=grammar]).
cpp_if_defined(Name) => tk('#if'), ' ', tk('defined'), paren(tk(Name)), tk_nl.

:- rprop(cpp_endif/0, [level=grammar]).
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
ty(ftype_ctype(f_i_signed)) => callexp('FTYPE_ctype', [tk('f_i_signed')]).
%
ty(ptr(Ty)) => ty(Ty), ' ', tk('*').

sizeof(tagged) => callexp('sizeof', [ty(tagged)]).
sizeof(sw_on_key_node) => callexp('sizeof', [ty(sw_on_key_node)]).

% ---------------------------------------------------------------------------
%! # Terms

tagp(hva,Ptr) => callexp('Tagp', [tk('HVA'),Ptr]).
tagp(sva,Ptr) => callexp('Tagp', [tk('SVA'),Ptr]).
tagp(cva,Ptr) => callexp('Tagp', [tk('CVA'),Ptr]).
tagp(str,Ptr) => callexp('Tagp', [tk('STR'),Ptr]).
tagp(lst,Ptr) => callexp('Tagp', [tk('LST'),Ptr]).

:- rprop(sw_on_heap_var/4, []).
sw_on_heap_var(Reg, HVACode, CVACode, NVACode) =>
    '{', localv(tagged, Aux),
    callstmt('SwitchOnHeapVar', [Reg, Aux, blk(HVACode), blk(CVACode), blk(NVACode)]),
    '}'.

:- rprop(sw_on_var/5, []).
sw_on_var(Reg, HVACode, CVACode, SVACode, NVACode) =>
    '{', localv(tagged, Aux),
    callstmt('SwitchOnVar', [Reg, Aux, blk(HVACode), blk(CVACode), blk(SVACode), blk(NVACode)]),
    '}'.

% TODO: deprecate
:- rprop(deref_sw/3, []).
deref_sw(Reg, Aux, VarCode) => callstmt('DerefSwitch', [Reg, Aux, blk(VarCode)]).
:- rprop(deref_sw0/2, []).
deref_sw0(Reg, VarCode) => callstmt('DerefSwitch0', [Reg, blk(VarCode)]).

unify_heap_atom(U,V) =>
    '{',
    localv(tagged, T1, V),
    sw_on_heap_var(T1,
      bind(hva, T1, U),
      bind(cva, T1, U),
      if(T1 \== U, jump_fail)),
    '}'.

unify_atom(U,V) =>
    '{',
    localv(tagged, T1, V),
    sw_on_var(T1,
      bind(hva, T1, U),
      bind(cva, T1, U),
      bind(sva, T1, U),
      if(T1 \== U, jump_fail)),
    '}'.

unify_atom_internal(Atom,Var) =>
    '{',
    localv(tagged, T1, Var),
    if(T1 /\ tk('TagBitSVA'),
      (bind(sva, T1, Atom)),
      (bind(hva, T1, Atom))),
    '}'.

:- rprop(unify_heap_structure/3, []).
unify_heap_structure(U,V,Cont) =>
    '{',
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_heap_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       if(logical_or(not(callexp('TaggedIsSTR', [T1])),
                     callexp('TaggedToHeadfunctor', [T1]) \== U), jump_fail),
       (~s) <- callexp('TaggedToArg', [T1, 1]),
       Cont)),
    '}',
    '$unreachable'.

:- rprop(unify_structure/3, []).
unify_structure(U,V,Cont) =>
    '{',
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(sva, T1, tagp(str, H)), heap_push(U),
       Cont),
      ([[ update(mode(M)) ]],
       if(logical_or(not(callexp('TaggedIsSTR', [T1])),
                     callexp('TaggedToHeadfunctor', [T1]) \== U), jump_fail),
       (~s) <- callexp('TaggedToArg', [T1, 1]),
       Cont)),
    '}',
    '$unreachable'.

unify_heap_large(P, T) =>
    '{',
    localv(tagged, T1, T),
    sw_on_heap_var(T1,
      bind(hva, T1, cfun_eval('BC_MakeBlob', [P])),
      bind(cva, T1, cfun_eval('BC_MakeBlob', [P])),
      callexp('BC_EqBlob', [T1, P, blk(jump_fail)])),
    '}'.

unify_large(P, T) =>
    '{',
    localv(tagged, T1), T1<-T,
    sw_on_var(T1,
      bind(hva, T1, cfun_eval('BC_MakeBlob', [P])),
      bind(cva, T1, cfun_eval('BC_MakeBlob', [P])),
      bind(sva, T1, cfun_eval('BC_MakeBlob', [P])),
      callexp('BC_EqBlob', [T1, P, blk(jump_fail)])),
    '}'.

:- rprop(unify_heap_list/2, []).
unify_heap_list(V,Cont) =>
    '{',
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_heap_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       if(not(callexp('TermIsLST', [T1])), jump_fail),
       (~s) <- callexp('TagpPtr', [tk('LST'), T1]),
       Cont)),
    '}',
    '$unreachable'.

:- rprop(unify_list/2, []).
unify_list(V,Cont) =>
    '{',
    localv(tagged, T1, V),
    [[ mode(M) ]],
    sw_on_var(T1,
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(hva, T1, tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(cva, T1, tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       setmode(w),
       cachedreg('H', H),
       bind(sva, T1, tagp(lst, H)),
       Cont),
      ([[ update(mode(M)) ]],
       if(not(callexp('TermIsLST', [T1])), jump_fail),
       (~s) <- callexp('TagpPtr', [tk('LST'), T1]),
       Cont)),
    '}',
    '$unreachable'.

unify_local_value(T1) =>
    if(callexp('TaggedIsSVA', [T1]),
      (localv(tagged, T0),
       do_while((
           callstmt('RefSVA', [T0,T1]),
           if(T0==T1, (
               cachedreg('H', H),
               bind(sva, T1, tagp(hva, H)),
               preload(hva, T1),
               break)),
           T1 <- T0
       ), callexp('TaggedIsSVA', [T1])))),
    heap_push(T1).

% ---------------------------------------------------------------------------
%! # Auxiliary macro definitions

% Worker state
x(Xn) => callexp('X', [Xn]).
y(Yn) => callexp('Y', [Yn]).

% Concurrency: if we cut (therefore discarding intermediate
% choicepoints), make sure we also get rid of the linked chains which
% point to the pending calls to concurrent predicates. (MCL)

% TODO: Bug: the PROFILE__HOOK_CUT should be implemented like show_nodes
%     show_nodes(w->choice, w->previous_choice);

do_cut =>
    profile_hook(cut),
    (~b) <- (~w)^.previous_choice,
    callstmt('SetChoice', [(~b)]),
    callstmt('TRACE_CHPT_CUT', [(~w)^.choice]),
    callstmt('ConcChptCleanUp', [tk('TopConcChpt'), (~w)^.choice]).

cunify(U,V) =>
    if(not(callexp('CBOOL__SUCCEED', [tk('cunify'),U,V])), jump_fail).

% This must not clobber  t2, X[*].  Build goal from Func(X(0),...X(arity-1))
emul_to_goal(Ret) => % (stores: Ret)
    if((~func)^.arity == 0,
      Ret <- (~func)^.printname,
      (cachedreg('H', H),
       Ret <- tagp(str, H),
       heap_push(callexp('SetArity', [(~func)^.printname,(~func)^.arity])),
       foreach(intmach, range((~func)^.arity), I,
               (localv(tagged, T1, x(I)),
                unify_local_value(T1)))
      )).

deallocate =>
    (~w)^.next_insn <- (~e)^.next_insn,
    (~w)^.frame <- (~e)^.frame.

code_neck =>
    if(not(callexp('IsDeep',[])),
      (do_neck,
       % OK even before allocate
       callstmt('SetE', [(~w)^.local_top]))).

code_neck_proceed =>
    if(not(callexp('IsDeep',[])),
      do_neck,
      (~w)^.local_top <- 0),
    callstmt('SetE', [(~w)^.frame]),
    (~p) <- (~w)^.next_insn,
    profile_hook(neck_proceed),
    jump_ins_dispatch.

% TODO:[oc-merge] CODE_MAYBE_NECK_TRY
do_neck => % (assume !IsDeep())
    (~b) <- (~w)^.choice,
    if(not(callexp('IsShallowTry',[])),
      % retry
      (callstmt('NECK_RETRY_PATCH', [(~b)])), % TODO:[oc-merge] this is not in OC
      % try
      ((~b)^.next_alt <- (~w)^.next_alt, %  /* 4 contiguous moves */
       (~b)^.frame <- (~w)^.frame,
       (~b)^.next_insn <- (~w)^.next_insn,
       (~b)^.local_top <- (~w)^.local_top,
       localv(intmach, I, callexp('ChoiceArity', [(~b)])),
       foreach(intmach, range(I), K, ((~b)^.x[K] <- (~w)^.x[K])),
       maybe_choice_overflow) % TODO:[oc-merge] check for choice overflow needed here?
    ),
    callstmt('SetDeep', []).

maybe_choice_overflow =>
    if(callexp('ChoiceYounger',
        [callexp('ChoiceOffset', [(~b),tk('CHOICEPAD')]),(~w)^.trail_top]),
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
dec(op(f_x,N),R) => [[ R = callexp('Xb', [N]) ]].
dec(op(f_y,N),R) => [[ R = callexp('Yb', [N]) ]].
dec(op(f_b,N),R) => [[ R = addr(N) ]]. % (a reference to the blob)
dec(op(f_g,N),R) => [[ R = addr(N) ]]. % (a reference to the blob)
dec(op(_,N),R) => [[ R = N ]].

decops(Xs) => [[ format(Fs) ]], decopsf(Fs, Xs).

% TODO: explicit format (avoid it...)
decopsf(Fs, Xs) => decops_(Fs, 0, Xs).

decops_([], _, []) => true.
decops_([f_Q|Fs], Idx, Xs) => decops_(Fs, Idx+fsize(f_Q), Xs).
decops_([F|Fs], Idx, [X|Xs]) => dec(op(F,bcp(F,Idx)),X), decops_(Fs, Idx+fsize(F), Xs).

% (see op_macros/0)
bcp(f_b,N) => callexp('BcP', [tk('f_t'),N]). % TODO: treated as f_t just for casting, better way?
bcp(f_E,N) => callexp('BcP', [tk('f_p'),N]). % TODO: treated as f_p
bcp(f_g,N) => callexp('BcP', [tk('f_l'),N]). % TODO: treated as f_l (for addr)
bcp(FType,N) => [[ Id = tk(FType) ]], callexp('BcP', [Id,N]).

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
fsize(FType) => [[ Id = tk(FType) ]], callexp('Fs',[Id]).

% ---------------------------------------------------------------------------
%! # (bytecode support)

% (sum of fsize)
fsize_sum([]) => 0.
fsize_sum([X]) => fsize(X).
fsize_sum([X|Xs]) => fsize(X), tk('+'), fsize_sum(Xs).

% Jump to a given instruction keeping the same operand stream
goto_ins(Ins) =>
    get_label(Ins, Label),
    goto(Label).

% Dispatch (using implicit format)
dispatch => [[ format(Fs) ]], dispatchf(fsize_sum(Fs)).

% Dispatch (jump to next instruction in the selected read/write
% mode). Skips OpsSize items from the operand stream.
dispatchf(OpsSize) =>
    assign((~p) + OpsSize),
    jump_ins_dispatch.

% Load/store local copies of worker registers
regload('H') => call0('LoadH').

regstore('H') => call0('StoreH').

:- rprop(cachedreg/2, []).
cachedreg('H',H), [[ mode(r) ]] => [[ H = (~w)^.heap_top ]].
cachedreg('H',H), [[ mode(w) ]] => [[ H = tk('H') ]].

% Switch the read/write mode
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

put_yvoid =>
    '{',
    localv(tagged, T0, bcp(f_y,0)),
    shiftf(f_y),
    dec(op(f_y,T0),Y),
    load(sva, Y),
    '}'.
    
heap_push(X) =>
    cachedreg('H', H),
    callstmt('HeapPush', [H, X]).

% TODO: expression vs statement
unsafe_var_expr(X) => not(callexp('YoungerStackVar', [tagp(sva,callexp('Offset',[(~e),tk('EToY0')])), X])).

ref_stack_unsafe(To,From) =>
    '{',
    localv(tagged, T0),
    localv(tagged, T1),
    T0 <- From,
    if(callexp('TaggedIsSVA', [T0]),
      do_while((
          callstmt('RefSVA', [T1,T0]),
          if(T1==T0, (
              if(unsafe_var_expr(T0),
                 (load(hva, T0),
                  bind(sva, T1, T0))),
              break)
          ),
          T0 <- T1
      ), callexp('TaggedIsSVA', [T0]))),
    To <- T0,
    '}'.

ref_heap_next(A) => callstmt('RefHeapNext', [A, (~s)]).

preload(hva, A) =>
    cachedreg('H', H),
    callstmt('PreLoadHVA', [A, H]).

load2(hva, A, B) =>
    cachedreg('H', H),
    callstmt('Load2HVA', [A, B, H]).
load2(sva, A, B) =>
    callstmt('Load2SVA', [A, B]).

load(hva, A) =>
    cachedreg('H', H),
    callstmt('LoadHVA', [A, H]).
load(sva, A) =>
    callstmt('LoadSVA', [A]).
load(cva, A) =>
    cachedreg('H', H),
    callstmt('LoadCVA', [A, H]).

bind(hva, T0, T1) => callstmt('BindHVA', [T0,T1]).
bind(cva, T0, T1) => callstmt('BindCVA', [T0,T1]).
bind(sva, T0, T1) => callstmt('BindSVA', [T0,T1]).

trail_if_conditional_sva(U) =>
    if(callexp('CondStackvar', [U]), (
        callstmt('TrailPushCheck', [(~w)^.trail_top,tagp(sva, addr(U))])
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
    (~s) <- callexp('HeapOffset', [(~s), X]).
un_voidr(X), [[ mode(w) ]] =>
    '{',
    localv(intmach, I, cast(ftype_ctype(f_i_signed), X)),
    do_while(callexp('ConstrHVA', [tk('H')]), (tk('--'),I)),
    '}'.

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

alloc => callstmt('CODE_ALLOC', [(~e)]).
    
% Emit the initialization of Y variables
init_yvars(Count) =>
    foreach(intmach, revrangeq(Count-sizeof(tagged), tk('EToY0')*sizeof(tagged), sizeof(tagged)), T, (
        dec(op(f_y,T),Y),
        load(sva,Y)
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
get_atom([], X) => [[ X = tk('atom_nil') ]].

% ---------------------------------------------------------------------------
%! # Definition of the instruction set

inittrue => decops([N]),
    alloc,
    init_yvars(N),
    goto('firsttrue').

firsttrue_n => decopsf([f_i],[N]),
    '{',
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I), _I0, put_yvoid),
    '}',
    goto('firsttrue'),
    label('firsttrue'),
    decopsf([f_e],[EnvSize]),
    (~e)^.next_insn <- (~w)^.next_insn,
    (~e)^.frame <- (~w)^.frame,
    (~w)^.frame <- (~e),
    % (~w)^.next_insn <- callexp('PoffR', [2]), % (before)
    (~w)^.next_insn <- callexp('BCoff', [(~p), fsize_sum([f_e])]),
    (~w)^.local_top <- callexp('StackCharOffset', [(~e),EnvSize]),
    if(callexp('OffStacktop',[(~e),tk('Stack_Warn')]), callstmt('SetEvent', [])),
    dispatchf(fsize_sum([f_e])). % (was f_i before)

initcall => decops([_,N]),
    alloc,
    init_yvars(N),
    goto_ins(firstcall).

firstcall_n => decopsf([f_i],[N]),
    '{',
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,8), _I0, put_yvoid),
    '}',
    goto_ins(firstcall_8).

firstcall_8 => put_yvoid, goto_ins(firstcall_7).
firstcall_7 => put_yvoid, goto_ins(firstcall_6).
firstcall_6 => put_yvoid, goto_ins(firstcall_5).
firstcall_5 => put_yvoid, goto_ins(firstcall_4).
firstcall_4 => put_yvoid, goto_ins(firstcall_3).
firstcall_3 => put_yvoid, goto_ins(firstcall_2).
firstcall_2 => put_yvoid, goto_ins(firstcall_1).
firstcall_1 => put_yvoid, goto_ins(firstcall).
firstcall => decops([PredPtr,EnvSize]),
    (~e)^.next_insn <- (~w)^.next_insn,
    (~e)^.frame <- (~w)^.frame,
    (~w)^.frame <- (~e),
    (~w)^.next_insn <- callexp('BCoff', [(~p), fsize_sum([f_E,f_e])]),
    (~w)^.local_top <- callexp('StackCharOffset', [(~e),EnvSize]),
    (~p) <- PredPtr,
    if(callexp('OffStacktop',[(~e),tk('Stack_Warn')]), callstmt('SetEvent', [])),
    goto('enter_predicate').

putarg_z_shift(Xn) =>
    '{',
    localv(tagged, T1, bcp(f_z,0)),
    shiftf(f_z),
    putarg(T1,Xn),
    '}'.

call_n => decopsf([f_i],[N]),
    '{',
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,8), I0, putarg_z_shift(I0-1)),
    '}',
    goto_ins(call_8).

call_8 => putarg_z_shift(7), goto_ins(call_7).
call_7 => putarg_z_shift(6), goto_ins(call_6).
call_6 => putarg_z_shift(5), goto_ins(call_5).
call_5 => putarg_z_shift(4), goto_ins(call_4).
call_4 => putarg_z_shift(3), goto_ins(call_3).
call_3 => putarg_z_shift(2), goto_ins(call_2).
call_2 => putarg_z_shift(1), goto_ins(call_1).
call_1 => putarg_z_shift(0), goto_ins(call).
call => decops([Pred,_]),
    (~w)^.next_insn <- callexp('BCoff', [(~p), fsize_sum([f_E,f_e])]),
    (~p) <- Pred,
    goto('enter_predicate').

lastcall_n => decopsf([f_i],[N]),
    '{',
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,8), I0, putarg_z_shift(I0-1)),
    '}',
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

executeq => decops([Pred]),
    setmode(w),
    (~p) <- Pred,
    goto('enter_predicate').
execute => decops([Pred]),
    setmode(w),
    (~p) <- Pred,
    goto('enter_predicate').

put_x_void => decops([X]),
    load(hva,X),
    dispatch.

put_x_variable => decops([A,B]),
    load2(hva, A, B),
    dispatch.

put_xval_xval => decops([A,B,C,D]),
    A <- B,
    C <- D,
    dispatch.

put_x_value => decops([A,B]),
    A <- B,
    dispatch.

put_x_unsafe_value => decops([A,B]),
    '{',
    localv(tagged, T0), ref_stack_unsafe(T0,B),
    A <- T0,
    B <- T0,
    '}',
    dispatch.

put_y_first_variable =>
    alloc,
    goto_ins(put_y_variable).

put_y_variable => decops([A,B]),
    load2(sva, A, B),
    dispatch.

put_yfvar_yvar =>
    alloc,
    goto_ins(put_yvar_yvar).

put_yvar_yvar => decops([A,B,C,D]),
    load2(sva, A, B),
    load2(sva, C, D),
    dispatch.

put_yval_yval => decops([A,B,C,D]),
    A <- B,
    C <- D,
    dispatch.

put_y_value => decops([A,B]),
    A <- B,
    dispatch.

put_y_unsafe_value => decops([A,B]),
    ref_stack_unsafe(A,B),
    dispatch.

put_constantq => decops([A,B]),
    A <- B,
    dispatch.
put_constant => decops([A,B]),
    A <- B,
    dispatch.

put_nil => decops([A]),
    get_atom([], Nil),
    A <- Nil,
    dispatch.

put_largeq => decops([A,B]),
    [[ mode(M) ]],
    setmode(r),
    A <- cfun_eval('BC_MakeBlob', [B]),
    setmode(M),
    dispatchf(fsize_sum([f_Q,f_x])+callexp('LargeSize',[B^])).
put_large => decops([A,B]),
    [[ mode(M) ]],
    setmode(r),
    A <- cfun_eval('BC_MakeBlob', [B]),
    setmode(M),
    dispatchf(fsize_sum([f_x])+callexp('LargeSize',[B^])).

put_structureq => decops([A,B]),
    cachedreg('H', H),
    A <- tagp(str, H),
    heap_push(B),
    dispatch.
put_structure => decops([A,B]),
    cachedreg('H', H),
    A <- tagp(str, H),
    heap_push(B),
    dispatch.

put_list => decops([A]),
    cachedreg('H', H),
    A <- tagp(lst, H),
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

get_x_value => decops([A,B]),
    u_val(A,B), % TODO:[oc-merge] was reversed before oc merge
    dispatch.

get_y_first_value => decops([A,B]),
    u_fval(A,B),
    dispatch.

get_y_value => decops([A,B]),
    u_val(A,B),
    dispatch.

get_constant => decops([A,B]),
    unify_atom(B,A),
    dispatch.

get_large => decops([A,B]),
    unify_large(B,A),
    dispatchf(fsize_sum([f_x])+callexp('LargeSize',[B^])).

get_structure => decops([A,B]),
    unify_structure(B,A,dispatch).

get_nil => decops([A]),
    get_atom([], Nil),
    unify_atom(Nil,A),
    dispatch.

get_list => decops([A]),
    unify_list(A, dispatch).

get_constant_neck_proceed => decops([A,B]),
    unify_atom(B,A),
    setmode(w),
    goto_ins(neck_proceed).

get_nil_neck_proceed => decops([A]),
    get_atom([], Nil),
    unify_atom(Nil,A),
    setmode(w),
    goto_ins(neck_proceed).

cutb_x => decops([A]),
    (~w)^.local_top <- 0, % may get hole at top of local stack
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [A]),
    do_cut,
    dispatch.

cutb_x_neck => decops([A]),
    (~w)^.local_top <- 0, % may get hole at top of local stack
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [A]),
    shiftf,
    goto_ins(cutb_neck).

cutb_neck =>
    do_cutb_neck,
    dispatch.

cutb_x_neck_proceed => decops([A]),
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [A]),
    shiftf_nodec,
    % w->local_top <- 0 % done by CODE_PROCEED
    goto_ins(cutb_neck_proceed).

cutb_neck_proceed =>
    do_cutb_neck,
    goto_ins(proceed).

do_cutb_neck =>
    do_cut,
    if(not(callexp('IsDeep',[])),
      (callstmt('SetDeep', []),
       % TODO:[merge-oc] if neck is not pending, then choice overflow has already been checked?
       maybe_choice_overflow)).

cute_x => decops([A]),
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [A]),
    (~w)^.local_top <- (~e), % w->local_top may be 0 here
    do_cut,
    callstmt('SetE', [(~w)^.local_top]),
    dispatch.

cute_x_neck => decops([A]),
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [A]),
    shiftf,
    goto_ins(cute_neck).

cute_neck =>
    (~w)^.local_top <- (~e), %  w->local_top may be 0 here.
    do_cut,
    % w->next_alt can't be NULL here
    callstmt('SetDeep', []),
    if(callexp('ChoiceYounger', [callexp('ChoiceOffset', [(~b),tk('CHOICEPAD')]),(~w)^.trail_top]),
      cvoid_call('choice_overflow', [2*tk('CHOICEPAD')*sizeof(tagged),~true])),
    callstmt('SetE', [(~w)^.local_top]),
    dispatch.

cutf_x => decops([A]),
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [A]),
    shiftf,
    goto_ins(cutf). % TODO: check that pending 'format' after shift is the expected one

cutf =>
    do_cut,
    callstmt('SetE', [(~w)^.frame]),
    dispatch.

cut_y => decops([A]),
    '{',
    localv(tagged, T1), T1 <- A,
    (~w)^.previous_choice <- callexp('ChoiceFromTagged', [T1]),
    '}',
    do_cut,
    callstmt('SetE', [(~w)^.frame]),
    dispatch.

choice_x => decops([X]),
    X <- callexp('ChoiceToTagged', [(~w)^.previous_choice]),
    dispatch.

choice_yf =>
    alloc,
    goto_ins(choice_y).

choice_y => decops([Y]),
    Y <- callexp('ChoiceToTagged', [(~w)^.previous_choice]),
    dispatch.

kontinue =>
    % after wakeup, write mode!
    callstmt('Setfunc', [callexp('TaggedToFunctor', [y(0)])]),
    foreach(intmach, range((~func)^.arity), I, (x(I) <- y(I+1))),
    deallocate,
    goto('enter_predicate').

leave => goto_ins(exit_toplevel).

exit_toplevel =>
    goto('exit_toplevel').

retry_cq => decops([A]),
    if(not(callexp('IsDeep',[])),
      (callstmt('NECK_RETRY_PATCH', [(~b)]),
       callstmt('SetDeep', []))),
    if(not(call_fC(cbool0,A,[])), jump_fail),
    goto_ins(proceed).
retry_c => decops([A]),
    if(not(callexp('IsDeep',[])),
      (callstmt('NECK_RETRY_PATCH', [(~b)]),
       callstmt('SetDeep', []))),
    if(not(call_fC(cbool0,A,[])), jump_fail),
    goto_ins(proceed).

% _x0 instructions, where read-mode match has been done during indexing

get_structure_x0, [[ mode(r) ]] =>
    '{',
    localv(tagged, T0, x(0)),
    (~s) <- callexp('TaggedToArg', [T0, 1]),
    '}',
    dispatch.
get_structure_x0, [[ mode(w) ]] => decops([A]),
    '{',
    cachedreg('H', H),
    localv(tagged, T1, tagp(str, H)),
    localv(tagged, T0, x(0)),
    if(callexp('TaggedIsHVA', [T0]),
      bind(hva,T0,T1),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,T1),
        bind(cva,T0,T1))),
    heap_push(A),
    '}',
    dispatch.

get_large_x0, [[ mode(r) ]] => decops([A]),
    '{',
    localv(tagged, T0, x(0)),
    unify_large(A,T0),
    '}',
    ( [[ format([f_Q|_]) ]] -> % TODO: be careful with dispatchf
        dispatchf(fsize_sum([f_Q])+callexp('LargeSize',[A^]))
    ; dispatchf(callexp('LargeSize',[A^]))
    ).
%    dispatchf(callexp('LargeSize',[A^])).
get_large_x0, [[ mode(w) ]] => decops([A]),
    '{',
    setmode(r),
    localv(tagged, T1, cfun_eval('BC_MakeBlob', [A])),
    setmode(w),
    localv(tagged, T0, x(0)),
    if(callexp('TaggedIsHVA', [T0]),
      bind(hva,T0,T1),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,T1),
        bind(cva,T0,T1))),
    '}',
    dispatchf(callexp('LargeSize',[A^])).

get_constant_x0, [[ mode(r) ]] => dispatch.
get_constant_x0, [[ mode(w) ]] => decops([A]),
    '{',
    localv(tagged, T0, x(0)),
    if(callexp('TaggedIsHVA', [T0]),
      bind(hva,T0,A),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,A),
        bind(cva,T0,A))),
    '}',
    dispatch.

get_nil_x0, [[ mode(r) ]] =>
    dispatch.
get_nil_x0, [[ mode(w) ]] =>
    '{',
    localv(tagged, T0, x(0)),
    get_atom([], Nil),
    if(callexp('TaggedIsHVA', [T0]),
      bind(hva,T0,Nil),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,Nil),
        bind(cva,T0,Nil))),
    '}',
    dispatch.

get_list_x0, [[ mode(r) ]] =>
    '{',
    localv(tagged, T0, x(0)),
    (~s) <- callexp('TagpPtr', [tk('LST'), T0]),
    '}',
    dispatch.
get_list_x0, [[ mode(w) ]] =>
    '{',
    cachedreg('H', H),
    localv(tagged, T1, tagp(lst, H)),
    localv(tagged, T0, x(0)),
    if(callexp('TaggedIsHVA', [T0]),
      bind(hva,T0,T1),
      if(T0 /\ tk('TagBitSVA'),
        bind(sva,T0,T1),
        bind(cva,T0,T1))),
    '}',
    dispatch.

get_xvar_xvar => decops([A,B,C,D]),
    B <- A,
    D <- C,
    dispatch.

get_x_variable => decops([A,B]),
    B <- A,
    dispatch.

get_y_first_variable =>
    alloc,
    goto_ins(get_y_variable).

get_y_variable => decops([A,B]),
    B <- A,
    dispatch.

get_yfvar_yvar =>
    alloc,
    goto_ins(get_yvar_yvar).

get_yvar_yvar => decops([A,B,C,D]),
    B <- A,
    D <- C,
    dispatch.

branch => decops([Addr]),
    (~p) <- callexp('BCoff', [(~p), Addr]),
    jump_ins_dispatch.

% Call Expr function returning a tagged, goto fail on ERRORTAG
cfun_semidet(Target, Expr) =>
    '{',
    localv(tagged, Res, cast(tagged, Expr)),
    Target <- Res,
    if(tk('ERRORTAG')==Res, jump_fail),
    '}'.

cblt_semidet(Expr) => if(not(Expr), jump_fail).

function_1q => decops([A,B,C,Li]),
    (~w)^.liveinfo <- Li,
    cfun_semidet(A, call_fC(ctagged1, C, [B])),
    dispatch.
function_1 => decops([A,B,C,Li]),
    (~w)^.liveinfo <- Li,
    cfun_semidet(A, call_fC(ctagged1, C, [B])),
    dispatch.

function_2q => decops([A,B,C,D,Li]),
    (~w)^.liveinfo <- Li,
    cfun_semidet(A, call_fC(ctagged2, D, [B,C])),
    dispatch.
function_2 => decops([A,B,C,D,Li]),
    (~w)^.liveinfo <- Li,
    cfun_semidet(A, call_fC(ctagged2, D, [B,C])),
    dispatch.

builtin_1q => decops([A,B]),
    cblt_semidet(call_fC(cbool1,B,[A])),
    dispatch.
builtin_1 => decops([A,B]),
    cblt_semidet(call_fC(cbool1,B,[A])),
    dispatch.

builtin_2q => decops([A,B,C]),
    cblt_semidet(call_fC(cbool2,C,[A,B])),
    dispatch.
builtin_2 => decops([A,B,C]),
    cblt_semidet(call_fC(cbool2,C,[A,B])),
    dispatch.

builtin_3q => decops([A,B,C,D]),
    cblt_semidet(call_fC(cbool3,D,[A,B,C])),
    dispatch.
builtin_3 => decops([A,B,C,D]),
    cblt_semidet(call_fC(cbool3,D,[A,B,C])),
    dispatch.

% backtracking into clause/2
retry_instance =>
    % Take into account 'open' predicates.  (MCL)
    % If there is *definitely* no next instance, remove choicepoint
    if(logical_or(logical_and(callexp('TaggedToRoot',[x(tk('RootArg'))])^.behavior_on_failure \== tk('DYNAMIC'),
                              % Wait and removes handle if needed
                              not(cbool_succeed('next_instance_conc', [addr((~w)^.misc^.ins)]))),
                  logical_and(callexp('TaggedToRoot',[x(tk('RootArg'))])^.behavior_on_failure == tk('DYNAMIC'),
                              not(cbool_succeed('next_instance', [addr((~w)^.misc^.ins)])))), (
        callstmt('SetDeep', []),
        (~b) <- (~w)^.previous_choice,
        callstmt('SetChoice', [(~b)])
    )),
    if(is_null((~w)^.misc^.ins),
      % A conc. predicate has been closed, or a non-blocking call was made (MCL)
      (trace(retry_instance_debug_1),
       tk('TopConcChpt') <- callexp('TermToPointerOrNull', [ty(choice), x(tk('PrevDynChpt'))]),
       trace(retry_instance_debug_2),
       % But fail anyway
       jump_fail)),
    trace(retry_instance_debug_3),
    (~p) <- cast(bcp, (~w)^.misc^.ins^.emulcode),
    jump_ins_dispatch.

get_constraint => decops([A]),
    '{', 
    localv(tagged, T1, A),
    localv(tagged, T2), load(cva,T2),
    sw_on_var(T1,
      (bind(hva,T1,T2), A <- T2),
      bind(cva,T2,T1),
      (bind(sva,T1,T2), A <- T2),
      bind(cva,T2,T1)),
    '}',
    dispatch.

unify_void, [[ mode(r) ]] => decops([N]),
    un_voidr(N),
    dispatch.
unify_void, [[ mode(w) ]] => decops([N]),
    '{',
    localv(intmach, I, cast(ftype_ctype(f_i_signed), N)),
    shiftf(f_i),
    foreach(intmach, revrange(I,4), _I0,
      (cachedreg('H', H),
       callstmt('ConstrHVA', [H]))),
    '}',
    goto_ins(unify_void_4).

unify_void_1, [[ mode(r) ]] =>
    un_voidr(1),
    dispatch.
unify_void_1, [[ mode(w) ]] =>
    cachedreg('H', H),
    callstmt('ConstrHVA', [H]),
    dispatch.

unify_void_2, [[ mode(r) ]] =>
    un_voidr(2),
    dispatch.
unify_void_2, [[ mode(w) ]] =>
    cachedreg('H', H),
    callstmt('ConstrHVA', [H]),
    goto_ins(unify_void_1).

unify_void_3, [[ mode(r) ]] =>
    un_voidr(3),
    dispatch.
unify_void_3, [[ mode(w) ]] =>
    cachedreg('H', H),
    callstmt('ConstrHVA', [H]),
    goto_ins(unify_void_2).

unify_void_4, [[ mode(r) ]] =>
    un_voidr(4),
    dispatch.
unify_void_4, [[ mode(w) ]] =>
    cachedreg('H', H),
    callstmt('ConstrHVA', [H]),
    goto_ins(unify_void_3).

unify_x_variable => decops([A]),
    un_var(A),
    dispatch.

unify_x_value => decops([A]),
    un_val(A),
    dispatch.

unify_x_local_value => decops([A]),
    un_lval(A),
    dispatch.

unify_y_first_variable =>
    alloc,
    goto_ins(unify_y_variable).

unify_y_variable => decops([A]),
    un_var(A),
    dispatch.

unify_y_first_value => decops([A]),
    un_fval(A),
    dispatch.

unify_y_value => decops([A]),
    un_val(A),
    dispatch.

unify_y_local_value => decops([A]),
    un_lval(A),
    dispatch.

unify_constant, [[ mode(r) ]] => decops([A]),
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_atom(A,T1),
    '}',
    dispatch.
unify_constant, [[ mode(w) ]] => decops([A]),
    heap_push(A),
    dispatch.

unify_large, [[ mode(r) ]] => decops([A]),
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_large(A,T1),
    '}',
    dispatchf(callexp('LargeSize',[A^])).
unify_large, [[ mode(w) ]] => decops([A]),
    % TODO: try to switch to r mode properly (this code is tricky)
    % (this is 'heap_push and switch to read')
    cachedreg('H', H),
    (~w)^.heap_top <- callexp('HeapOffset', [H,1]),
    H^ <- cfun_eval('BC_MakeBlob', [A]),
    [[ update(mode(r)) ]],
    dispatchf(callexp('LargeSize',[A^])).

unify_structure, [[ mode(r) ]] => decops([A]),
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_structure(A,T1,dispatch),
    '}'.
unify_structure, [[ mode(w) ]] => decops([A]),
    cachedreg('H', H),
    heap_push(tagp(str,callexp('HeapOffset', [H,1]))),
    heap_push(A),
    dispatch.

unify_nil, [[ mode(r) ]] =>
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    get_atom([], Nil),
    unify_heap_atom(Nil, T1),
    '}',
    dispatch.
unify_nil, [[ mode(w) ]] =>
    get_atom([], Nil),
    heap_push(Nil),
    dispatch.

unify_list, [[ mode(r) ]] =>
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_list(T1,dispatch),
    '}'.
unify_list, [[ mode(w) ]] =>
    cachedreg('H', H),
    heap_push(tagp(lst,callexp('HeapOffset', [H,1]))),
    dispatch.

unify_constant_neck_proceed, [[ mode(r) ]] => decops([A]),
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    unify_heap_atom(A,T1),
    '}',
    setmode(w),
    goto_ins(neck_proceed).
unify_constant_neck_proceed, [[ mode(w) ]] => decops([A]),
    heap_push(A),
    goto_ins(neck_proceed).

unify_nil_neck_proceed, [[ mode(r) ]] =>
    '{',
    localv(tagged, T1), ref_heap_next(T1),
    get_atom([], Nil),
    unify_heap_atom(Nil, T1),
    '}',
    setmode(w),
    goto_ins(neck_proceed).
unify_nil_neck_proceed, [[ mode(w) ]] =>
    get_atom([], Nil),
    heap_push(Nil),
    goto_ins(neck_proceed).

u2_void_xvar => decops([N,B]),
    un_voidr(N),
    un_var(B),
    dispatch.

u2_void_yfvar =>
    alloc,
    goto_ins(u2_void_yvar).

u2_void_yvar => decops([N,B]),
    un_voidr(N),
    un_var(B),
    dispatch.

u2_void_xval => decops([N,B]),
    un_voidr(N),
    un_val(B),
    dispatch.

u2_void_xlval => decops([N,B]),
    un_voidr(N),
    un_lval(B),
    dispatch.

u2_void_yfval => decops([N,B]),
    un_voidr(N),
    un_fval(B),
    dispatch.

u2_void_yval => decops([N,B]),
    un_voidr(N),
    un_val(B),
    dispatch.

u2_void_ylval => decops([N,B]),
    un_voidr(N),
    un_lval(B),
    dispatch.

u2_xvar_void => decops([A,N]),
    un_var(A),
    un_voidr(N),
    dispatch.

u2_xvar_xvar => decops([A,B]),
    un_var(A),
    un_var(B),
    dispatch.

u2_xvar_yfvar =>
    alloc,
    goto_ins(u2_xvar_yvar).

u2_xvar_yvar => decops([A,B]),
    un_var(A),
    un_var(B),
    dispatch.

u2_xvar_xval => decops([A,B]),
    un_var(A),
    un_val(B),
    dispatch.

u2_xvar_xlval => decops([A,B]),
    un_var(A),
    un_lval(B),
    dispatch.

u2_xvar_yfval => decops([A,B]),
    un_var(A),
    un_fval(B),
    dispatch.

u2_xvar_yval => decops([A,B]),
    un_var(A),
    un_val(B),
    dispatch.

u2_xvar_ylval => decops([A,B]),
    un_var(A),
    un_lval(B),
    dispatch.

u2_yfvar_void =>
    alloc,
    goto_ins(u2_yvar_void).

u2_yvar_void => decops([A,N]),
    un_var(A),
    un_voidr(N),
    dispatch.

u2_yfvar_xvar =>
    alloc,
    goto_ins(u2_yvar_xvar).

u2_yvar_xvar => decops([A,B]),
    un_var(A),
    un_var(B),
    dispatch.

u2_yfvar_yvar =>
    alloc,
    goto_ins(u2_yvar_yvar).

u2_yvar_yvar => decops([A,B]),
    un_var(A),
    un_var(B),
    dispatch.

u2_yfvar_xval =>
    alloc,
    goto_ins(u2_yvar_xval).

u2_yfvar_xlval =>
    alloc,
    goto_ins(u2_yvar_xlval).

u2_yvar_xval => decops([A,B]),
    un_var(A),
    un_val(B),
    dispatch.

u2_yvar_xlval => decops([A,B]),
    un_var(A),
    un_lval(B),
    dispatch.

u2_yfvar_yval =>
    alloc,
    goto_ins(u2_yvar_yval).

u2_yfvar_ylval =>
    alloc,
    goto_ins(u2_yvar_ylval).

u2_yvar_yval => decops([A,B]),
    un_var(A),
    un_val(B),
    dispatch.

u2_yvar_ylval => decops([A,B]),
    un_var(A),
    un_lval(B),
    dispatch.

u2_yfval_void => decops([A,N]),
    un_fval(A),
    un_voidr(N),
    dispatch.

u2_yfval_xvar => decops([A,B]),
    un_fval(A),
    un_var(B),
    dispatch.

u2_yfval_yfval => decops([A,B]),
    un_fval(A),
    un_fval(B),
    dispatch.

u2_yfval_xval => decops([A,B]),
    un_fval(A),
    un_val(B),
    dispatch.

u2_yfval_xlval => decops([A,B]),
    un_fval(A),
    un_lval(B),
    dispatch.

u2_yfval_yval => decops([A,B]),
    un_fval(A),
    un_val(B),
    dispatch.

u2_yfval_ylval => decops([A,B]),
    un_fval(A),
    un_lval(B),
    dispatch.

u2_xval_void => decops([A,N]),
    un_val(A),
    un_voidr(N),
    dispatch.

u2_xlval_void => decops([A,N]),
    un_lval(A),
    un_voidr(N),
    dispatch.

u2_xval_xvar => decops([A,B]),
    un_val(A),
    un_var(B),
    dispatch.

u2_xlval_xvar => decops([A,B]),
    un_lval(A),
    un_var(B),
    dispatch.

u2_xval_yfvar =>
    alloc,
    goto_ins(u2_xval_yvar).

u2_xlval_yfvar =>
    alloc,
    goto_ins(u2_xlval_yvar).

u2_xval_yvar => decops([A,B]),
    un_val(A),
    un_var(B),
    dispatch.

u2_xlval_yvar => decops([A,B]),
    un_lval(A),
    un_var(B),
    dispatch.

u2_xval_xval => decops([A,B]),
    un_val(A),
    un_val(B),
    dispatch.

u2_xval_xlval => decops([A,B]),
    un_val(A),
    un_lval(B),
    dispatch.

u2_xlval_xval => decops([A,B]),
    un_lval(A),
    un_val(B),
    dispatch.

u2_xlval_xlval => decops([A,B]),
    un_lval(A),
    un_lval(B),
    dispatch.

u2_xval_yfval => decops([A,B]),
    un_val(A),
    un_fval(B),
    dispatch.

u2_xlval_yfval => decops([A,B]),
    un_lval(A),
    un_fval(B),
    dispatch.

u2_xval_yval => decops([A,B]),
    un_val(A),
    un_val(B),
    dispatch.

u2_xval_ylval => decops([A,B]),
    un_val(A),
    un_lval(B),
    dispatch.

u2_xlval_yval => decops([A,B]),
    un_lval(A),
    un_val(B),
    dispatch.

u2_xlval_ylval => decops([A,B]),
    un_lval(A),
    un_lval(B),
    dispatch.

u2_yval_void => decops([A,N]),
    un_val(A),
    un_voidr(N),
    dispatch.

u2_ylval_void => decops([A,N]),
    un_lval(A),
    un_voidr(N),
    dispatch.

u2_yval_xvar => decops([A,B]),
    un_val(A),
    un_var(B),
    dispatch.

u2_ylval_xvar => decops([A,B]),
    un_lval(A),
    un_var(B),
    dispatch.

u2_yval_yvar => decops([A,B]),
    un_val(A),
    un_var(B),
    dispatch.

u2_ylval_yvar => decops([A,B]),
    un_lval(A),
    un_var(B),
    dispatch.

u2_yval_yfval => decops([A,B]),
    un_val(A),
    un_fval(B),
    dispatch.

u2_ylval_yfval => decops([A,B]),
    un_lval(A),
    un_fval(B),
    dispatch.

u2_yval_xval => decops([A,B]),
    un_val(A),
    un_val(B),
    dispatch.

u2_yval_xlval => decops([A,B]),
    un_val(A),
    un_lval(B),
    dispatch.

u2_ylval_xval => decops([A,B]),
    un_lval(A),
    un_val(B),
    dispatch.

u2_ylval_xlval => decops([A,B]),
    un_lval(A),
    un_lval(B),
    dispatch.

u2_yval_yval => decops([A,B]),
    un_val(A),
    un_val(B),
    dispatch.

u2_yval_ylval => decops([A,B]),
    un_val(A),
    un_lval(B),
    dispatch.

u2_ylval_yval => decops([A,B]),
    un_lval(A),
    un_val(B),
    dispatch.

u2_ylval_ylval => decops([A,B]),
    un_lval(A),
    un_lval(B),
    dispatch.

bump_counter => decops([A]),
    gauge_incr_counter(A),
    dispatch.

counted_neck => decops([A,B]),
    cpp_if_defined('GAUGE'),
    if(not(callexp('IsDeep',[])), (
      (~b) <- (~w)^.choice,
      if(not(callexp('IsShallowTry',[])), (
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
    if(callexp('HeapCharDifference', [H, tk('Heap_End')]) < cast(intmach,A),
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
    unify_atom_internal(callexp('PointerToTerm',[(~w)^.misc^.ins]),x(3)),
    if(callexp('IsDeep',[]), goto_ins(proceed)),
    (~b) <- (~w)^.choice,
    % (assume w->next_alt != NULL)
    if(callexp('IsShallowTry', []),(
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
    callstmt('SetE', [(~w)^.frame]),
    (~p) <- (~w)^.next_insn,
    profile_hook(proceed),
    dispatch.

% TODO: this a new instruction really needed here? consider special builtin functions
restart_point =>
    setmode_setH(r, callexp('TaggedToPointer', [(~w)^.choice^.x[0]])),
    setmode(w),
    (~p) <- cast(bcp, callexp('TaggedToPointer',[(~w)^.choice^.x[0]])^),
    (~w)^.next_insn <- (~w)^.choice^.next_insn,
    cvoid_call('pop_choicept', []),
    goto('enter_predicate').

% ---------------------------------------------------------------------------
%! # WAM execution tracing

:- rprop(pred_trace/1, []).
pred_trace(Kind) =>
    callstmt('PredTrace', [Kind, (~func)]).

:- rprop(trace/1, []).
trace(X) => callstmt('ON_DEBUG', [blk(trace_(X))]).

trace_print(Args) => callstmt('fprintf', [tk('stderr')|Args]).

:- rprop(trace_/1, []).
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
      if(logical_and(callexp('TaggedToRoot', [x(tk('RootArg'))])^.behavior_on_failure \== tk('CONC_CLOSED'),
                     callexp('IS_BLOCKING', [x(tk('InvocationAttr'))])),
         trace_print([tk_string("**wam(): failing on a concurrent closed pred, chpt=%p, failing chpt=%p .\n"),
                      (~w)^.choice,
                      tk('TopConcChpt')]))),
    if(tk('debug_conc'),
      if(logical_or(callexp('TaggedToRoot', [x(tk('RootArg'))])^.x2_pending_on_instance,
                    callexp('TaggedToRoot', [x(tk('RootArg'))])^.x5_pending_on_instance),
         trace_print([tk_string("**wam(): failing with invokations pending from root, type = %d.\n"),
                      callexp('TaggedToRoot', [x(tk('RootArg'))])^.behavior_on_failure]))).
trace_(retry_instance_debug_2) =>
    if(tk('debug_concchoicepoints'),
       trace_print([tk_string("New topmost concurrent chpt = %x\n"), tk('TopConcChpt')])).
trace_(retry_instance_debug_3) =>
    if(logical_and(tk('debug_conc'),
                   callexp('TaggedToRoot', [x(tk('RootArg'))])^.behavior_on_failure \== tk('DYNAMIC')),
       trace_print([(tk_string("*** "), ' ', tk('PRIdm'), ' ', tk_string("backtracking on a concurrent predicate.\n")),
                    cast(intmach,tk('Thread_Id')),
                    cast(intmach,tk('GET_INC_COUNTER'))])),
    if(logical_and(tk('debug_concchoicepoints'),
                   callexp('TaggedToRoot', [x(tk('RootArg'))])^.behavior_on_failure \== tk('DYNAMIC')),
       trace_print([tk_string("backtracking to chpt. = %p\n"), (~w)^.choice])).

% ---------------------------------------------------------------------------
%! # WAM profiling

:- rprop(profile_hook/1, []).
profile_hook(cut) => call0('PROFILE__HOOK_CUT').
profile_hook(proceed) => call0('PROFILE__HOOK_PROCEED').
profile_hook(neck_proceed) => call0('PROFILE__HOOK_NECK_PROCEED').
profile_hook(fail) => call0('PROFILE__HOOK_FAIL').
profile_hook(redo) => call0('PROFILE__HOOK_REDO').

% ---------------------------------------------------------------------------
%! # Gauge (profiling counters)

gauge_incr_counter(Counter) =>
    cpp_if_defined('GAUGE'),
    callstmt('INCR_COUNTER', [Counter]),
    cpp_endif.

% ---------------------------------------------------------------------------
%! # Entries to generate other engine support files

% IDs for exported instructions (names visible from C code)
:- rprop(all_ins_op/0, []).
all_ins_op =>
    autogen_warning_comment,
    %
    [[ collect_and_filter(instruction_set, '$exported_ins'/3, Insns) ]],
    '$foreach'(Insns, ins_op).

ins_op('$exported_ins'(Op, Ins, Name)), [[ get(op(Op).optional, Flag) ]] =>
    cpp_if_defined(Flag),
    ins_op_(Op, Name),
    cpp_endif.
ins_op('$exported_ins'(Op, _Ins, Name)) =>
    ins_op_(Op, Name).

ins_op_(Opcode, Name) =>
    [[ uppercase(Name, NameUp) ]],
    cpp_define(NameUp, Opcode).

% TODO: refactor
% Engine info (for inclusion in Makefile)
:- rprop(eng_info_mk/0, []).
eng_info_mk =>
    [[ findall(F, use_native(F, c), Cs) ]],
    [[ findall(F, use_native(F, h), Hs) ]],
    [[ findall(F, use_native(F, h_noalias), HsNoAlias) ]],
    [[ engine_stubmain(StubMain) ]],
    makefile_def('ENG_STUBMAIN', [StubMain]),
    makefile_def('ENG_CFILES', Cs),
    makefile_def('ENG_HFILES', Hs),
    makefile_def('ENG_HFILES_NOALIAS', HsNoAlias).

:- rprop(makefile_def/2, []).
makefile_def(X, Fs) =>
    tk(X), ' ', tk('='), ' ', 
    '$foreach_sep'(' ', Fs, fmt_atom),
    tk_nl.

% Engine info (for inclusion in sh scripts)
:- rprop(eng_info_sh/0, []).
eng_info_sh =>
    [[ findall(F, use_native(F, c), Cs) ]],
    [[ findall(F, use_native(F, h), Hs) ]],
    [[ findall(F, use_native(F, h_noalias), HsNoAlias) ]],
    [[ engine_stubmain(StubMain) ]],
    sh_def('ENG_STUBMAIN', [StubMain]),
    sh_def('ENG_CFILES', Cs),
    sh_def('ENG_HFILES', Hs),
    sh_def('ENG_HFILES_NOALIAS', HsNoAlias).

:- rprop(sh_def/2, []).
sh_def(X, Fs) =>
    tk(X), tk('='), tk('"'), 
    '$foreach_sep'(' ', Fs, fmt_atom),
    tk('"'),
    tk_nl.

:- rprop(fmt_atom/1, []).
fmt_atom(X) => tk(X).     

% Meta-information
:- rprop(absmachdef/0, []).
absmachdef =>
    autogen_warning_comment,
    %
    [[ get(max_op, MaxOp) ]],
    [[ NumOp is MaxOp + 1 ]],
    cpp_define('INS_OPCOUNT', NumOp),
    cpp_define('Fs(Ty)', callexp('FTYPE_size', [tk('Ty')])), % (shorter name) % TODO: duplicated
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
    tk('(ftype_base_t *[])'), '{', tk_nl,
    '$foreach_sep'(',\n', Ops, absmach_insinfo_),
    '}'.

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
    tk('(ftype_base_t *[])'), '{', tk_nl,
    '$foreach_sep'(',\n', FTypes, ftype_info_),
    '}'.

ftype_info_(Id), [[ id_ftype(Id, FType) ]] =>
    [[ ftype_def(FType, _, Def) ]],
    ftype_info__(Def, FType).
ftype_info_(_) => ftype_info__(str([]), none).

ftype_info__(array(A,B), _FType) =>
    [[ map_ftype_id([A,B], Ys) ]],
    callexp('FTYPE_ARRAY', Ys).
ftype_info__(str(Xs), _FType) => ftype_info__str(Xs).
ftype_info__(basic(SMethod,LMethod), FType) => callexp('FTYPE_BASIC', [fsize(FType),SMethod,LMethod]).
ftype_info__(blob, _) => callexp('FTYPE_BLOB', []).

ftype_info__str([]) => callexp('FTYPE_STR0', []).
ftype_info__str(Xs) =>
    [[ length(Xs, N) ]],
    [[ map_ftype_id(Xs, Ys) ]],
    callexp('FTYPE_STR', [N, callexp('BRACES', Ys)]).

:- rprop(ftype_id/1, []).
ftype_id(FType) =>
    [[ ftype_def(FType, Id, _) ]],
    tk_number(Id).

:- rprop(insnames/0, []).
insnames =>
    [[ get(max_op, MaxOp) ]],
    [[ NumOp is MaxOp + 1 ]],
    tk('char *ins_name['), NumOp, tk('] = '), '{', tk_nl,
    [[ range(0, MaxOp, Ops) ]],
    '$foreach_sep'(',\n', Ops, op_insname),
    '}', stmtend.

:- rprop(op_insname/1, []).
op_insname(Op) =>
    ( [[ get(op(Op).ins, Ins) ]] ->
        tk('"'), tk(Ins), tk('"')
    ; tk_string("(none)")
    ).

:- rprop(autogen_warning_comment/0, []).
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

:- rprop(op_macros/0, []).
op_macros =>
    cpp_define('LoadH', paren((tk('H'), tk('='), (~w)^.heap_top))),
    cpp_define('StoreH', paren(((~w)^.heap_top, tk('='), tk('H')))),
    %
    cpp_define('BcOPCODE', callexp('BcFetchOPCODE', [])),
    % address for a bytecode operand
    cpp_define('BcP(Ty,X)', paren((tk('*'), paren((callexp('FTYPE_ctype', [tk('Ty')]), ' ', tk('*'))), callexp('BCoff', [tk('P'), tk('(X)')])))),
    cpp_define('Fs(Ty)', callexp('FTYPE_size', [tk('Ty')])). % (shorter name)

:- rprop(wam_loop_defs/0, []).
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

:- rprop(wam_def/0, []).
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
    label('again'),
    tk('EXCEPTION__CATCH'), '(', '{', tk_nl, % try
    tk('CVOID__CALL(wam__2, desc, func)'), stmtend,
    return,
    '}', tk(','), ' ', '{', tk_nl, % catch
    vardecl(ptr(choice), tk('b')),
    %% vardecl(ptr(frame), tk('e')),
    % (like code_neck but do not set frame)
    if(not(callexp('IsDeep',[])), do_neck), % Force neck if not done
    %% code_neck, % Force neck if not done
    x(0) <- callexp('MakeSmall', [tk('ErrCode')]), % Error code
    x(1) <- callexp('GET_ATOM', [tk('ErrFuncName')]), % Builtin name
    x(2) <- callexp('MakeSmall', [tk('ErrFuncArity')]), % Builtin arity
    x(4) <- tk('Culprit'), % Culprit arg.
    x(3) <- callexp('MakeSmall', [tk('ErrArgNo')]), % w. number
    tk('func') <- tk('address_error'),
    goto('again'),
    '}', tk(')'), stmtend,
    '}', tk_nl.

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
    '{', tk_nl,
    wam_loop,
    '}', tk_nl.

wam_loop =>
    wam_loop_decls,
    code_loop_begin,
    % MISCELLANEOUS SUPPORT
    %
    labeled_block('escape_to_p2', escape_to_p2),
    %
    labeled_block('escape_to_p', escape_to_p),
    %
    % ENTERING A PREDICATE:  H always live.
    % Take into account attributed variables !!
    labeled_block('enter_predicate', code_enter_pred),
    %
    labeled_block('switch_on_pred', switch_on_pred),
    %
    labeled_block('switch_on_pred_sub', code_switch_on_pred_sub),
    %
    % FAILING
    labeled_block('fail', code_fail),
    %
    alt_ins_dispatcher,
    %
    labeled_block('exit_toplevel', code_exit_toplevel),
    %
    labeled_block('illop', code_illop).

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
      % callstmt('GetFrameTop', [(~w)^.local_top,(~b),(~g)^.frame]),
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
    '{',
    localv(tagged, T2),
    localv(tagged, T3),
    T2 <- callexp('PointerToTerm', [(~func)^.code.intinfo]),
    emul_to_goal(T3), % (stores: T3)
    (~p) <- tk('ptemp'),
    x(0) <- T3,
    x(1) <- T2,
    '}',
    goto('switch_on_pred').

escape_to_p => % (needs: ptemp)
    [[ update(mode(w)) ]],
    '{',
    localv(tagged, T3),
    emul_to_goal(T3), % (stores: T3)
    (~p) <- tk('ptemp'),
    x(0) <- T3,
    '}',
    goto('switch_on_pred').

code_undo(T0) =>
    [[ update(mode(r)) ]],
    (~w)^.frame <- (~b)^.frame,
    (~w)^.next_insn <- (~b)^.next_insn,
    callstmt('SetE', [callexp('NodeLocalTop', [(~b)])]),
    (~e)^.frame <- (~w)^.frame,
    (~e)^.next_insn <- (~w)^.next_insn,
    (~w)^.frame <- (~e),
    (~w)^.next_insn <- tk('failcode'),
    (~w)^.local_top <- cast(ptr(frame), callexp('Offset', [(~e),tk('EToY0')])),
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
    callstmt('ResetWakeCount', []),
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
    T1 <- cast(tagged, callexp('TrailTopUnmark', [(~b)^.trail_top])),
    if(callexp('TrailYounger',[Pt2,T1]),
      (do_while(
        ([[ mode(M) ]],
         callstmt('PlainUntrail', [Pt2, T0, blk((
           (~w)^.trail_top <- Pt2,
           code_undo(T0)
         ))]),
         [[ update(mode(M)) ]]),
        (callexp('TrailYounger', [Pt2,T1]))),
      (~w)^.trail_top <- Pt2)),
    '}',
    %
    backtrack_.

backtrack_ =>
    (~w)^.heap_top <- callexp('NodeGlobalTop', [(~b)]),
    code_restore_args,
    profile_hook(redo),
    (~p) <- cast(bcp, (~w)^.next_alt),
    '{',
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
    '}',
    '$unreachable'.

jump_fail_cont(no_alt, _Alt) =>
    callstmt('SetDeep', []),
    (~b) <- (~w)^.previous_choice,
    callstmt('SetChoice', [(~b)]),
    callstmt('ON_TABLING', [blk((
        % To avoid sharing wrong trail - it might be associated to the
        % previous frozen choice point
        if(callexp('FrozenChpt', [(~b)]),
           callstmt('push_choicept', [(~w),tk('address_nd_fake_choicept')]))
    ))]),
    jump_alt_code((~p)).
jump_fail_cont(next_alt, Alt) =>
    % TODO:[oc-merge] 'altmode.jump_fail_cont'(_,next_alt)
    callstmt('CODE_CHOICE_PATCH', [(~w)^.choice, Alt]),
    jump_alt_code((~p)).

jump_alt_code(Alt) =>
    (~p) <- cast(ptr(try_node), Alt)^.emul_p,
    if(not(callexp('IsVar',[x(0)])), jump_ins_dispatch),
    setmode(w),
    jump_ins_dispatch.

code_restore_args =>
    if(callexp('IsDeep',[]), deep_backtrack).

% TODO:[oc-merge] part of code_restore_args0
deep_backtrack =>
    % deep backtracking
    trace(deep_backtracking),
    (~w)^.frame <- (~b)^.frame,
    (~w)^.next_insn <- (~b)^.next_insn,
    (~w)^.next_alt <- (~b)^.next_alt,
    (~w)^.local_top <- callexp('NodeLocalTop', [(~b)]),
    '{',
    % TODO: use this syntax? I::intmach <- B^.next_alt^.arity,
    localv(intmach, I, (~b)^.next_alt^.arity),
    (~w)^.previous_choice <- callexp('ChoiceCont0', [(~b),I]),
    % TODO:[oc-merge] set_shallow_retry here?
    callstmt('SetShallowRetry', []),
    foreach(intmach, range(I), K, ((~w)^.x[K] <- (~b)^.x[K])),
    '}'.

code_enter_pred =>
    [[ update(mode(w)) ]],
    callstmt('ON_ANDPARALLEL', [blk((
      if(tk('Suspend') == tk('TOSUSPEND'), (
          tk('Suspend') <- tk('SUSPENDED'),
          callstmt('Wait_Acquire_lock', [tk('Waiting_For_Work_Lock')]),
          callstmt('Cond_Var_Wait', [tk('Waiting_For_Work_Cond_Var'),tk('Waiting_For_Work_Lock')]),
          tk('Suspend') <- tk('RELEASED'),
          callstmt('Release_lock', [tk('Waiting_For_Work_Lock')])))
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
    if(callexp('TestEventOrHeapWarnOverflow', [tk('H')]), (
      [[ WakeCount = tk('wake_count') ]],
      vardecl(int, WakeCount),
      %
      if(cbool_succeed('Stop_This_Goal',[]), goto('exit_toplevel')),
      %
      WakeCount <- callexp('WakeCount', []),
      %
      if(callexp('HeapCharAvailable', [tk('H')]) =< tk('CALLPAD')+4*WakeCount*sizeof(tagged), % TODO: It was OffHeaptop(H+4*wake_count,Heap_Warn), equivalent to '<='; but '<' should work?! (also in TestEventOrHeapWarnOverflow?)
        (callstmt('SETUP_PENDING_CALL', [(~e), tk('address_true')]),
         setmode(r),
         cvoid_call('heap_overflow', [2*(tk('CALLPAD')+4*WakeCount*sizeof(tagged))]),
         setmode(w))),
      if(WakeCount > 0,
        if(WakeCount==1, (
          callstmt('SETUP_PENDING_CALL', [(~e), tk('address_uvc')]),
          cvoid_call('collect_one_pending_unification', []), % does not touch H
          localv(tagged, T0),
          callstmt('DEREF', [T0,x(1)]),
          if(callexp('TaggedIsCVA', [T0]),
            (% X(1)=*TaggedToGoal(t0);
             x(1) <- T0,
             % patch prev. SETUP_PENDING_CALL
             callstmt('Setfunc', [tk('address_ucc')])))),
          % wake_count > 1
          ([[ update(mode(w)) ]],
           callstmt('SETUP_PENDING_CALL', [(~e), tk('address_pending_unifications')]),
           setmode(r),
           cvoid_call('collect_pending_unifications', [WakeCount]),
           setmode(w)))),
      if(callexp('OffStacktop', [(~w)^.frame,tk('Stack_Warn')]), (
         callstmt('SETUP_PENDING_CALL', [(~e), tk('address_true')]),
         cvoid_call('stack_overflow', []))),
      callstmt('UnsetEvent', []),
      if(callexp('TestCIntEvent', []), (
         callstmt('SETUP_PENDING_CALL', [(~e), tk('address_help')]),
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
    '{',
    localv(intmach, I, call_fC(cbool0,(~func)^.code.proc,[])),
    [[ ExpW = tk('Expanded_Worker') ]],
    if(not_null(ExpW),
      (trace(worker_expansion_blt),
      if(is_null(tk('desc')),
        (% JFKK this is temp sometimes wam is called without gd
         trace_print([tk_string("bug: invalid WAM expansion\n")]),
         callstmt('abort', []))),
      (~w) <- ExpW,
      tk('desc')^.worker_registers <- (~w),
      ExpW <- ~null)),
    if(I,goto_ins(proceed),jump_fail),
    '}'.

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
    (~w)^.misc^.ins <- callexp('CFUN__EVAL', [tk('current_instance0')]),
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
         callstmt('abort', []))),
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
    (~w)^.misc^.ins <- callexp('TaggedToInstance', [x(2)]),
    (~p) <- cast(bcp, (~w)^.misc^.ins^.emulcode),
    jump_ins_dispatch.

pred_enter_builtin_geler =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    '{',
    localv(tagged, T1, x(0)),
    deref_sw0(T1,';'),
    localv(tagged, T3),
    T3 <- x(1),
    deref_sw0(T3,';'),
    callstmt('Setfunc', [callexp('find_definition', [tk('predicates_location'),T3,addr((~w)^.structure),~true])]),
    % suspend the goal  t3  on  t1.  Func, must be live.
    [[ mode(M) ]],
    setmode(r),
    callstmt('CVOID__CALL', [tk('SUSPEND_T3_ON_T1'), (~func), T3, T1]),
    setmode(M),
    '}',
    goto_ins(proceed).

% Like pred_enter_builtin_syscall/0, but fails on undefined
pred_enter_builtin_nodebugcall =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    '{',
    localv(tagged, T0, x(0)),
    deref_sw(T0,x(0),';'),
    do_builtin_call(nodebugcall, T0),
    '}'.

% Like pred_enter_builtin_call/0, but ignores Current_Debugger_Mode
pred_enter_builtin_syscall =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    '{',
    localv(tagged, T0, x(0)), 
    deref_sw(T0,x(0),';'),
    do_builtin_call(syscall, T0),
    '}'.

pred_enter_builtin_call =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    '{',
    localv(tagged, T0, x(0)),
    deref_sw(T0,x(0),';'),
    do_builtin_call(call, T0),
    '}'.

do_builtin_call(CallMode, T0) =>
    callstmt('Setfunc', [callexp('find_definition', [tk('predicates_location'),T0,addr((~w)^.structure),~false])]),
    % Undefined?
    ( [[ CallMode = nodebugcall ]] ->
        if(is_null((~func)),jump_fail)
    ; ( [[ CallMode = syscall ]] -> true
      ; [[ CallMode = call ]] -> true
      ; [[ fail ]]
      ),
      if(is_null((~func)),
        (callstmt('Setfunc', [tk('address_undefined_goal')]),
         goto('switch_on_pred')))
    ),
    % Debug hook?
    ( [[ CallMode = nodebugcall ]] -> true
    ; [[ CallMode = syscall ]] -> true
    ; [[ CallMode = call ]],
      if(tk('Current_Debugger_Mode') \== tk('atom_off'),
        (callstmt('Setfunc', [tk('address_trace')]),
         goto('switch_on_pred')))
    ),
    %
    jump_call4((~func)^.enter_instr).

jump_call4(Enter) => tk('ei') <- Enter, goto('call4').
code_call4 =>
    switch(tk('ei'), (
        case('ENTER_INTERPRETED'), pred_call_interpreted,
        case('BUILTIN_DIF'), pred_call_builtin_dif,
        case('SPYPOINT'), pred_call_spypoint,
        case('WAITPOINT'), label('call_waitpoint'), pred_call_waitpoint,
        labeled_block('call5', code_call5), % TODO: move outside switch?
        label('default'), pred_call_default
    )).

pred_call_interpreted =>
    [[ update(mode(w)) ]],
    % pred_trace(tk_string("I")),
    x(1) <- callexp('PointerToTerm', [(~func)^.code.intinfo]),
    callstmt('Setfunc', [tk('address_interpret_goal')]),
    goto('switch_on_pred').

pred_call_builtin_dif =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    '{',
    localv(ptr(tagged), Pt1, (~w)^.structure),
    localv(tagged, T0), callstmt('RefHeapNext', [T0,Pt1]), x(0) <- T0,
    localv(tagged, T1), callstmt('RefHeapNext', [T1,Pt1]), x(1) <- T1,
    '}',
    %goto('dif1').
    goto('dif0').

pred_call_spypoint =>
    [[ update(mode(w)) ]],
    if(not((~func)^.properties.wait), jump_call5),
    goto('call_waitpoint').

pred_call_waitpoint =>
    [[ update(mode(w)) ]],
    '{',
    localv(tagged, T0),
    localv(tagged, T1),
    callstmt('RefHeap', [T0,(~w)^.structure]),
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
    '}',
    jump_call5.

jump_call5 => goto('call5').
code_call5 => jump_call4((~func)^.predtyp).

pred_call_default =>
    [[ update(mode(w)) ]],
    '{',
    localv(intmach, I, (~func)^.arity),
    if(I\==0,
       (
       localv(ptr(tagged), Pt1, (~w)^.x),
       localv(ptr(tagged), Pt2, (~w)^.structure),
       do_while(
         callstmt('PushRefHeapNext', [Pt1,Pt2]),
         (tk('--'),I))
      )),
    '}',
    jump_switch_on_pred_sub(tk('ei')).

% NOTE: see prolog_dif
pred_enter_builtin_dif =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("B")),
    '{',
    localv(tagged, T0, x(0)), deref_sw0(T0,';'),
    localv(tagged, T1, x(1)), deref_sw0(T1,';'),
    (~w)^.structure <- ~null,
    %goto('dif1'),
    % check fast cases first
    %label('dif1'),
    %[[ update(mode(w)) ]],
    if(T0==T1,
      jump_fail,
      if(logical_and(not(callexp('IsVar',[T0/\T1])),
                     logical_or(callexp('IsAtomic',[T0]),
                                callexp('IsAtomic',[T1]))),
        goto_ins(proceed),
        (x(0) <- T0,
         x(1) <- T1,
         setmode(r),
         goto('dif2')))),
    '}',
    label('dif2'),
    [[ update(mode(r)) ]],
    if(not(cbool_succeed('prolog_dif', [(~func)])), jump_fail),
    goto_ins(proceed).

pred_enter_builtin_abort =>
    [[ update(mode(w)) ]],
    % cut all the way and fail, leaving wam with a return code
    pred_trace(tk_string("B")),
    '{',
    localv(tagged, T0, x(0)), deref_sw0(T0,';'),
    (~w)^.misc^.exit_code <- callexp('GetSmall', [T0]),
    '}',
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
    '{',
    localv(tagged, T1, x(0)),
    deref_sw(T1,x(0),(
      localv(tagged, T3),
      emul_to_goal(T3), % (stores: t3)
      T1 <- x(0),
      if(callexp('TaggedIsSVA', [T1]), % t1 may have been globalised
        callstmt('RefSVA', [T1,x(0)])),
      % suspend the goal  t3  on  t1.  Func, must be live.
      [[ mode(M) ]],
      setmode(r),
      callstmt('CVOID__CALL', [tk('SUSPEND_T3_ON_T1'), (~func), T3, T1]),
      setmode(M),
      goto_ins(proceed)
    )),
    '}',
    goto('nowait'),
    label('nowait'),
    jump_switch_on_pred_sub((~func)^.predtyp).

pred_enter_breakpoint =>
    [[ update(mode(w)) ]],
    jump_switch_on_pred_sub((~func)^.predtyp).

pred_enter_compactcode_indexed =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("E")),
    '{',
    localv(tagged, T0, x(0)),
    deref_sw(T0,x(0), jump_tryeach((~func)^.code.incoreinfo^.varcase)),
    localv(tagged, T1),
    setmode(r),
    % non variable
    if(T0 /\ tk('TagBitComplex'),
      if(T0 /\ tk('TagBitFunctor'), (
          (~s) <- callexp('TaggedToArg', [T0,0]),
          T1 <- callexp('HeapNext', [(~s)])
      ), (
          (~s) <- callexp('TagpPtr', [tk('LST'),T0]),
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
        HtabNode <- callexp('SW_ON_KEY_NODE_FROM_OFFSET', [Htab, T1]),
        if(logical_or(HtabNode^.key==T2, not(HtabNode^.key)), break),
        assign(I + sizeof(sw_on_key_node)),
        T1 <- (T1+I) /\ Htab^.mask
    ), ~true),
    jump_tryeach(HtabNode^.value.try_chain), % (this will break the loop)
    '}'.

pred_enter_compactcode =>
    [[ update(mode(w)) ]],
    pred_trace(tk_string("E")),
    [[ update(mode(w)) ]],
    jump_tryeach((~func)^.code.incoreinfo^.varcase).

jump_switch_on_pred_sub(Enter), [[ Enter = tk('ei') ]] => goto('switch_on_pred_sub').
jump_switch_on_pred_sub(Enter) =>
   tk('ei') <- Enter,
   goto('switch_on_pred_sub').

code_switch_on_pred_sub => % (needs: ei)
    switch(tk('ei'), (
        case('ENTER_FASTCODE_INDEXED'), goto('enter_undefined'),
        case('ENTER_FASTCODE'), goto('enter_undefined'),
        case('ENTER_UNDEFINED'), label('enter_undefined'), pred_enter_undefined,
        case('ENTER_INTERPRETED'), pred_enter_interpreted,
        case('ENTER_C'), pred_enter_c,
        case('BUILTIN_TRUE'), pred_enter_builtin_true,
        case('BUILTIN_FAIL'), pred_enter_builtin_fail,
        case('BUILTIN_CURRENT_INSTANCE'), pred_enter_builtin_current_instance,
        case('BUILTIN_COMPILE_TERM'), pred_enter_builtin_compile_term,
        case('BUILTIN_INSTANCE'), pred_enter_builtin_instance,
        case('BUILTIN_GELER'), pred_enter_builtin_geler,
        case('BUILTIN_NODEBUGCALL'), pred_enter_builtin_nodebugcall,
        case('BUILTIN_SYSCALL'), pred_enter_builtin_syscall,
        labeled_block('call4', code_call4), % TODO: move outside switch?
        case('BUILTIN_CALL'), pred_enter_builtin_call,
        case('BUILTIN_DIF'), label('dif0'), pred_enter_builtin_dif,
        case('BUILTIN_ABORT'), pred_enter_builtin_abort,
        case('SPYPOINT'), pred_enter_spypoint,
        case('WAITPOINT'), label('waitpoint'), pred_enter_waitpoint,
        case('BREAKPOINT'), pred_enter_breakpoint,
        case('ENTER_PROFILEDCODE_INDEXED'), goto('enter_compactcode_indexed'),
        case('ENTER_COMPACTCODE_INDEXED'), label('enter_compactcode_indexed'), pred_enter_compactcode_indexed,
        case('ENTER_PROFILEDCODE'), goto('enter_compactcode'),
        case('ENTER_COMPACTCODE'), label('enter_compactcode'), pred_enter_compactcode
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
    callstmt('SERIOUS_FAULT', [tk_string("unimplemented WAM instruction")]).

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

tryeach_lab(Lab) => get_label('tryeach', Lab).

alt_dispatcher => % (needs: alts)
    tryeach_lab(TryEach),
    label(TryEach),
    [[ Alts = tk('alts') ]],
    %
    gauge_incr_counter_alts(Alts),
    %
    emul_p(Alts, EmulP),
    (~p) <- EmulP,
    % TODO:[merge-oc] try_alt/1
    (~w)^.previous_choice <- (~w)^.choice,
    '{',
    localv(ptr(try_node), Alt, (Alts^.next)),
    if(not_null(Alt), ( % TODO: This one is not a deep check! (see line above)
      (~b) <- (~w)^.choice,
      callstmt('GetFrameTop', [(~w)^.local_top,(~b),(~g)^.frame]),
      cachedreg('H',H),
      callstmt('CODE_CHOICE_NEW0', [(~b), Alt, H]),
      trace(create_choicepoint),
      % segfault patch -- jf
      maybe_choice_overflow
    ),(
      callstmt('SetDeep', [])
    )),
    '}',
    jump_ins_dispatch.

gauge_incr_counter_alts(Alts) => % Counter in Alts
    ( [[ mode(r) ]] -> [[ EntryCounter = (Alts^.entry_counter + 1) ]] % TODO: do not use pointer arith
    ; [[ mode(w) ]], [[ EntryCounter = (Alts^.entry_counter) ]]
    ),
    gauge_incr_counter(EntryCounter).

jump_ins_dispatch =>
    ins_dispatch_label(DispatchLabel),
    goto(DispatchLabel).

% TODO: define special meta-predicates? (Label is a output meta-argument)
ins_dispatch_label(Label), [[ mode(r) ]] => [[ Label = 'ReadMode' ]]. /* Here with H in memory. */
ins_dispatch_label(Label), [[ mode(w) ]] => [[ Label = 'WriteMode' ]]. /* Here with H in register. */

ins_dispatcher =>
    ins_dispatch_label(Label),
    label(Label),
    [[ collect_and_filter(instruction_set, '$ins_entry'/4, Insns) ]],
    switch(tk('BcOPCODE'),
      (% (all instructions)
      '$foreach'(Insns, ins_case),
      label('default'),
      goto('illop'))).

% Instruction case
ins_case('$ins_entry'(Opcode,InsName,InsCode,Format)), [[ get(op(Opcode).optional, Flag) ]] =>
    % Emit optional instructions (based on C preprocessor flags)
    cpp_if_defined(Flag),
    ins_case_(Opcode,InsName,InsCode,Format),
    cpp_endif.
ins_case('$ins_entry'(Opcode,InsName,InsCode,Format)) => ins_case_(Opcode,InsName,InsCode,Format).

% TODO: write instruction as comment? [[ uppercase(Ins, InsUp) ]], % (do not use name, just opcode)
ins_case_(Opcode,InsName,InsCode,Format) =>
    ins_label(Opcode,InsName),
    case(Opcode),
    [[ mode(M) ]],
    [[ update(format(Format)) ]],
    ins_case__(Opcode,InsCode),
    [[ update(mode(M)) ]].

ins_case__(Opcode, InsCode), [[ get(op(Opcode).in_mode, M) ]] => in_mode(M, InsCode).
ins_case__(_, InsCode) => InsCode.

% Wrapper for execution of instruction G in the specified mode M
in_mode(M, G), [[ mode(M) ]] => G.
in_mode(M, G) => setmode(M), goto_ins(G).

% 'label' statement for the instruction
ins_label(Opcode,InsName), [[ mode(M), get(op(Opcode).label, M) ]] =>
   get_label(InsName, Label),
   label(Label).
ins_label(_,_) => true.

get_label(X, Label), [[ mode(r) ]] => [[ atom_concat('r_', X, Label) ]].
get_label(X, Label), [[ mode(w) ]] => [[ atom_concat('w_', X, Label) ]].

%! # Instruction entries
% NOTE: declaration order is important (for performance)

ins_entry(Ins,Opcode,Format) => ins_entry(Ins,Opcode,Format,[]).

% TODO: we need more complex rules (see OC)
ins_entry(Ins,Opcode,Format,exported+Props) => % (exported)
    ins_entry_(Ins,Opcode,Format,yes,Props).
ins_entry(Ins,Opcode,Format,Props) => % (no exported)
    ins_entry_(Ins,Opcode,Format,no,Props).

ins_entry_(Ins,Opcode,Format,Export,[q|Props]) => % (no props in Q version)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,InsQ,[],Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[q0|Props]) => % (see q0)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,q0(Ins),[],Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[q0w|Props]) => % (see q0w)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,q0w(Ins,Ins),[],Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[q0r|Props]) => % (see q0r)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,q0r(Ins,Ins),[],Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[qall|Props]) => % (no props in Q version)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,Ins,[],Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[qqall|Props]) => % (props in both)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,Ins,Props,Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[qq|Props]) => % (props in both)
    [[ atom_concat(Ins, 'q', InsQ) ]],
    ins_entry_align(InsQ,Ins,Opcode,Format,Export,InsQ,Props,Ins,Props).
ins_entry_(Ins,Opcode,Format,Export,[rw(R,W)|Props]) =>
    ins_entry__(Ins,Opcode,Format,Export,i_rw(Ins,R,W),Props).
ins_entry_(Ins,Opcode,Format,Export,Props) =>
    ins_entry__(Ins,Opcode,Format,Export,Ins,Props).

ins_entry_align(InsQ,Ins,Opcode,Format,Export,InsCodeQ,PropsQ,InsCode,Props) =>
    [[ OpcodeQ is Opcode - 1 ]],
    ins_entry__(InsQ, OpcodeQ, [f_Q|Format], Export, InsCodeQ, PropsQ),
    ins_entry__(Ins, Opcode, Format, Export, InsCode, Props).

ins_entry__(Ins,Opcode,Format,yes,InsCode,Props) =>
    prim('$exported_ins'(Opcode, Ins, Ins)),
    ins_entry__(Ins,Opcode,Format,no,InsCode,Props).
ins_entry__(Ins,Opcode,Format,no,InsCode,Props) =>
    prim('$decl'(ins_op_format(Ins,Opcode,Format,Props))),
    prim('$ins_entry'(Opcode,Ins,InsCode,Format)).

% shift padding and go to unpadded instruction
q0(Ins) => shiftf, goto_ins(Ins).

% like q0, only for write mode
q0w(Ins, _InsCode), [[ mode(w) ]] => shiftf, goto_ins(Ins).
q0w(_Ins, InsCode), [[ mode(r) ]] => InsCode.

% like q0, only for read mode
q0r(Ins, _InsCode), [[ mode(r) ]] => shiftf, goto_ins(Ins).
q0r(_Ins, InsCode), [[ mode(w) ]] => InsCode.

% goto in read mode, ins in write mode
i_rw(InsCode,e(InsR,0),1), [[ mode(r) ]] => goto_ins(InsR).
i_rw(InsCode,e(InsR,0),1), [[ mode(w) ]] => InsCode.

:- '$decl'(ins_op_format/4).
ins_op_format(Ins, Op, Format, Props) :-
    putmax(max_op, Op),
    put(op(Op).format, Format),
    put(op(Op).ins, Ins),
    ins_op_props(Op, Props).

:- '$decl'(ins_op_props/2).
ins_op_props(_Op, []) :- true. % TODO: allow facts (no ":- true")
ins_op_props(Op, [Prop|Props]) :-
    ins_op_prop(Op, Prop),
    ins_op_props(Op, Props).

:- '$decl'(ins_op_prop/2).
ins_op_prop(Op, label(M)) :- put(op(Op).label, M).
% instruction always switches to mode M
ins_op_prop(Op, in_mode(M)) :- put(op(Op).in_mode, M).
% instruction is optional (under cpp flag)
ins_op_prop(Op, optional(Name)) :- put(op(Op).optional, Name).

%! # Instruction set switch
% NOTE: declaration order is important (for performance)

% :- ins_op_format(ci_call, 241, [f_i,f_i])),
% :- ins_op_format(ci_inarg, 242, [f_i,f_i])),
% :- ins_op_format(ci_outarg, 243, [f_i,f_i])),
% :- ins_op_format(ci_retval, 244, [f_i,f_i])),

instruction_set =>
    iset_init,
    iset_call,
    iset_put,
    iset_get1,
    iset_cut,
    iset_choice,
    iset_misc1,
    iset_get2,
    ins_entry(branch, 68, [f_i], exported+[]), % (for qread)
    iset_blt,
    ins_entry(get_constraint, 247, [f_x], exported+[in_mode(w),label(w)]), % (for compile_term_aux)
    iset_unify,
    iset_u2,
    iset_misc2.

iset_init =>
    ins_entry(inittrue, 260, [f_e], [in_mode(w),label(w)]),
    ins_entry(firsttrue_n, 261, [f_Y,f_e], [in_mode(w),label(w)]),
    ins_entry(initcall, 1, [f_E,f_e], [q0,in_mode(w),label(_)]).

iset_call =>
    ins_entry(firstcall_n, 21, [f_Y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_8, 19, [f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_7, 17, [f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_6, 15, [f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_5, 13, [f_y,f_y,f_y,f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_4, 11, [f_y,f_y,f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_3, 9, [f_y,f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_2, 7, [f_y,f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall_1, 5, [f_y,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(firstcall, 3, [f_E,f_e], [q0,in_mode(w),label(_)]),
    %
    ins_entry(call_n, 41, [f_Z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_8, 39, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_7, 37, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_6, 35, [f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_5, 33, [f_z,f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_4, 31, [f_z,f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_3, 29, [f_z,f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_2, 27, [f_z,f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call_1, 25, [f_z,f_E,f_e], [q0,in_mode(w),label(_)]),
    ins_entry(call, 23, [f_E,f_e], exported+[q0,in_mode(w),label(_)]), % (for ciao_initcode and init_some_bytecode)
    %
    ins_entry(lastcall_n, 61, [f_Z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_8, 59, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_7, 57, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_6, 55, [f_z,f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_5, 53, [f_z,f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_4, 51, [f_z,f_z,f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_3, 49, [f_z,f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_2, 47, [f_z,f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall_1, 45, [f_z,f_E], [q0,in_mode(w),label(_)]),
    ins_entry(lastcall, 43, [f_E], exported+[q0,in_mode(w),label(_)]), % (for chat_tabling.c)
    %
    ins_entry(execute, 63, [f_E], exported+[qall,label(w)]). % (for ciao_initcode and init_some_bytecode)

iset_put =>
    ins_entry(put_x_void, 69, [f_x], [in_mode(w),label(w)]),
    ins_entry(put_x_variable, 70, [f_x,f_x], [in_mode(w),label(w)]),
    ins_entry(put_xval_xval, 85, [f_x,f_x,f_x,f_x]),
    ins_entry(put_x_value, 71, [f_x,f_x]),
    ins_entry(put_x_unsafe_value, 72, [f_x,f_x], [in_mode(w),label(w)]),
    ins_entry(put_y_first_variable, 73, [f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_y_variable, 74, [f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_yfvar_yvar, 83, [f_x,f_y,f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_yvar_yvar, 84, [f_x,f_y,f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_y_value, 75, [f_x,f_y]),
    ins_entry(put_y_unsafe_value, 76, [f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_constant, 78, [f_x,f_t], [qq]),
    ins_entry(put_nil, 81, [f_x]),
    ins_entry(put_large, 253, [f_x,f_b], [qq,in_mode(w),label(w)]),
    ins_entry(put_structure, 80, [f_x,f_f], [qq,in_mode(w),label(w)]),
    ins_entry(put_list, 82, [f_x], [in_mode(w),label(w)]),
    ins_entry(put_yval_yval, 86, [f_x,f_y,f_x,f_y]),
    ins_entry(put_yval_yuval, 87, [f_x,f_y,f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_yuval_yval, 88, [f_x,f_y,f_x,f_y], [in_mode(w),label(w)]),
    ins_entry(put_yuval_yuval, 89, [f_x,f_y,f_x,f_y], [in_mode(w),label(w)]).

iset_blt =>
    ins_entry(function_1, 223, [f_x,f_x,f_C,f_g], [qq,in_mode(r),label(r)]),
    ins_entry(function_2, 225, [f_x,f_x,f_x,f_C,f_g], [qq,in_mode(r),label(r)]),
    ins_entry(builtin_1, 227, [f_x,f_C], [qq,in_mode(r),label(r)]),
    ins_entry(builtin_2, 229, [f_x,f_x,f_C], [qq,in_mode(r),label(r)]),
    ins_entry(builtin_3, 231, [f_x,f_x,f_x,f_C], [qq,in_mode(r),label(r)]),
    ins_entry(retry_instance, 232, [], exported+[in_mode(r),label(r)]).

iset_get1 =>
    ins_entry(get_x_value, 91, [f_x,f_x], exported+[in_mode(r),label(r)]), % (for cterm)
    ins_entry(get_y_first_value, 94, [f_x,f_y], [in_mode(r),label(r)]),
    ins_entry(get_y_value, 95, [f_x,f_y], [in_mode(r),label(r)]),
    ins_entry(get_constant, 97, [f_x,f_t], exported+[q0,in_mode(r),label(_)]), % (for cterm)
    ins_entry(get_large, 255, [f_x,f_b], exported+[q0,in_mode(r),label(_)]), % (for cterm)
    ins_entry(get_structure, 99, [f_x,f_f], exported+[q0,in_mode(r),label(_)]), % (for cterm)
    ins_entry(get_nil, 100, [f_x], exported+[in_mode(r),label(r)]), % (for cterm)
    ins_entry(get_list, 101, [f_x], exported+[in_mode(r),label(r)]), % (for cterm)
    ins_entry(get_constant_neck_proceed, 112, [f_x,f_t], [q0,in_mode(r),label(_)]),
    ins_entry(get_nil_neck_proceed, 113, [f_x], [in_mode(r),label(r)]).

iset_cut =>
    ins_entry(cutb_x, 208, [f_x], [in_mode(r),label(r)]),
    ins_entry(cutb_x_neck, 210, [f_x], [in_mode(r),label(r)]),
    ins_entry(cutb_neck, 211, [], [in_mode(r),label(r)]),
    ins_entry(cutb_x_neck_proceed, 212, [f_x], [in_mode(r),label(r)]),
    ins_entry(cutb_neck_proceed, 213, [], [in_mode(r),label(r)]),
    ins_entry(cute_x, 214, [f_x], [in_mode(r),label(r)]),
    ins_entry(cute_x_neck, 216, [f_x], [in_mode(r),label(r)]),
    ins_entry(cute_neck, 217, [], [in_mode(r),label(r)]),
    ins_entry(cutf_x, 215, [f_x], [in_mode(r),label(r)]),
    ins_entry(cutf, 209, [], [in_mode(r),label(r)]),
    ins_entry(cut_y, 218, [f_y], [in_mode(r),label(r)]).

iset_choice =>
    ins_entry(choice_x, 219, [f_x]),
    ins_entry(choice_yf, 220, [f_y]),
    ins_entry(choice_y, 221, [f_y], [label(_)]).

iset_misc1 =>
    ins_entry(kontinue, 233, [], exported+[in_mode(w),label(w)]),
    ins_entry(leave, 234, [], [in_mode(r),label(r)]),
    ins_entry(exit_toplevel, 235, [], exported+[in_mode(r),label(r)]),
    ins_entry(retry_c, 238, [f_C], exported+[qq,in_mode(r),label(r)]).

iset_get2 =>
    ins_entry(get_structure_x0, 105, [f_f], exported+[q0w,label(w)]), % (for cterm)
    ins_entry(get_large_x0, 257, [f_b], [q0w,label(w)]),
    ins_entry(get_constant_x0, 103, [f_t], exported+[q0w,label(w)]), % (for cterm)
    ins_entry(get_nil_x0, 106, [], exported+[]), % (for cterm)
    ins_entry(get_list_x0, 107, [], exported+[]), % (for cterm)
    ins_entry(get_xvar_xvar, 108, [f_x,f_x,f_x,f_x]),
    ins_entry(get_x_variable, 90, [f_x,f_x], exported+[]), % (for cterm)
    ins_entry(get_y_first_variable, 92, [f_x,f_y]),
    ins_entry(get_y_variable, 93, [f_x,f_y], [label(_)]),
    ins_entry(get_yfvar_yvar, 109, [f_x,f_y,f_x,f_y]),
    ins_entry(get_yvar_yvar, 110, [f_x,f_y,f_x,f_y], [label(_)]).

iset_unify =>
    ins_entry(unify_void, 114, [f_i], exported+[]), % (for cterm)
    ins_entry(unify_void_1, 115, [], exported+[label(w)]), % (for cterm)
    ins_entry(unify_void_2, 116, [], exported+[label(w)]), % (for cterm)
    ins_entry(unify_void_3, 117, [], exported+[label(w)]), % (for cterm)
    ins_entry(unify_void_4, 118, [], exported+[label(w)]), % (for cterm)
    ins_entry(unify_x_variable, 119, [f_x], exported+[]), % (for cterm)
    ins_entry(unify_x_value, 120, [f_x], exported+[rw(e(unify_x_local_value,0),1)]), % (for cterm)
    ins_entry(unify_x_local_value, 121, [f_x], [label(r)]),
    ins_entry(unify_y_first_variable, 122, [f_y]),
    ins_entry(unify_y_variable, 123, [f_y], [label(_)]),
    ins_entry(unify_y_first_value, 124, [f_y]),
    ins_entry(unify_y_value, 125, [f_y], [rw(e(unify_y_local_value,0),1)]),
    ins_entry(unify_y_local_value, 126, [f_y], [label(r)]),
    ins_entry(unify_constant, 128, [f_t], exported+[q0r,label(r)]), % (for cterm)
    ins_entry(unify_large, 259, [f_b], exported+[q0,label(_)]), % (for cterm)
    ins_entry(unify_structure, 130, [f_f], exported+[q0r,label(r)]), % (for cterm)
    ins_entry(unify_nil, 131, [], exported+[]), % (for cterm)
    ins_entry(unify_list, 132, [], exported+[]), % (for cterm)
    ins_entry(unify_constant_neck_proceed, 134, [f_t], [q0r,label(r)]),
    ins_entry(unify_nil_neck_proceed, 135, []).

iset_u2 =>
    ins_entry(u2_void_xvar, 136, [f_i,f_x]),
    ins_entry(u2_void_yfvar, 139, [f_i,f_y]),
    ins_entry(u2_void_yvar, 140, [f_i,f_y], [label(_)]),
    ins_entry(u2_void_xval, 137, [f_i,f_x], [rw(e(u2_void_xlval,0),1)]),
    ins_entry(u2_void_xlval, 138, [f_i,f_x], [label(r)]),
    ins_entry(u2_void_yfval, 141, [f_i,f_y]),
    ins_entry(u2_void_yval, 142, [f_i,f_y], [rw(e(u2_void_ylval,0),1)]),
    ins_entry(u2_void_ylval, 143, [f_i,f_y], [label(r)]),
    ins_entry(u2_xvar_void, 144, [f_x,f_i]),
    ins_entry(u2_xvar_xvar, 145, [f_x,f_x]),
    ins_entry(u2_xvar_yfvar, 148, [f_x,f_y]),
    ins_entry(u2_xvar_yvar, 149, [f_x,f_y], [label(_)]),
    ins_entry(u2_xvar_xval, 146, [f_x,f_x], [rw(e(u2_xvar_xlval,0),1)]),
    ins_entry(u2_xvar_xlval, 147, [f_x,f_x], [label(r)]),
    ins_entry(u2_xvar_yfval, 150, [f_x,f_y]),
    ins_entry(u2_xvar_yval, 151, [f_x,f_y], [rw(e(u2_xvar_ylval,0),1)]),
    ins_entry(u2_xvar_ylval, 152, [f_x,f_y], [label(r)]),
    ins_entry(u2_yfvar_void, 153, [f_y,f_i]),
    ins_entry(u2_yvar_void, 154, [f_y,f_i], [label(_)]),
    ins_entry(u2_yfvar_xvar, 155, [f_y,f_x]),
    ins_entry(u2_yvar_xvar, 156, [f_y,f_x], [label(_)]),
    ins_entry(u2_yfvar_yvar, 157, [f_y,f_y]),
    ins_entry(u2_yvar_yvar, 158, [f_y,f_y], [label(_)]),
    ins_entry(u2_yfvar_xval, 159, [f_y,f_x], [rw(e(u2_yfvar_xlval,0),1)]),
    ins_entry(u2_yfvar_xlval, 161, [f_y,f_x], [label(r)]),
    ins_entry(u2_yvar_xval, 160, [f_y,f_x], [rw(e(u2_yvar_xlval,0),1),label(w)]),
    ins_entry(u2_yvar_xlval, 162, [f_y,f_x], [label(_)]),
    ins_entry(u2_yfvar_yval, 163, [f_y,f_y], [rw(e(u2_yfvar_ylval,0),1)]),
    ins_entry(u2_yfvar_ylval, 165, [f_y,f_y], [label(r)]),
    ins_entry(u2_yvar_yval, 164, [f_y,f_y], [rw(e(u2_yvar_ylval,0),1),label(w)]),
    ins_entry(u2_yvar_ylval, 166, [f_y,f_y], [label(_)]),
    ins_entry(u2_yfval_void, 185, [f_y,f_i]),
    ins_entry(u2_yfval_xvar, 188, [f_y,f_x]),
    ins_entry(u2_yfval_yfval, 199, [f_y,f_y]),
    ins_entry(u2_yfval_xval, 193, [f_y,f_x], [rw(e(u2_yfval_xlval,0),1)]),
    ins_entry(u2_yfval_xlval, 196, [f_y,f_x], [label(r)]),
    ins_entry(u2_yfval_yval, 202, [f_y,f_y], [rw(e(u2_yfval_ylval,0),1)]),
    ins_entry(u2_yfval_ylval, 205, [f_y,f_y], [label(r)]),
    ins_entry(u2_xval_void, 167, [f_x,f_i], [rw(e(u2_xlval_void,0),1)]),
    ins_entry(u2_xlval_void, 168, [f_x,f_i], [label(r)]),
    ins_entry(u2_xval_xvar, 169, [f_x,f_x], [rw(e(u2_xlval_xvar,0),1)]),
    ins_entry(u2_xlval_xvar, 170, [f_x,f_x], [label(r)]),
    ins_entry(u2_xval_yfvar, 171, [f_x,f_y], [rw(e(u2_xlval_yfvar,0),1)]),
    ins_entry(u2_xlval_yfvar, 172, [f_x,f_y], [label(r)]),
    ins_entry(u2_xval_yvar, 173, [f_x,f_y], [rw(e(u2_xlval_yvar,0),1),label(w)]),
    ins_entry(u2_xlval_yvar, 174, [f_x,f_y], [label(_)]),
    ins_entry(u2_xval_xval, 175, [f_x,f_x], [rw(e(u2_xval_xlval,0),1)]),
    ins_entry(u2_xval_xlval, 177, [f_x,f_x], [rw(e(u2_xlval_xval,0),1),label(r)]),
    ins_entry(u2_xlval_xval, 176, [f_x,f_x], [rw(e(u2_xlval_xlval,0),1),label(r)]),
    ins_entry(u2_xlval_xlval, 178, [f_x,f_x], [label(r)]),
    ins_entry(u2_xval_yfval, 179, [f_x,f_y], [rw(e(u2_xlval_yfval,0),1)]),
    ins_entry(u2_xlval_yfval, 180, [f_x,f_y], [label(r)]),
    ins_entry(u2_xval_yval, 181, [f_x,f_y], [rw(e(u2_xval_ylval,0),1)]),
    ins_entry(u2_xval_ylval, 183, [f_x,f_y], [rw(e(u2_xlval_yval,0),1),label(r)]),
    ins_entry(u2_xlval_yval, 182, [f_x,f_y], [rw(e(u2_xlval_ylval,0),1),label(r)]),
    ins_entry(u2_xlval_ylval, 184, [f_x,f_y], [label(r)]),
    ins_entry(u2_yval_void, 186, [f_y,f_i], [rw(e(u2_ylval_void,0),1)]),
    ins_entry(u2_ylval_void, 187, [f_y,f_i], [label(r)]),
    ins_entry(u2_yval_xvar, 189, [f_y,f_x], [rw(e(u2_ylval_xvar,0),1)]),
    ins_entry(u2_ylval_xvar, 190, [f_y,f_x], [label(r)]),
    ins_entry(u2_yval_yvar, 191, [f_y,f_y], [rw(e(u2_ylval_yvar,0),1)]),
    ins_entry(u2_ylval_yvar, 192, [f_y,f_y], [label(r)]),
    ins_entry(u2_yval_yfval, 200, [f_y,f_y], [rw(e(u2_ylval_yfval,0),1)]),
    ins_entry(u2_ylval_yfval, 201, [f_y,f_y], [label(r)]),
    ins_entry(u2_yval_xval, 194, [f_y,f_x], [rw(e(u2_yval_xlval,0),1)]),
    ins_entry(u2_yval_xlval, 197, [f_y,f_x], [rw(e(u2_ylval_xval,0),1), label(r)]),
    ins_entry(u2_ylval_xval, 195, [f_y,f_x], [rw(e(u2_ylval_xlval,0),1), label(r)]),
    ins_entry(u2_ylval_xlval, 198, [f_y,f_x], [label(r)]),
    ins_entry(u2_yval_yval, 203, [f_y,f_y], [rw(e(u2_yval_ylval,0),1)]),
    ins_entry(u2_yval_ylval, 206, [f_y,f_y], [rw(e(u2_ylval_yval,0),1), label(r)]),
    ins_entry(u2_ylval_yval, 204, [f_y,f_y], [rw(e(u2_ylval_ylval,0),1), label(r)]),
    ins_entry(u2_ylval_ylval, 207, [f_y,f_y], [label(r)]).

iset_misc2 =>
    ins_entry(bump_counter, 249, [f_l], [q0,label(_)]),
    ins_entry(counted_neck, 251, [f_l,f_l], [q0,label(_)]),
    ins_entry(fail, 67, [], exported+[]), % (for ciao_initcode and init_some_bytecode)
    ins_entry(heapmargin_call, 246, [f_g], exported+[q0,label(_)]), % (for compile_term_aux)
    ins_entry(neck, 65, [], [label(_)]),
    ins_entry(dynamic_neck_proceed, 236, [], exported+[in_mode(w),label(w)]),
    ins_entry(neck_proceed, 66, [], [in_mode(w),label(w)]),
    ins_entry(proceed, 64, [], [label(_)]),
    ins_entry(restart_point, 262, [], exported+[optional('PARBACK')]). % (for ciao_initcode and init_some_bytecode)

% Set declarations (globally) from the instruction set
% TODO: hack, we need better control of context
:- [[ collect_and_filter(instruction_set, '$decl'/1, G) ]], '$exec_decls'(G).

