% (included file)

% ---------------------------------------------------------------------------
% Abstract machine definition (emugen version)
% NOTE: See core_OC/engine/absmach_def.pl for ImProlog version

% Author: Jose F. Morales (based on the original code in C)

% Instruction set and auxiliary data structures for the bytecode
% emulator. The specification in this file is translated to C files,
% which are included in the engine runtime code to generate a working
% bytecode emulator.

% ---------------------------------------------------------------------------

% TODO: Make sure that 'pred' with just comp props does not introduce
%   'true' calls assertions.
% TODO:
%  - remove 'unfold' (we must always unfold!)
%  - add loop detection
%  - move fmt:call/n into code generation, support lvalues, rvalues
%  - declare constraints that can be used inside [[ ]]
%  - move more formatting into code generation
%  - multi-level resolution (stop at tokens, etc.)
%    (so that formatting as code generation is practical)
%  - better/alternative search
% TODO: Missing from optim_comp:
%  - automatic alignment (q versions)
%  - instruction merging
%  - instruction specialization

% ---------------------------------------------------------------------------
:- doc(section, "Definition of instruction format types (ftypes)").

% ftype_def(Code, Id, Def)

% f_o opcode
:- ftype_def(f_o, 15, basic("FTYPE_size(f_o)", 8, 8)).
% f_e frame_size
:- ftype_def(f_e, 8, basic("FTYPE_size(f_e)"/*2*/, 8, 8)).
% f_f functor
:- ftype_def(f_f, 9, basic("FTYPE_size(f_f)"/*4*/, 5, 6)).
% f_i count
:- ftype_def(f_i, 10, basic("FTYPE_size(f_i)"/*2*/, 8, 8)).
% f_l long
:- ftype_def(f_l, 11, basic("FTYPE_size(f_l)"/*4*/, 2, 6)).
% f_g lifeinfo
:- ftype_def(f_g, 12, str([f_l, f_i])).
% f_p bytecode pointer
:- ftype_def(f_p, 13, basic("FTYPE_size(f_p)"/*4*/, 3, 3)).
% f_t term
:- ftype_def(f_t, 14, basic("FTYPE_size(f_t)"/*4*/, 6, 6)).
% f_x x operand
:- ftype_def(f_x, 16, basic("FTYPE_size(f_x)"/*2*/, 8, 8)).
% f_y y operand
:- ftype_def(f_y, 17, basic("FTYPE_size(f_y)"/*2*/, 8, 8)).
% f_z y operand, low bit -> unsafe
:- ftype_def(f_z, 18, basic("FTYPE_size(f_z)"/*2*/, 8, 8)).
% f_C C/native code pointer
:- ftype_def(f_C, 5, basic("FTYPE_size(f_C)"/*4*/, 9, 6)).
% f_E predicate pointer
:- ftype_def(f_E, 6, basic("FTYPE_size(f_E)"/*4*/, 7, 6)).
% f_Q pad byte
:- ftype_def(f_Q, 19, basic("FTYPE_size(f_Q)"/*2*/, 8, 8)).
% f_Y ::= <i>{<y>}
:- ftype_def(f_Y, 3, array(f_i, f_y)).
% f_Z ::= <i>{<z>}
:- ftype_def(f_Z, 4, array(f_i, f_z)).
% f_b blob (large number or float) (spec functor and data object)
:- ftype_def(f_b, 7, blob).

% ---------------------------------------------------------------------------
:- doc(section, "C syntax and control constructs").

:- pred(fmtbb/0, [unfold, grammar_level]).
fmtbb :- [[indent(N)]], fmt:bb(N).

:- pred(fmtinc/0, [unfold, grammar_level]).
fmtinc :-
	[[indent(N)]],
	[[N1 is N + 2]],
	[[update(indent(N1))]].
:- pred(fmtdec/0, [unfold, grammar_level]).
fmtdec :-
	[[indent(N)]],
	[[N1 is N - 2]],
	[[update(indent(N1))]].

:- pred(for/2, [unfold, grammar_level]).
for(Range, Code) :-
	"for (", Range, ") {", fmtinc,
	fmt:nl, Code, 
	fmtdec, "}", fmt:nl.

:- pred(do_while/2, [unfold, grammar_level]).
do_while(Code, Cond) :-
	"do {", fmtinc,
	fmt:nl, Code, 
	fmtdec, "} while (", Cond, ");", fmtinc.

:- pred(if/2, [unfold, grammar_level]).
if(Cond, Then) :-
	"if (", Cond, ") {", fmtinc,
	fmt:nl, Then,
	fmtdec, fmtbb, "}", fmt:nl.

:- pred(if/3, [unfold, grammar_level]).
if(Cond, Then, Else) :-
	"if (", Cond, ") {", fmtinc,
	fmt:nl, Then,
	fmtdec, fmtbb, "} else ",
	( is_if(Else), Else
	; not_if(Else), "{", fmtinc,
	  fmt:nl, Else,
	  fmtdec, fmtbb, "}", fmt:nl
	).

:- pred(is_if/1, [unfold, grammar_level]).
is_if(Stmt) :- [[Stmt = if(_,_)]].
is_if(Stmt) :- [[Stmt = if(_,_,_)]].

:- pred(not_if/1, [unfold, grammar_level]).
not_if(Stmt) :- [[Stmt \= if(_,_)]], [[Stmt \= if(_,_,_)]].

:- pred(switch/2, [unfold, grammar_level]).
switch(Expr, Cases) :-
	"switch (", Expr, ") {", fmtinc,
	fmt:nl, Cases, 
	fmtdec, "}", fmtinc.

:- pred(vardecl/2, [unfold, grammar_level]).
vardecl(Type, V) :-
	Type, " ", V, ";", fmt:nl.

:- pred(vardecl/3, [unfold, grammar_level]).
vardecl(Type, V, A) :-
	Type, " ", V, " = ", A, ";", fmt:nl.

:- pred(argdecl/2, [unfold, grammar_level]).
argdecl(Type, V) :-
	Type, " ", V.

:- pred((<-)/2, [unfold, grammar_level]).
(A <- B) :-
	A, " = ", B, ";", fmt:nl.

:- pred(inc/2, [unfold, grammar_level]).
inc(A, B) :-
	[[ B = 1 ]],
	A, "++", ";", fmt:nl.
inc(A, B) :-
	[[ B \= 1 ]],
	A, " += ", B, ";", fmt:nl.

% Assign, except if we self assign (voids warning)
:- pred(assign_noself/2, [unfold, grammar_level]).
assign_noself(U, V) :-
	( [[ U = V ]]
        ; [[ U \= V ]], U <- V
	).

:- pred(label/1, [unfold, grammar_level]).
label(A) :-
	fmt:atom(A), ":", fmt:nl.

:- pred(case/1, [unfold, grammar_level]).
case(A) :-
	"case ", fmt:atom(A), ":", fmt:nl.

:- pred(goto/1, [unfold, grammar_level]).
goto(A) :-
	"goto ", fmt:atom(A), ";", fmt:nl.

:- pred(break/0, [unfold, grammar_level]).
break :-
	"break;", fmt:nl.

:- pred(return/1, [unfold, grammar_level]).
return(A) :-
	"return ", A, ";", fmt:nl.

:- pred(call0/1, [unfold, grammar_level]).
call0(X) :- fmt:atom(X), ";", fmt:nl.

% ---------------------------------------------------------------------------
:- doc(section, "C preprocessor macros").

% C preprocessor

:- pred(cpp_define/2, [unfold, grammar_level]).
cpp_define(Name, Value) :-
	"#define ", fmt:atom(Name), " ", Value, fmt:nl.

:- pred(cpp_if_defined/1, [unfold, grammar_level]).
cpp_if_defined(Name) :-
	"#if defined(", fmt:atom(Name), ")", fmt:nl.

:- pred(cpp_endif/0, [unfold, grammar_level]).
cpp_endif :-
	"#endif", fmt:nl.

% ---------------------------------------------------------------------------
:- doc(section, "Custom control structures").

:- pred(labeled_block/2, [unfold]).
labeled_block(Label, Code) :-
	label(Label),
	Code.

:- pred(sw_on_heap_var/5, [unfold]).
sw_on_heap_var(Reg, Aux, HVACode, CVACode, NVACode) :-
	"SwitchOnHeapVar(", Reg, ",", Aux, ",{", fmt:nl,
	HVACode,
	"},{", fmt:nl,
	CVACode,
	"},{", fmt:nl,
	NVACode,
	"});", fmt:nl.

:- pred(sw_on_var/6, [unfold]).
sw_on_var(Reg, Aux, HVACode, CVACode, SVACode, NVACode) :-
	"SwitchOnVar(", Reg, ",", Aux, ",{", fmt:nl,
	HVACode,
	"},{", fmt:nl,
	CVACode,
	"},{", fmt:nl,
	SVACode,
	"},{", fmt:nl,
	NVACode,
	"});", fmt:nl.

:- pred(deref_sw/3, [unfold]).
deref_sw(Reg, Aux, VarCode) :-
	"DerefSwitch(", Reg, ",", Aux, ", {", fmt:nl,
	VarCode,
	"});", fmt:nl.

:- pred(deref_heap_sw/3, [unfold]).
deref_heap_sw(Reg, Aux, VarCode) :-
	"DerefHeapSwitch(", Reg, ",", Aux, ", {", fmt:nl,
	VarCode,
	"});", fmt:nl.

:- pred(unify_heap_atom/2, [unfold]).
unify_heap_atom(U,V) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, V),
	sw_on_heap_var(T1, T0,
	  bind(hva, T1, U),
	  bind(cva, T1, U),
	  if((T1, "!=", U), goto('fail'))).

:- pred(unify_atom/2, [unfold]).
unify_atom(U,V) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, V),
	sw_on_var(T1, T0,
	  bind(hva, T1, U),
	  bind(cva, T1, U),
	  bind(sva, T1, U),
	  if((T1, "!=", U), goto('fail'))).

:- pred(unify_atom_internal/2, [unfold]).
unify_atom_internal(Atom,Var) :-
	t1(T1),
	assign_noself(T1, Var),
	if((T1, "&", "TagBitSVA"),
	  (bind(sva, T1, Atom)),
	  (bind(hva, T1, Atom))).

:- pred(unify_heap_structure/3, [unfold]).
unify_heap_structure(U,V,Cont) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, V),
	[[mode(M)]],
	sw_on_heap_var(T1, T0,
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(hva, T1, callexp('Tag', ["STR", H])), heap_push(U),
	   Cont),
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(cva, T1, callexp('Tag', ["STR", H])), heap_push(U),
	   Cont),
	  ([[update(mode(M))]],
	   "if(!TagIsSTR(", T1, ") || (TagToHeadfunctor(", T1, ")!=", U, ")) goto fail;",
	   "S" <- callexp('TagToArg', [T1, 1]),
	   Cont)),
	% Make sure that no mode dependant code appears next
	% TODO: better way?
	[[update(mode('?'))]].

:- pred(unify_structure/3, [unfold]).
unify_structure(U,V,Cont) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, V),
	[[mode(M)]],
	sw_on_var(T1, T0,
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(hva, T1, callexp('Tag', ["STR", H])), heap_push(U),
	   Cont),
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(cva, T1, callexp('Tag', ["STR", H])), heap_push(U),
	   Cont),
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(sva, T1, callexp('Tag', ["STR", H])), heap_push(U),
	   Cont),
	  ([[update(mode(M))]],
	   "if(!TagIsSTR(", T1, ") || (TagToHeadfunctor(", T1, ")!=", U, ")) goto fail;",
	   "S" <- callexp('TagToArg', [T1, 1]),
	   Cont)),
	% Make sure that no mode dependant code appears next
	% TODO: better way?
	[[update(mode('?'))]].

:- pred(unify_heap_large/3, [unfold]).
unify_heap_large(ARG,P, T) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, T),
	sw_on_heap_var(T1, T0,
	  bind(hva, T1, callexp('BC_MakeLarge', [ARG, P])),
	  bind(cva, T1, callexp('BC_MakeLarge', [ARG, P])),
	  ("BC_EqLarge(", T1, ",", P, ", {", fmt:nl,
	   goto('fail'),
	   "});", fmt:nl)).

:- pred(unify_large/3, [unfold]).
unify_large(ARG,P, T) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, T),
	sw_on_var(T1, T0,
	  bind(hva, T1, callexp('BC_MakeLarge', [ARG, P])),
	  bind(cva, T1, callexp('BC_MakeLarge', [ARG, P])),
	  bind(sva, T1, callexp('BC_MakeLarge', [ARG, P])),
	  ("BC_EqLarge(", T1, ",", P, ", {", fmt:nl,
	   goto('fail'),
	   "});", fmt:nl)).

:- pred(unify_heap_list/2, [unfold]).
unify_heap_list(V,Cont) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, V),
	[[mode(M)]],
	sw_on_heap_var(T1, T0,
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(hva, T1, callexp('Tag', ["LST", H])),
	   Cont),
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(cva, T1, callexp('Tag', ["LST", H])),
	   Cont),
	  ([[update(mode(M))]],
	   if(("!TermIsLST(", T1, ")"), goto('fail')),
	   "S" <- callexp('TagToLST', [T1]),
	   Cont)),
	% Make sure that no mode dependant code appears next
	% TODO: better way?
	[[update(mode('?'))]].

:- pred(unify_list/2, [unfold]).
unify_list(V,Cont) :-
	t0(T0),
	t1(T1),
	assign_noself(T1, V),
	[[mode(M)]],
	sw_on_var(T1, T0,
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(hva, T1, callexp('Tag', ["LST", H])),
	   Cont),
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(cva, T1, callexp('Tag', ["LST", H])),
	   Cont),
	  ([[update(mode(M))]],
	   setmode(w),
	   cachedreg('H', H),
	   bind(sva, T1, callexp('Tag', ["LST", H])),
	   Cont),
	  ([[update(mode(M))]],
	   if(("!TermIsLST(", T1, ")"), goto('fail')),
	   "S" <- callexp('TagToLST', [T1]),
	   Cont)),
	% Make sure that no mode dependant code appears next
	% TODO: better way?
	[[update(mode('?'))]].

:- pred(unify_local_value/1, [unfold]).
unify_local_value(T1) :-
        if(callexp('TagIsSVA', [T1]),
          do_while(
            (call('RefSVA', ["t0",T1]),
	     if(("t0"," == ",T1), 
	       (cachedreg('H', H),
		bind(sva, T1, callexp('TagHVA', [H])),
		preload(hva, T1),
		break))),
            callexp('TagIsSVA', [(T1,"=","t0")]))),
        heap_push(T1).

% Concurrency: if we cut (therefore discarding intermediate
% choicepoints), make sure we also get rid of the linked chains which
% point to the pending calls to concurrent predicates. (MCL)

% TODO: Bug: the PROFILE__HOOK_CUT should be implemented like show_nodes
%     show_nodes(w->node, w->next_node);

:- pred(do_cut/0, [unfold]).
do_cut :-
        profile_hook(cut),
        call('SetB', ["w->next_node"]),
        "w->node" <- "B",
        "SetShadowregs(B);", fmt:nl,
        "TRACE_CHPT_CUT(w->node);", fmt:nl,
        "ConcChptCleanUp(TopConcChpt, w->node);", fmt:nl.

:- pred(cunify/2, [unfold]).
cunify(U,V) :-
	t0(T0), t1(T1),
	assign_noself(T0, U),
	assign_noself(T1, V),
        if((T0, "!=", T1), 
          if("!cunify(Arg,t0,t1)", goto('fail'))).

% This must not clobber  t2, X[*].  Build goal, store in t3.
:- pred(emul_to_goal/0, [unfold]).
emul_to_goal :-
        if("Func->arity==0", 
          "t3" <- "Func->printname",
	  (cachedreg('H', H),
	   "t3" <- callexp('Tag', ["STR", H]),
	   heap_push("SetArity(Func->printname,Func->arity)"),
	   for("i=0; i<Func->arity; i++",
	     ("t1" <- "X(i)",
	      unify_local_value("t1"))))).

:- pred(deallocate/0, [unfold]).
deallocate :-
	"w->next_insn" <- "E->next_insn",
	"w->frame" <- "E->frame".

% Do not edit this defn - it's the special case 
%   ComputeA(w->local_top,B)
:- pred(compute_Ltop/1, [unfold]).
compute_Ltop(B) :-
	if("w->local_top", ";",
	  if("!StackYounger(w->local_top = NodeLocalTop(B),w->frame)",
	    "w->local_top" <- "StackCharOffset(w->frame,FrameSize(w->next_insn))")).

:- pred(code_neck/0, [unfold]).
code_neck :-
	if("w->next_alt",
	  (do_neck,
	   % OK even before allocate
	   call('SetE', ["w->local_top"]))).

:- pred(code_neck_proceed/0, [unfold]).
code_neck_proceed :-
	if("w->next_alt",
	  do_neck,
	  "w->local_top" <- 0),
	call('SetE', ["w->frame"]),
	"P = w->next_insn;", fmt:nl,
	profile_hook(neck_proceed),
	"goto WriteMode;", fmt:nl.

:- pred(do_neck/0, [unfold]).
do_neck :-
	call('SetB', ["w->node"]),
	if("B->next_alt",
	  % retry
          ("B->next_alt = w->next_alt;", fmt:nl),
	  % try
          ("B->next_alt = w->next_alt; /* 4 contiguous moves */", fmt:nl,
          "B->frame = w->frame;", fmt:nl,
          "B->next_insn = w->next_insn;", fmt:nl,
          "SaveLtop(B);", fmt:nl,
          "i=B->next_alt->node_offset;", fmt:nl,
          if("i>ArityToOffset(0)",
            ("i = OffsetToArity(i);", fmt:nl,
             call('SetB', ["w->next_node"]),
             trace(neck("i")),
	     do_while(
	       "ChoicePush(pt1,(w->term-1)[i]);",
	       "--i"))),
	  maybe_choice_overflow)),
	"w->next_alt = NULL;", fmt:nl.

:- pred(maybe_choice_overflow/0, [unfold]).
maybe_choice_overflow :-
	if(callexp('ChoiceYounger',
	    [callexp('ChoiceOffset', ["B","CHOICEPAD"]),"w->trail_top"]),
	  call('choice_overflow', ["Arg","CHOICEPAD"])).

% ---------------------------------------------------------------------------
:- doc(section, "Auxiliary macro definitions").

% Defs for pairs of UNIFYs

% Local WAM loop function state

:- pred(t0/1, [unfold]).
t0(T) :- [[ T = "t0" ]].
:- pred(t1/1, [unfold]).
t1(T) :- [[ T = "t1" ]].
:- pred(t2/1, [unfold]).
t2(T) :- [[ T = "t2" ]].

% Worker state

:- pred(x/2, [unfold]).
x(Xn,X) :- [[ X = callexp('X', [Xn]) ]].

:- pred(y/2, [unfold]).
y(Yn,Y) :- [[ Y = callexp('Y', [Yn]) ]].

% 'Decoding' a bytecode operand as an expression
:- pred(dec/2, [unfold]).
dec(op(f_f,N),R) :- [[ R = N ]].
dec(op(f_t,N),R) :- [[ R = N ]].
dec(op(f_x,N),R) :- [[ R = callexp('Xb', [N]) ]].
dec(op(f_y,N),R) :- [[ R = callexp('Yb', [N]) ]].
dec(op(f_b,N),R) :- [[ R = ["&",N] ]]. % (a reference to the blob)

% Like dec/2, but uses a temporary variable to cache the P address
:- pred(dectmp/3, [unfold]).
dectmp(Tmp, op(FType,N), R) :-
	Tmp <- N,
	dec(op(FType, Tmp), R).

% Move the program counter to discard an argument
:- pred(shift/1, [unfold]).
shift(f_Q) :- 
        inc("P","FTYPE_size(f_Q)").
shift(f_x) :- 
        inc("P","FTYPE_size(f_x)").
shift(f_y) :- 
        inc("P","FTYPE_size(f_y)").
shift(f_z) :- 
        inc("P","FTYPE_size(f_z)").
shift(f_i) :- 
        inc("P","FTYPE_size(f_i)").

% Jump to a given instruction keeping the same operand stream
:- pred(goto_ins/1, [unfold]).
goto_ins(Ins) :-
	[[mode(M)]],
	[[get_ins_label(Ins, M, Label)]],
	goto(Label).

% Dispatch (jump to next instruction in the selected read/write
% mode). Skips OpsSize items from the operand stream.
:- pred(dispatch/1, [unfold]).
dispatch(OpsSize) :-
	inc("P", OpsSize),
	goto_ins_dispatch.

% Load/store local copies of worker registers
:- pred(regload/1, [unfold]).
regload('H') :- call0('LoadH').

:- pred(regstore/1, [unfold]).
regstore('H') :- call0('StoreH').

:- pred(cachedreg/2, [unfold]).
:- pred(cachedreg(Reg,_), [in_moded('cachedreg/2'(Reg))]).
cachedreg('H',H) :-
	( [[mode(r)]], [[H = "w->global_top"]]
	; [[mode(w)]], [[H = "H"]]
	).

% Switch the read/write mode
:- pred(setmode/1, [unfold]).
setmode(w) :-
	[[mode(r)]],
	regload('H'),
	[[update(mode(w))]].
setmode(r) :-
	[[mode(w)]],
	regstore('H'),
	[[update(mode(r))]].
setmode(M) :-
	[[mode(M)]].

% Switch mode and update H simulateneously
% (this avoids an unnecessary StoreH in w->r switch)
:- pred(setmode_setH/2, [unfold]).
setmode_setH(r, NewH) :-
	[[mode(r)]],
	"w->global_top" <- NewH.
setmode_setH(r, NewH) :-
	[[mode(w)]],
	[[update(mode(r))]],
	"w->global_top" <- NewH.

:- pred(put_yvoid/0, [unfold]).
put_yvoid :-
	"t0" <- "BcP(f_y, 1)",
        shift(f_y),
        load(sva, "Yb(t0)").
	
:- pred(heap_push/1, [unfold]).
heap_push(X) :-
	cachedreg('H', H),
	call('HeapPush', [H, X]).

:- pred(ref_stack/3, [unfold]).
ref_stack(safe, A, B) :-
	call('RefStack', [A,["&",B]]).
ref_stack(unsafe, A, B) :-
	ref_stack_unsafe(A,["&",B]).

% NOTE: this is an expression!
:- pred(unsafe_var_expr/1, [unfold]).
unsafe_var_expr(X) :-
	"(!YoungerStackVar(TagSVA(Offset(E,EToY0)),", X, "))".

% Must return value in t0.  Second arg must not involve t0.
:- pred(ref_stack_unsafe/2, [unfold]).
ref_stack_unsafe(To,From) :-
	call('RefStack', ["t0", From]),
	if(callexp('TagIsSVA', ["t0"]),
	  do_while(
	    ("RefSVA(t1,t0);", fmt:nl,
	     if("t1 == t0",
	       (if(unsafe_var_expr("t0"),
	          (load(hva,"t0"),
	  	   bind(sva, "t1", "t0"))),
	       break))),
	    callexp('TagIsSVA', ["t0=t1"]))),
	To <- "t0".

:- pred(ref_heap_next/1, [unfold]).
ref_heap_next(A) :-
	call('RefHeapNext', [A, "S"]).

:- pred(preload/2, [unfold]).
preload(hva, A) :-
	cachedreg('H', H),
	call('PreLoadHVA', [A, H]).

:- pred(load2/3, [unfold]).
load2(hva, A, B) :-
	cachedreg('H', H),
	call('Load2HVA', [A, B, H]).
load2(sva, A, B) :-
	call('Load2SVA', [A, B]).

:- pred(load/2, [unfold]).
load(hva, A) :-
	cachedreg('H', H),
	call('LoadHVA', [A, H]).
load(sva, A) :-
	call('LoadSVA', [A]).
load(cva, A) :-
	cachedreg('H', H),
	call('LoadCVA', [A, H]).

:- pred(bind/3, [unfold]).
bind(hva, T0, T1) :-
	call('BindHVA', [T0,T1]).
bind(cva, T0, T1) :-
	call('BindCVA', [T0,T1]).
bind(sva, T0, T1) :-
	call('BindSVA', [T0,T1]).

% segfault patch -- jf
% 'U' is a 'Yb(I)' expression.
:- pred(get_first_value/2, [unfold]).
get_first_value(U, V) :-
	if(callexp('CondStackvar', [U]), (
          call('TrailPushCheck', ["w->trail_top",callexp('TagSVA', [("&",U)])]),
	  U <- V
	), (
          U <- V
        )).

:- pred(u1/1, [unfold]).
u1(void(X)) :-
	[[mode(r)]],
	"S" <- call('HeapOffset', ["S", X]).
u1(void(X)) :-
	[[mode(w)]],
	"i" <- ["(FTYPE_ctype(f_i_signed))", X],
	do_while("ConstrHVA(H);", "--i").
u1(var(X)) :-
	[[mode(r)]],
	ref_heap_next(X).
u1(var(X)) :-
	[[mode(w)]],
	load(hva,X).
u1(xval(X)) :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	cunify(X, T1).
u1(yval(Y)) :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	t0(T0),
	ref_stack(safe, T0, Y),
	cunify(T0, T1).
u1(yfval(Y)) :-
	[[mode(r)]],
	t0(T0),
	ref_heap_next(T0),
	get_first_value(Y,T0).
u1(xval(X)) :-
	[[mode(w)]],
	heap_push(X).
u1(yval(Y)) :-
	[[mode(w)]],
	cachedreg('H', H),
	call('HeapPushRefStack', [H,["&",Y]]).
u1(yfval(Y)) :-
	[[mode(w)]],
	t0(T0),
	load(hva,T0),
	get_first_value(Y,T0).
u1(xlval(X)) :-
	[[mode(r)]],
	u1(xval(X)).
u1(ylval(Y)) :-
	[[mode(r)]],
	u1(yval(Y)).
u1(xlval(X)) :-
	[[mode(w)]],
	t1(T1),
	T1 <- X,
	unify_local_value("t1").
u1(ylval(Y)) :-
	[[mode(w)]],
	t1(T1),
	ref_stack(safe, T1, Y),
	unify_local_value("t1").

:- pred(eunify/3, [unfold]).
eunify(U, V, OpsSize) :-
	t0(T0), t1(T1),
	assign_noself(T0, U),
	assign_noself(T1, V),
	inc("P", OpsSize),
	goto('unify_t0_t1').

% u1 + dispatch
:- pred(u1_dispatch/2, [unfold]).
u1_dispatch(xlval(X), OpsSize) :-
	[[mode(r)]],
	u1_dispatch(xval(X), OpsSize).
u1_dispatch(ylval(Y), OpsSize) :-
	[[mode(r)]],
	u1_dispatch(yval(Y), OpsSize).
u1_dispatch(xval(X), OpsSize) :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	eunify(X, T1, OpsSize).
u1_dispatch(yval(Y), OpsSize) :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	t0(T0),
	ref_stack(safe, T0, Y),
	eunify(T0,T1,OpsSize).
u1_dispatch(U, OpsSize) :-
	[[mode(w)]],
	u1(U),
	dispatch(OpsSize).

:- pred(computeE/0, [unfold]).
computeE :- call0('ComputeE').
	
% Emit the initialization of Y variables
:- pred(init_yvars/1, [unfold]).
init_yvars(Count) :-
	t0(T0),
	for((T0, " = ", Count, "-sizeof(tagged_t); ", 
	     T0, " >= EToY0*sizeof(tagged_t); ", 
	     T0, " -= sizeof(tagged_t)"),
	    (dec(op(f_y,T0),Y),
	     load(sva,Y))).

% Emit the code to put a Y argument (which may be 'unsafe')
:- pred(putarg/2, [unfold]).
putarg(Zn,Xn) :-
	x(Xn,X),
	if((Zn, "&", "1"),
	  (dec(op(f_y,(Zn,"+","1")),Y1),
	   ref_stack(unsafe, X, Y1)),
	  (dec(op(f_y,Zn),Y2),
           ref_stack(safe, X, Y2))).

% Wrapper for execution of instruction G in the specified mode M
:- pred(in_mode/3, [unfold]).
in_mode(M, _, G) :- [[mode(M)]], G.
in_mode(M, G, _) :- [[mode(M2)]], [[M \= M2]],
	setmode(M),
	goto_ins(G).

% Pre-registered atoms
:- pred(get_atom/2, [unfold]).
get_atom([], X) :- [[ X = "atom_nil" ]].

% ---------------------------------------------------------------------------

:- doc(section, "Declaration of instructions").

:- pred(ins_op_format/3, [unfold_decl]).
ins_op_format(Ins, Op, Format) :-
	add(pred_prop(Ins, ins_op(Op))),
	add(pred_prop(Ins, format(Format))),
	update_max_op(Op),
	update_op_ins(Op, Ins).

:- pred(ins_op_format/4, [unfold_decl]).
ins_op_format(Ins, Op, Format, [label(M)]) :-
	add(pred_prop(Ins, ins_op(Op))),
	add(pred_prop(Ins, format(Format))),
	add(pred_prop(Ins, label(M))),
	update_max_op(Op),
	update_op_ins(Op, Ins).
ins_op_format(Ins, Op, Format, [optional(Name)]) :-
	add(pred_prop(Ins, ins_op(Op))),
	add(pred_prop(Ins, format(Format))),
	add(pred_prop(Ins, optional(Name))),
	update_max_op(Op),
	update_op_ins(Op, Ins).

% Ins instruction always switches to mode Mode
:- pred(ins_in_mode/2, [unfold_decl]).
ins_in_mode(Ins, Mode) :-
	add(pred_prop(Ins, in_mode(Mode))).

% ---------------------------------------------------------------------------
:- doc(section, "Definition of the instruction set").

:- ins_op_format(inittrue, 260, [f_e], [label(w)]).
:- ins_in_mode(inittrue, w).
inittrue :-
	computeE,
	init_yvars("BcP(f_e, 1)"),
	goto('firsttrue').

:- ins_op_format(firsttrue_n, 261, [f_Y,f_e], [label(w)]).
:- ins_in_mode(firsttrue_n, w).
firsttrue_n :-
	"i" <- ["(FTYPE_ctype(f_i_signed))", "BcP(f_i, 1)"],
        shift(f_i),
	for("; i>0; --i", put_yvoid),
	goto('firsttrue'),
	label('firsttrue'),
	"E->next_insn" <- "w->next_insn",
	"E->frame" <- "w->frame",
	"w->frame" <- "E",
	"w->next_insn" <- "PoffR(2)",
	"w->local_top" <- callexp('StackCharOffset', ["E","BcP(f_e, 1)"]),
	if(callexp('OffStacktop',["E","Stack_Warn"]),
          call0('SetEvent')),
	dispatch("FTYPE_size(f_i)").

:- ins_op_format(initcallq, 0, [f_Q,f_E,f_e]).
initcallq :- shift(f_Q), goto_ins(initcall).

:- ins_op_format(initcall, 1, [f_E,f_e], [label(_)]).
:- ins_in_mode(initcall, w).
initcall :-
	computeE,
	init_yvars("BcP(f_e,3)"),
	goto_ins(firstcall).

:- ins_op_format(firstcall_nq, 20, [f_Q,f_Y,f_E,f_e]).
firstcall_nq :- shift(f_Q), goto_ins(firstcall_n).

:- ins_op_format(firstcall_n, 21, [f_Y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_n, w).
firstcall_n :-
	"i" <- ["(FTYPE_ctype(f_i_signed))", "BcP(f_i, 1)"],
        shift(f_i),
	for("; i>8; --i", put_yvoid),
	goto_ins(firstcall_8).

:- ins_op_format(firstcall_8q, 18, [f_Q,f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e]).
firstcall_8q :- shift(f_Q), goto_ins(firstcall_8).

:- ins_op_format(firstcall_8, 19, [f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_8, w).
firstcall_8 :- put_yvoid, goto_ins(firstcall_7).

:- ins_op_format(firstcall_7q, 16, [f_Q,f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e]).
firstcall_7q :- shift(f_Q), goto_ins(firstcall_7).

:- ins_op_format(firstcall_7, 17, [f_y,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_7, w).
firstcall_7 :- put_yvoid, goto_ins(firstcall_6).

:- ins_op_format(firstcall_6q, 14, [f_Q,f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e]).
firstcall_6q :- shift(f_Q), goto_ins(firstcall_6).

:- ins_op_format(firstcall_6, 15, [f_y,f_y,f_y,f_y,f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_6, w).
firstcall_6 :- put_yvoid, goto_ins(firstcall_5).

:- ins_op_format(firstcall_5q, 12, [f_Q,f_y,f_y,f_y,f_y,f_y,f_E,f_e]).
firstcall_5q :- shift(f_Q), goto_ins(firstcall_5).

:- ins_op_format(firstcall_5, 13, [f_y,f_y,f_y,f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_5, w).
firstcall_5 :- put_yvoid, goto_ins(firstcall_4).

:- ins_op_format(firstcall_4q, 10, [f_Q,f_y,f_y,f_y,f_y,f_E,f_e]).
firstcall_4q :- shift(f_Q), goto_ins(firstcall_4).

:- ins_op_format(firstcall_4, 11, [f_y,f_y,f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_4, w).
firstcall_4 :- put_yvoid, goto_ins(firstcall_3).

:- ins_op_format(firstcall_3q, 8, [f_Q,f_y,f_y,f_y,f_E,f_e]).
firstcall_3q :- shift(f_Q), goto_ins(firstcall_3).

:- ins_op_format(firstcall_3, 9, [f_y,f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_3, w).
firstcall_3 :- put_yvoid, goto_ins(firstcall_2).

:- ins_op_format(firstcall_2q, 6, [f_Q,f_y,f_y,f_E,f_e]).
firstcall_2q :- shift(f_Q), goto_ins(firstcall_2).

:- ins_op_format(firstcall_2, 7, [f_y,f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_2, w).
firstcall_2 :- put_yvoid, goto_ins(firstcall_1).

:- ins_op_format(firstcall_1q, 4, [f_Q,f_y,f_E,f_e]).
firstcall_1q :- shift(f_Q), goto_ins(firstcall_1).

:- ins_op_format(firstcall_1, 5, [f_y,f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall_1, w).
firstcall_1 :- put_yvoid, goto_ins(firstcall).

:- ins_op_format(firstcallq, 2, [f_Q,f_E,f_e]).
firstcallq :- shift(f_Q), goto_ins(firstcall).

:- ins_op_format(firstcall, 3, [f_E,f_e], [label(_)]).
:- ins_in_mode(firstcall, w).
firstcall :-
	"E->next_insn" <- "w->next_insn",
	"E->frame" <- "w->frame",
	"w->frame" <- "E",
	"w->next_insn" <- "BCoff(P, FTYPE_size(f_E)+FTYPE_size(f_e))",
	"w->local_top" <- "StackCharOffset(E,BcP(f_e,3))",
	"P" <- "BcP(f_p, 1)",
	if(callexp('OffStacktop',["E","Stack_Warn"]),
	  call0('SetEvent')),
	goto('enter_predicate').

:- ins_op_format(call_nq, 40, [f_Q,f_Z,f_E,f_e]).
call_nq :- shift(f_Q), goto_ins(call_n).

:- ins_op_format(call_n, 41, [f_Z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_n, w).
call_n :-
	"i" <- ["(FTYPE_ctype(f_i_signed))", "BcP(f_i, 1)"],
        shift(f_i),
	for("; i>8; --i",
	  (t1(T1),
	   T1 <- "BcP(f_z, 1)",
	   shift(f_z),
	   putarg(T1,"i-1"))),
	goto_ins(call_8).

:- ins_op_format(call_8q, 38, [f_Q,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e]).
call_8q :- shift(f_Q), goto_ins(call_8).

:- ins_op_format(call_8, 39, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_8, w).
call_8 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,7),
	goto_ins(call_7).

:- ins_op_format(call_7q, 36, [f_Q,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e]).
call_7q :- shift(f_Q), goto_ins(call_7).

:- ins_op_format(call_7, 37, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_7, w).
call_7 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,6),
	goto_ins(call_6).

:- ins_op_format(call_6q, 34, [f_Q,f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e]).
call_6q :- shift(f_Q), goto_ins(call_6).

:- ins_op_format(call_6, 35, [f_z,f_z,f_z,f_z,f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_6, w).
call_6 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,5),
	goto_ins(call_5).

:- ins_op_format(call_5q, 32, [f_Q,f_z,f_z,f_z,f_z,f_z,f_E,f_e]).
call_5q :- shift(f_Q), goto_ins(call_5).

:- ins_op_format(call_5, 33, [f_z,f_z,f_z,f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_5, w).
call_5 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,4),
	goto_ins(call_4).

:- ins_op_format(call_4q, 30, [f_Q,f_z,f_z,f_z,f_z,f_E,f_e]).
call_4q :- shift(f_Q), goto_ins(call_4).

:- ins_op_format(call_4, 31, [f_z,f_z,f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_4, w).
call_4 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,3),
	goto_ins(call_3).

:- ins_op_format(call_3q, 28, [f_Q,f_z,f_z,f_z,f_E,f_e]).
call_3q :- shift(f_Q), goto_ins(call_3).

:- ins_op_format(call_3, 29, [f_z,f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_3, w).
call_3 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,2),
	goto_ins(call_2).

:- ins_op_format(call_2q, 26, [f_Q,f_z,f_z,f_E,f_e]).
call_2q :- shift(f_Q), goto_ins(call_2).

:- ins_op_format(call_2, 27, [f_z,f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_2, w).
call_2 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,1),
	goto_ins(call_1).

:- ins_op_format(call_1q, 24, [f_Q,f_z,f_E,f_e]).
call_1q :- shift(f_Q), goto_ins(call_1).

:- ins_op_format(call_1, 25, [f_z,f_E,f_e], [label(_)]).
:- ins_in_mode(call_1, w).
call_1 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,0),
	goto_ins(call).

:- ins_op_format(callq, 22, [f_Q,f_E,f_e]).
callq :- shift(f_Q), goto_ins(call).

:- ins_op_format(call, 23, [f_E,f_e], [label(_)]).
:- ins_in_mode(call, w).
call :-
	"w->next_insn" <- "BCoff(P, FTYPE_size(f_E)+FTYPE_size(f_e))",
	"P" <- "BcP(f_p, 1)",
	goto('enter_predicate').

:- ins_op_format(lastcall_nq, 60, [f_Q,f_Z,f_E]).
lastcall_nq :- shift(f_Q), goto_ins(lastcall_n).

:- ins_op_format(lastcall_n, 61, [f_Z,f_E], [label(_)]).
:- ins_in_mode(lastcall_n, w).
lastcall_n :-
	"i" <- ["(FTYPE_ctype(f_i_signed))", "BcP(f_i, 1)"],
        shift(f_i),
	for("; i>8; --i",
	  (t1(T1),
	   T1 <- "BcP(f_z, 1)",
	   shift(f_z),
	   putarg(T1,"i-1"))),
	goto_ins(lastcall_8).

:- ins_op_format(lastcall_8q, 58, [f_Q,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E]).
lastcall_8q :- shift(f_Q), goto_ins(lastcall_8).

:- ins_op_format(lastcall_8, 59, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_8, w).
lastcall_8 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,7),
	goto_ins(lastcall_7).

:- ins_op_format(lastcall_7q, 56, [f_Q,f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E]).
lastcall_7q :- shift(f_Q), goto_ins(lastcall_7).

:- ins_op_format(lastcall_7, 57, [f_z,f_z,f_z,f_z,f_z,f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_7, w).
lastcall_7 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,6),
	goto_ins(lastcall_6).

:- ins_op_format(lastcall_6q, 54, [f_Q,f_z,f_z,f_z,f_z,f_z,f_z,f_E]).
lastcall_6q :- shift(f_Q), goto_ins(lastcall_6).

:- ins_op_format(lastcall_6, 55, [f_z,f_z,f_z,f_z,f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_6, w).
lastcall_6 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,5),
	goto_ins(lastcall_5).

:- ins_op_format(lastcall_5q, 52, [f_Q,f_z,f_z,f_z,f_z,f_z,f_E]).
lastcall_5q :- shift(f_Q), goto_ins(lastcall_5).

:- ins_op_format(lastcall_5, 53, [f_z,f_z,f_z,f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_5, w).
lastcall_5 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,4),
	goto_ins(lastcall_4).

:- ins_op_format(lastcall_4q, 50, [f_Q,f_z,f_z,f_z,f_z,f_E]).
lastcall_4q :- shift(f_Q), goto_ins(lastcall_4).

:- ins_op_format(lastcall_4, 51, [f_z,f_z,f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_4, w).
lastcall_4 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,3),
	goto_ins(lastcall_3).

:- ins_op_format(lastcall_3q, 48, [f_Q,f_z,f_z,f_z,f_E]).
lastcall_3q :- shift(f_Q), goto_ins(lastcall_3).

:- ins_op_format(lastcall_3, 49, [f_z,f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_3, w).
lastcall_3 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,2),
	goto_ins(lastcall_2).

:- ins_op_format(lastcall_2q, 46, [f_Q,f_z,f_z,f_E]).
lastcall_2q :- shift(f_Q), goto_ins(lastcall_2).

:- ins_op_format(lastcall_2, 47, [f_z,f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_2, w).
lastcall_2 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,1),
	goto_ins(lastcall_1).

:- ins_op_format(lastcall_1q, 44, [f_Q,f_z,f_E]).
lastcall_1q :- shift(f_Q), goto_ins(lastcall_1).

:- ins_op_format(lastcall_1, 45, [f_z,f_E], [label(_)]).
:- ins_in_mode(lastcall_1, w).
lastcall_1 :-
	t1(T1),
	T1 <- "BcP(f_z, 1)",
	shift(f_z),
	putarg(T1,0),
	goto_ins(lastcall).

:- ins_op_format(lastcallq, 42, [f_Q,f_E]).
lastcallq :- shift(f_Q), goto_ins(lastcall).

:- ins_op_format(lastcall, 43, [f_E], [label(_)]).
:- ins_in_mode(lastcall, w).
lastcall :-
	deallocate,
	goto_ins(execute).

:- ins_op_format(executeq, 62, [f_Q,f_E]).
executeq :-
	( [[mode(r)]], setmode(w)
	; [[mode(w)]]
	),
	"P" <- "BcP(f_p, 2)",
	goto('enter_predicate').

:- ins_op_format(execute, 63, [f_E], [label(w)]).
execute :-
	( [[mode(r)]], setmode(w)
	; [[mode(w)]]
	),
	"P" <- "BcP(f_p, 1)",
	goto('enter_predicate').

:- ins_op_format(put_x_void, 69, [f_x], [label(w)]).
:- ins_in_mode(put_x_void, w).
put_x_void :-
	dec(op(f_x,"BcP(f_x, 1)"),X),
	load(hva,X),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(put_x_variable, 70, [f_x,f_x], [label(w)]).
:- ins_in_mode(put_x_variable, w).
put_x_variable :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	load2(hva, A, B),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(put_xval_xval, 85, [f_x,f_x,f_x,f_x]).
put_xval_xval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	dec(op(f_x,"BcP(f_x, 3)"),C),
	dec(op(f_x,"BcP(f_x, 4)"),D),
	A <- B,
	C <- D,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(put_x_value, 71, [f_x,f_x]).
put_x_value :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	A <- B,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(put_x_unsafe_value, 72, [f_x,f_x], [label(w)]).
:- ins_in_mode(put_x_unsafe_value, w).
put_x_unsafe_value :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	ref_stack(unsafe,A,B),
	t0(T0),
	B <- T0,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(put_y_first_variable, 73, [f_x,f_y], [label(w)]).
:- ins_in_mode(put_y_first_variable, w).
put_y_first_variable :-
	computeE,
	goto_ins(put_y_variable).

:- ins_op_format(put_y_variable, 74, [f_x,f_y], [label(w)]).
:- ins_in_mode(put_y_variable, w).
put_y_variable :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	t0(T0),
	dectmp(T0, op(f_y,"BcP(f_y, 2)"), B),
	load2(sva, A, B),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_yfvar_yvar, 83, [f_x,f_y,f_x,f_y], [label(w)]).
:- ins_in_mode(put_yfvar_yvar, w).
put_yfvar_yvar :-
	computeE,
	goto_ins(put_yvar_yvar).

:- ins_op_format(put_yvar_yvar, 84, [f_x,f_y,f_x,f_y], [label(w)]).
:- ins_in_mode(put_yvar_yvar, w).
put_yvar_yvar :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	t0(T0),
	dectmp(T0, op(f_y,"BcP(f_y, 2)"), B),
	load2(sva, A, B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dectmp(T0, op(f_y,"BcP(f_y, 4)"), D),
	load2(sva, C, D),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y)+FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_yval_yval, 86, [f_x,f_y,f_x,f_y]).
put_yval_yval :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	ref_stack(safe,A,B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dec(op(f_y,"BcP(f_y, 4)"), D),
	ref_stack(safe,C,D),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y)+FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_y_value, 75, [f_x,f_y]).
put_y_value :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	ref_stack(safe,A,B),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_y_unsafe_value, 76, [f_x,f_y], [label(w)]).
:- ins_in_mode(put_y_unsafe_value, w).
put_y_unsafe_value :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	ref_stack(unsafe,A,B),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_constantq, 77, [f_Q,f_x,f_t]).
put_constantq :-
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_t,"BcP(f_t, 3)"), B),
	A <- B,
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x))+FTYPE_size(f_t)").

:- ins_op_format(put_constant, 78, [f_x,f_t]).
put_constant :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_t,"BcP(f_t, 2)"), B),
	A <- B,
	dispatch("FTYPE_size(f_x)+FTYPE_size(f_t)").

:- ins_op_format(put_nil, 81, [f_x]).
put_nil :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	get_atom([], Nil),
	A <- Nil,
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(put_largeq, 252, [f_Q,f_x,f_b], [label(w)]).
:- ins_in_mode(put_largeq, w).
put_largeq :-
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_b,"BcP(f_t, 3)"), B),
	[[mode(M)]],
	setmode(r),
	A <- callexp('BC_MakeLarge', ["Arg",B]),
	setmode(M),
	dispatch(("(FTYPE_size(f_Q)+FTYPE_size(f_x))","+",callexp('LargeSize',["BcP(f_t, 3)"]),"")).

:- ins_op_format(put_large, 253, [f_x,f_b], [label(w)]).
:- ins_in_mode(put_large, w).
put_large :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_b,"BcP(f_t, 2)"), B),
	[[mode(M)]],
	setmode(r),
	A <- callexp('BC_MakeLarge', ["Arg",B]),
	setmode(M),
	dispatch(("FTYPE_size(f_x)","+",callexp('LargeSize',["BcP(f_t, 2)"]),"")).

:- ins_op_format(put_structureq, 79, [f_Q,f_x,f_f], [label(w)]).
:- ins_in_mode(put_structureq, w).
put_structureq :-
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_f,"BcP(f_f, 3)"), B),
	cachedreg('H', H),
	A <- callexp('Tag', ["STR",H]),
	heap_push(B),
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x))+FTYPE_size(f_f)").

:- ins_op_format(put_structure, 80, [f_x,f_f], [label(w)]).
:- ins_in_mode(put_structure, w).
put_structure :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	cachedreg('H', H),
	A <- callexp('Tag', ["STR",H]),
	dec(op(f_f,"BcP(f_f, 2)"), B),
	heap_push(B),
	dispatch("FTYPE_size(f_x)+FTYPE_size(f_f)").

:- ins_op_format(put_list, 82, [f_x], [label(w)]).
:- ins_in_mode(put_list, w).
put_list :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	cachedreg('H', H),
	A <- callexp('Tag', ["LST",H]),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(put_yval_yuval, 87, [f_x,f_y,f_x,f_y], [label(w)]).
:- ins_in_mode(put_yval_yuval, w).
put_yval_yuval :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	ref_stack(safe,A,B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dec(op(f_y,"BcP(f_y, 4)"), D),
	ref_stack(unsafe,C,D),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y)+FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_yuval_yval, 88, [f_x,f_y,f_x,f_y], [label(w)]).
:- ins_in_mode(put_yuval_yval, w).
put_yuval_yval :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	ref_stack(unsafe,A,B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dec(op(f_y,"BcP(f_y, 4)"), D),
	ref_stack(safe,C,D),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y)+FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(put_yuval_yuval, 89, [f_x,f_y,f_x,f_y], [label(w)]).
:- ins_in_mode(put_yuval_yuval, w).
put_yuval_yuval :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	ref_stack(unsafe,A,B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dec(op(f_y,"BcP(f_y, 4)"), D),
	ref_stack(unsafe,C,D),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y)+FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(get_x_value, 91, [f_x,f_x], [label(r)]).
:- ins_in_mode(get_x_value, r).
get_x_value :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	eunify(B,A,"(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(get_y_first_value, 94, [f_x,f_y], [label(r)]).
:- ins_in_mode(get_y_first_value, r).
get_y_first_value :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	get_first_value(B,A),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(get_y_value, 95, [f_x,f_y], [label(r)]).
:- ins_in_mode(get_y_value, r).
get_y_value :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	t1(T1),
	ref_stack(safe,T1,B),
	eunify(A,T1,"(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(get_constantq, 96, [f_Q,f_x,f_t]).
get_constantq :- shift(f_Q), goto_ins(get_constant).

:- ins_op_format(get_constant, 97, [f_x,f_t], [label(_)]).
:- ins_in_mode(get_constant, r).
get_constant :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_t,"BcP(f_t, 2)"), B),
	unify_atom(B,A),
	dispatch("FTYPE_size(f_x)+FTYPE_size(f_t)").

:- ins_op_format(get_largeq, 254, [f_Q,f_x,f_b]).
get_largeq :- shift(f_Q), goto_ins(get_large).

:- ins_op_format(get_large, 255, [f_x,f_b], [label(_)]).
:- ins_in_mode(get_large, r).
get_large :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_b,"BcP(f_t, 2)"), B),
	unify_large("Arg",B,A),
	dispatch(("FTYPE_size(f_x)","+",callexp('LargeSize',["BcP(f_t, 2)"]),"")).

:- ins_op_format(get_structureq, 98, [f_Q,f_x,f_f]).
get_structureq :- shift(f_Q), goto_ins(get_structure).

:- ins_op_format(get_structure, 99, [f_x,f_f], [label(_)]).
:- ins_in_mode(get_structure, r).
get_structure :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_f,"BcP(f_f, 2)"), B),
	unify_structure(B,A,dispatch("FTYPE_size(f_x)+FTYPE_size(f_f)")).

:- ins_op_format(get_nil, 100, [f_x], [label(r)]).
:- ins_in_mode(get_nil, r).
get_nil :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	get_atom([], Nil),
	unify_atom(Nil,A),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(get_list, 101, [f_x], [label(r)]).
:- ins_in_mode(get_list, r).
get_list :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	unify_list(A, dispatch("FTYPE_size(f_x)")).

:- ins_op_format(get_constant_neck_proceedq, 111, [f_Q,f_x,f_t]).
get_constant_neck_proceedq :- shift(f_Q), goto_ins(get_constant_neck_proceed).

:- ins_op_format(get_constant_neck_proceed, 112, [f_x,f_t], [label(_)]).
:- ins_in_mode(get_constant_neck_proceed, r).
get_constant_neck_proceed :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_t,"BcP(f_t, 2)"), B),
	unify_atom(B,A),
	setmode(w),
	goto_ins(neck_proceed).

:- ins_op_format(get_nil_neck_proceed, 113, [f_x], [label(r)]).
:- ins_in_mode(get_nil_neck_proceed, r).
get_nil_neck_proceed :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	get_atom([], Nil),
	unify_atom(Nil,A),
	setmode(w),
	goto_ins(neck_proceed).

:- ins_op_format(cutb_x, 208, [f_x], [label(r)]).
:- ins_in_mode(cutb_x, r).
cutb_x :-
	"w->local_top" <- "0", % may get hole at top of local stack
	dec(op(f_x,"BcP(f_x, 1)"), A),
	"w->next_node" <- call('ChoiceFromInt', [A]),
	do_cut,
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(cutb_x_neck, 210, [f_x], [label(r)]).
:- ins_in_mode(cutb_x_neck, r).
cutb_x_neck :-
	"w->local_top" <- "0", % may get hole at top of local stack
	dec(op(f_x,"BcP(f_x, 1)"),A),
	"w->next_node" <- call('ChoiceFromInt', [A]),
        shift(f_x),
	goto_ins(cutb_neck).

:- ins_op_format(cutb_neck, 211, [], [label(r)]).
:- ins_in_mode(cutb_neck, r).
cutb_neck :-
	do_cutb_neck,
	dispatch("0").

:- ins_op_format(cutb_x_neck_proceed, 212, [f_x], [label(r)]).
:- ins_in_mode(cutb_x_neck_proceed, r).
cutb_x_neck_proceed :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	"w->next_node" <- call('ChoiceFromInt', [A]),
	% shift(f_x)
	% w->local_top <- 0 % done by CODE_PROCEED
	goto_ins(cutb_neck_proceed).

:- ins_op_format(cutb_neck_proceed, 213, [], [label(r)]).
:- ins_in_mode(cutb_neck_proceed, r).
cutb_neck_proceed :-
	do_cutb_neck,
	goto_ins(proceed).

:- pred(do_cutb_neck/0, [unfold]).
do_cutb_neck :-
	do_cut,
	if("w->next_alt",
	  ("w->next_alt" <- "NULL",
	   % TODO: if neck is not pending, then choice overflow has already been checked?
	   maybe_choice_overflow)).

:- ins_op_format(cute_x, 214, [f_x], [label(r)]).
:- ins_in_mode(cute_x, r).
cute_x :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	"w->next_node" <- call('ChoiceFromInt', [A]),
	"w->local_top" <- "E", % w->local_top may be 0 here
	do_cut,
	call('SetE', ["w->local_top"]),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(cute_x_neck, 216, [f_x], [label(r)]).
:- ins_in_mode(cute_x_neck, r).
cute_x_neck :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	"w->next_node" <- call('ChoiceFromInt', [A]),
        shift(f_x),
	goto_ins(cute_neck).

:- ins_op_format(cute_neck, 217, [], [label(r)]).
:- ins_in_mode(cute_neck, r).
cute_neck :-
	"w->local_top" <- "E", %  w->local_top may be 0 here.
	do_cut,
	% w->next_alt can't be NULL here
	"w->next_alt" <- "NULL",
	if(callexp('ChoiceYounger', [callexp('ChoiceOffset', ["B","CHOICEPAD"]),"w->trail_top"]),
	  call('choice_overflow', ["Arg","CHOICEPAD"])),
	call('SetE', ["w->local_top"]),
	dispatch("0").

:- ins_op_format(cutf_x, 215, [f_x], [label(r)]).
:- ins_in_mode(cutf_x, r).
cutf_x :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	"w->next_node" <- call('ChoiceFromInt', [A]),
        shift(f_x),
	goto_ins(cutf).

:- ins_op_format(cutf, 209, [], [label(r)]).
:- ins_in_mode(cutf, r).
cutf :-
	do_cut,
	call('SetE', ["w->frame"]),
	dispatch("0").

:- ins_op_format(cut_y, 218, [f_y], [label(r)]).
:- ins_in_mode(cut_y, r).
cut_y :-
	dec(op(f_y,"BcP(f_y, 1)"), A),
	t1(T1),
	ref_stack(safe,T1,A),
	"w->next_node" <- callexp('ChoiceFromInt', [T1]),
	do_cut,
	call('SetE', ["w->frame"]),
	dispatch("FTYPE_size(f_y)").

:- ins_op_format(choice_x, 219, [f_x]).
choice_x :-
	dec(op(f_x,"BcP(f_x, 1)"), X),
	X <- callexp('ChoiceToInt', ["w->next_node"]),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(choice_yf, 220, [f_y]).
choice_yf :-
	computeE,
	goto_ins(choice_y).

:- ins_op_format(choice_y, 221, [f_y], [label(_)]).
choice_y :-
	dec(op(f_y,"BcP(f_y, 1)"), Y),
	Y <- callexp('ChoiceToInt', ["w->next_node"]),
	dispatch("FTYPE_size(f_y)").

:- ins_op_format(kontinue, 233, [], [label(w)]).
:- ins_in_mode(kontinue, w).
kontinue :-
	% after wakeup, write mode!
	y("0",Y0),
	call('Setfunc', [callexp('TagToFunctor', [Y0])]),
	for("i=0; i<Func->arity; i++", (
          x("i",Xi),
          y("i+1",Yi1),
          Xi <- Yi1)),
	deallocate,
	goto('enter_predicate').

:- ins_op_format(leave, 234, [], [label(r)]).
:- ins_in_mode(leave, r).
leave :-
	goto_ins(exit_toplevel).

:- ins_op_format(exit_toplevel, 235, [], [label(r)]).
:- ins_in_mode(exit_toplevel, r).
exit_toplevel :-
	goto('exit_toplevel').

:- ins_op_format(retry_cq, 237, [f_Q,f_C], [label(r)]).
:- ins_in_mode(retry_cq, r).
retry_cq :-
	if("w->next_alt",
	  ("B->next_alt" <- "w->next_alt",
	   "w->next_alt" <- "NULL")),
	if(("!","(BcP(f_Cb, 2))(Arg)"), goto('fail')),
	goto_ins(proceed).

:- ins_op_format(retry_c, 238, [f_C], [label(r)]).
:- ins_in_mode(retry_c, r).
retry_c :-
	if("w->next_alt",
	  ("B->next_alt" <- "w->next_alt",
	   "w->next_alt" <- "NULL")),
	if(("!","(BcP(f_Cb, 1))(Arg)"), goto('fail')),
	goto_ins(proceed).

% _x0 instructions, where read-mode match has been done during indexing

:- ins_op_format(get_structure_x0q, 104, [f_Q,f_f]).
get_structure_x0q :-
	[[mode(r)]],
	t0(T0),
	"S" <- callexp('TagToArg', [T0, "1"]),
	dispatch("FTYPE_size(f_Q)+FTYPE_size(f_f)").
get_structure_x0q :-
	[[mode(w)]], 
	shift(f_Q), goto_ins(get_structure_x0).

:- ins_op_format(get_structure_x0, 105, [f_f], [label(w)]).
get_structure_x0 :-
	[[mode(r)]],
	t0(T0),
	"S" <- callexp('TagToArg', [T0, "1"]),
	dispatch("FTYPE_size(f_f)").
get_structure_x0 :-
	[[mode(w)]],
	t1(T1),
	cachedreg('H', H),
	T1 <- callexp('Tag', ["STR",H]),
	t0(T0),
	if(callexp('TagIsHVA', [T0]),
	  bind(hva,T0,T1),
	  if((T0," & ", "TagBitSVA"),
	    bind(sva,T0,T1),
	    bind(cva,T0,T1))),
	dec(op(f_f,"BcP(f_f, 1)"),A),
	heap_push(A),
	dispatch("FTYPE_size(f_f)").

:- ins_op_format(get_large_x0q, 256, [f_Q,f_b]).
get_large_x0q :-
	[[mode(r)]],
	t0(T0),
	dec(op(f_b,"BcP(f_t, 2)"), A),
	unify_large("Arg",A,T0),
	dispatch(("FTYPE_size(f_x)","+",callexp('LargeSize',["BcP(f_t, 2)"]),"")).
get_large_x0q :-
	[[mode(w)]],
	shift(f_Q), goto_ins(get_large_x0).

:- ins_op_format(get_large_x0, 257, [f_b], [label(w)]).
get_large_x0 :-
	[[mode(r)]],
	t0(T0),
	dec(op(f_b,"BcP(f_t, 1)"), A),
	unify_large("Arg",A,T0),
	dispatch((callexp('LargeSize',["BcP(f_t, 1)"]),"")).
get_large_x0 :-
	[[mode(w)]],
	setmode(r),
	t1(T1),
	T1 <- callexp('BC_MakeLarge', ["Arg",["&","BcP(f_p, 1)"]]),
	setmode(w),
	t0(T0),
	if(callexp('TagIsHVA', [T0]),
	  bind(hva,T0,T1),
	  if((T0," & ", "TagBitSVA"),
	    bind(sva,T0,T1),
	    bind(cva,T0,T1))),
	dispatch((callexp('LargeSize',["BcP(f_t, 1)"]),"")).

:- ins_op_format(get_constant_x0q, 102, [f_Q,f_t]).
get_constant_x0q :-
	[[mode(r)]],
	dispatch("FTYPE_size(f_Q)+FTYPE_size(f_t)").
get_constant_x0q :-
	[[mode(w)]],
	shift(f_Q), goto_ins(get_constant_x0).

:- ins_op_format(get_constant_x0, 103, [f_t], [label(w)]).
get_constant_x0 :-
	[[mode(r)]],
	dispatch("FTYPE_size(f_t)").
get_constant_x0 :-
	[[mode(w)]],
	t0(T0),
	dec(op(f_t,"BcP(f_t, 1)"), A),
	if(callexp('TagIsHVA', [T0]),
	  bind(hva,T0,A),
	  if((T0," & ", "TagBitSVA"),
	    bind(sva,T0,A),
	    bind(cva,T0,A))),
	dispatch("FTYPE_size(f_t)").

:- ins_op_format(get_nil_x0, 106, []).
get_nil_x0 :-
	[[mode(r)]],
	dispatch("0").
get_nil_x0 :-
	[[mode(w)]],
	t0(T0),
	get_atom([], Nil),
	if(callexp('TagIsHVA', [T0]),
	  bind(hva,T0,Nil),
	  if((T0," & ", "TagBitSVA"),
	    bind(sva,T0,Nil),
	    bind(cva,T0,Nil))),
	dispatch("0").

:- ins_op_format(get_list_x0, 107, []).
get_list_x0 :-
	[[mode(r)]],
	t0(T0),
	"S" <- callexp('TagToLST', [T0]),
	dispatch("0").
get_list_x0 :-
	[[mode(w)]],
	t1(T1),
	cachedreg('H', H),
	T1 <- callexp('Tag', ["LST",H]),
	t0(T0),
	if(callexp('TagIsHVA', [T0]),
	  bind(hva,T0,T1),
	  if((T0," & ", "TagBitSVA"),
	    bind(sva,T0,T1),
	    bind(cva,T0,T1))),
	dispatch("0").

:- ins_op_format(get_xvar_xvar, 108, [f_x,f_x,f_x,f_x]).
get_xvar_xvar :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dec(op(f_x,"BcP(f_x, 4)"), D),
	B <- A,
	D <- C,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(get_x_variable, 90, [f_x,f_x]).
get_x_variable :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	B <- A,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(get_y_first_variable, 92, [f_x,f_y]).
get_y_first_variable :-
	computeE,
	goto_ins(get_y_variable).

:- ins_op_format(get_y_variable, 93, [f_x,f_y], [label(_)]).
get_y_variable :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	B <- A,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(get_yfvar_yvar, 109, [f_x,f_y,f_x,f_y]).
get_yfvar_yvar :-
	computeE,
	goto_ins(get_yvar_yvar).

:- ins_op_format(get_yvar_yvar, 110, [f_x,f_y,f_x,f_y], [label(_)]).
get_yvar_yvar :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_y,"BcP(f_y, 2)"), B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	dec(op(f_y,"BcP(f_y, 4)"), D),
	B <- A,
	D <- C,
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y)+FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(branch, 68, [f_i]).
branch :-
	"P" <- "BCoff(P, BcP(f_i, 1))",
	dispatch("0").

% Call Expr function returning a tagged, goto fail on ERRORTAG
:- pred(cfun_semidet/2, [unfold]).
cfun_semidet(Target, Expr) :-
	if(("ERRORTAG==(", Target, " = (tagged_t)", Expr, ")"), goto('fail')).

:- pred(cblt_semidet/1, [unfold]).
cblt_semidet(Expr) :-
	if(("!", Expr), goto('fail')).

:- ins_op_format(function_1q, 222, [f_Q,f_x,f_x,f_C,f_l,f_i], [label(r)]).
:- ins_in_mode(function_1q, r).
function_1q :-
	"Numstack_End" <- "NULL",
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_x,"BcP(f_x, 3)"), B),
	cfun_semidet(A, callexp('(BcP(f_Cf, 4))', ["Arg",B,["&","BcP(f_l, 6)"]])),
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)+FTYPE_size(f_l)+FTYPE_size(f_i)").

:- ins_op_format(function_1, 223, [f_x,f_x,f_C,f_l,f_i], [label(r)]).
:- ins_in_mode(function_1, r).
function_1 :-
	"Numstack_End" <- "NULL",
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	cfun_semidet(A, callexp('(BcP(f_Cf, 3))', ["Arg",B,["&","BcP(f_l, 5)"]])),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)+FTYPE_size(f_l)+FTYPE_size(f_i)").

:- ins_op_format(function_2q, 224, [f_Q,f_x,f_x,f_x,f_C,f_l,f_i], [label(r)]).
:- ins_in_mode(function_2q, r).
function_2q :-
	"Numstack_End" <- "NULL",
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_x,"BcP(f_x, 3)"), B),
	dec(op(f_x,"BcP(f_x, 4)"), C),
	cfun_semidet(A, callexp('(BcP(f_Cf, 5))', ["Arg",B,C,["&","BcP(f_l, 7)"]])),
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)+FTYPE_size(f_l)+FTYPE_size(f_i)").

:- ins_op_format(function_2, 225, [f_x,f_x,f_x,f_C,f_l,f_i], [label(r)]).
:- ins_in_mode(function_2, r).
function_2 :-
	"Numstack_End" <- "NULL",
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	cfun_semidet(A, callexp('(BcP(f_Cf, 4))', ["Arg",B,C,["&","BcP(f_l, 6)"]])),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)+FTYPE_size(f_l)+FTYPE_size(f_i)").

:- ins_op_format(builtin_1q, 226, [f_Q,f_x,f_C], [label(r)]).
:- ins_in_mode(builtin_1q, r).
builtin_1q :-
	dec(op(f_x,"BcP(f_x, 2)"), A),
	cblt_semidet(callexp('(BcP(f_Cb, 3))', ["Arg",A])),
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x))+FTYPE_size(f_C)").

:- ins_op_format(builtin_1, 227, [f_x,f_C], [label(r)]).
:- ins_in_mode(builtin_1, r).
builtin_1 :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	cblt_semidet(callexp('(BcP(f_Cb, 2))', ["Arg",A])),
	dispatch("FTYPE_size(f_x)+FTYPE_size(f_C)").

:- ins_op_format(builtin_2q, 228, [f_Q,f_x,f_x,f_C], [label(r)]).
:- ins_in_mode(builtin_2q, r).
builtin_2q :-
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_x,"BcP(f_x, 3)"), B),
	cblt_semidet(callexp('(BcP(f_Cb, 4))', ["Arg",A,B])),
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)").

:- ins_op_format(builtin_2, 229, [f_x,f_x,f_C], [label(r)]).
:- ins_in_mode(builtin_2, r).
builtin_2 :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	cblt_semidet(callexp('(BcP(f_Cb, 3))', ["Arg",A,B])),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)").

:- ins_op_format(builtin_3q, 230, [f_Q,f_x,f_x,f_x,f_C], [label(r)]).
:- ins_in_mode(builtin_3q, r).
builtin_3q :-
	dec(op(f_x,"BcP(f_x, 2)"), A),
	dec(op(f_x,"BcP(f_x, 3)"), B),
	dec(op(f_x,"BcP(f_x, 4)"), C),
	cblt_semidet(callexp('(BcP(f_Cb, 5))', ["Arg",A,B,C])),
	dispatch("(FTYPE_size(f_Q)+FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)").

:- ins_op_format(builtin_3, 231, [f_x,f_x,f_x,f_C], [label(r)]).
:- ins_in_mode(builtin_3, r).
builtin_3 :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"), B),
	dec(op(f_x,"BcP(f_x, 3)"), C),
	cblt_semidet(callexp('(BcP(f_Cb, 4))', ["Arg",A,B,C])),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x)+FTYPE_size(f_x))+FTYPE_size(f_C)").

% backtracking into clause/2
:- ins_op_format(retry_instance, 232, [], [label(r)]).
:- ins_in_mode(retry_instance, r).
retry_instance :-
	% Take into account 'open' predicates.  (MCL)
	% If there is *definitely* no next instance, remove choicepoint
	if(("(TagToRoot(X(RootArg))->behavior_on_failure != DYNAMIC &&", fmt:nl,
	    % Wait and removes handle if needed
	    "!next_instance_conc(Arg, &ins))", fmt:nl,
	    "||", fmt:nl,
	    "(TagToRoot(X(RootArg))->behavior_on_failure == DYNAMIC &&", fmt:nl,
	    "!next_instance(Arg, &ins))", fmt:nl),
	  ("w->next_alt = NULL;", fmt:nl,
	   call('SetB', ["w->next_node"]),
	   "w->node" <- "B",
	   "SetShadowregs(B);", fmt:nl)),
	if("!ins",
	  % A conc. predicate has been closed, or a non-blocking call was made (MCL)
          (trace(retry_instance_debug_1),
	   "TopConcChpt = (node_t *)TermToPointerOrNull(X(PrevDynChpt));", fmt:nl,
	   trace(retry_instance_debug_2),
	   % But fail anyway
	   goto('fail'))),
        trace(retry_instance_debug_3),
	"P" <- "(bcp_t)ins->emulcode",
	goto_ins_dispatch.

:- ins_op_format(get_constraint, 247, [f_x], [label(w)]).
:- ins_in_mode(get_constraint, w).
get_constraint :-
	dec(op(f_x,"BcP(f_x, 1)"), A),
	t1(T1),
	T1 <- A,
	t2(T2),
	load(cva,T2),
	t0(T0),
	sw_on_var(T1, T0,
	  (bind(hva,T1,T2), A <- T2),
	  bind(cva,T2,T1),
	  (bind(sva,T1,T2), A <- T2),
	  bind(cva,T2,T1)),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(unify_void, 114, [f_i]).
unify_void :-
	[[mode(r)]],
	u1(void("BcP(f_i, 1)")),
	dispatch("FTYPE_size(f_i)").
unify_void :-
	[[mode(w)]],
	"i" <- ["(FTYPE_ctype(f_i_signed))", "BcP(f_i, 1)"],
        shift(f_i),
	for("; i>4; --i",
	  (cachedreg('H', H),
	   call('ConstrHVA', [H]))),
	goto_ins(unify_void_4).

:- ins_op_format(unify_void_1, 115, [], [label(w)]).
unify_void_1 :-
	[[mode(r)]],
	u1(void("1")),
	dispatch("0").
unify_void_1 :-
	[[mode(w)]],
	cachedreg('H', H),
	call('ConstrHVA', [H]),
	dispatch("0").

:- ins_op_format(unify_void_2, 116, [], [label(w)]).
unify_void_2 :-
	[[mode(r)]],
	u1(void("2")),
	dispatch("0").
unify_void_2 :-
	[[mode(w)]],
	cachedreg('H', H),
	call('ConstrHVA', [H]),
	goto_ins(unify_void_1).

:- ins_op_format(unify_void_3, 117, [], [label(w)]).
unify_void_3 :-
	[[mode(r)]],
	u1(void("3")),
	dispatch("0").
unify_void_3 :-
	[[mode(w)]],
	cachedreg('H', H),
	call('ConstrHVA', [H]),
	goto_ins(unify_void_2).

:- ins_op_format(unify_void_4, 118, [], [label(w)]).
unify_void_4 :-
	[[mode(r)]],
	u1(void("4")),
	dispatch("0").
unify_void_4 :-
	[[mode(w)]],
	cachedreg('H', H),
	call('ConstrHVA', [H]),
	goto_ins(unify_void_3).

:- ins_op_format(unify_x_variable, 119, [f_x]).
unify_x_variable :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	u1(var(A)),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(unify_x_value, 120, [f_x]).
unify_x_value :-
	[[mode(r)]],
	goto_ins(unify_x_local_value).
unify_x_value :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	u1(xval(A)),
	dispatch("FTYPE_size(f_x)").

:- ins_op_format(unify_x_local_value, 121, [f_x], [label(r)]).
unify_x_local_value :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	u1_dispatch(xlval(A), "FTYPE_size(f_x)").

:- ins_op_format(unify_y_first_variable, 122, [f_y]).
unify_y_first_variable :-
	computeE,
	goto_ins(unify_y_variable).

:- ins_op_format(unify_y_variable, 123, [f_y], [label(_)]).
unify_y_variable :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(var(A)),
	dispatch("FTYPE_size(f_y)").

:- ins_op_format(unify_y_first_value, 124, [f_y]).
unify_y_first_value :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(yfval(A)),
	dispatch("FTYPE_size(f_y)").

:- ins_op_format(unify_y_value, 125, [f_y]).
unify_y_value :-
	[[mode(r)]],
	goto_ins(unify_y_local_value).
unify_y_value :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(yval(A)),
	dispatch("FTYPE_size(f_y)").

:- ins_op_format(unify_y_local_value, 126, [f_y], [label(r)]).
unify_y_local_value :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1_dispatch(ylval(A), "FTYPE_size(f_y)").

:- ins_op_format(unify_constantq, 127, [f_Q,f_t]).
unify_constantq :-
	[[mode(r)]],
	shift(f_Q), goto_ins(unify_constant).
unify_constantq :-
	[[mode(w)]],
	dec(op(f_t,"BcP(f_t, 2)"), A),
	heap_push(A),
	dispatch("FTYPE_size(f_Q)+FTYPE_size(f_t)").

:- ins_op_format(unify_constant, 128, [f_t], [label(r)]).
unify_constant :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	dec(op(f_t,"BcP(f_t, 1)"), A),
	unify_heap_atom(A,T1),
	dispatch("FTYPE_size(f_t)").
unify_constant :-
	[[mode(w)]],
	dec(op(f_t,"BcP(f_t, 1)"), A),
	heap_push(A),
	dispatch("FTYPE_size(f_t)").

:- ins_op_format(unify_largeq, 258, [f_Q,f_b]).
unify_largeq :- shift(f_Q), goto_ins(unify_large).

:- ins_op_format(unify_large, 259, [f_b], [label(_)]).
unify_large :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	dec(op(f_b,"BcP(f_t, 1)"), A),
	unify_heap_large("Arg",A,T1),
	dispatch((callexp('LargeSize',["BcP(f_t, 1)"]),"")).
unify_large :-
	[[mode(w)]],
	% TODO: try to switch to r mode properly (this code is tricky)
	% (this is 'heap_push and switch to read')
	cachedreg('H', H),
	"w->global_top" <- callexp('HeapOffset', [H,"1"]),
	dec(op(f_b,"BcP(f_t, 1)"), A),
	"*H" <- callexp('BC_MakeLarge', ["Arg",A]),
	[[update(mode(r))]],
	dispatch((callexp('LargeSize',["BcP(f_t, 1)"]),"")).

:- ins_op_format(unify_structureq, 129, [f_Q,f_f]).
unify_structureq :-
	[[mode(r)]],
	shift(f_Q), goto_ins(unify_structure).
unify_structureq :-
	[[mode(w)]],
	cachedreg('H', H),
	heap_push(callexp('Tag', ["STR",callexp('HeapOffset', [H,"1"])])),
	dec(op(f_f,"BcP(f_f, 2)"),A),
	heap_push(A),
	dispatch("FTYPE_size(f_Q)+FTYPE_size(f_f)").

:- ins_op_format(unify_structure, 130, [f_f], [label(r)]).
unify_structure :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	dec(op(f_f,"BcP(f_f, 1)"),A),
	unify_heap_structure(A,T1,dispatch("FTYPE_size(f_f)")).
unify_structure :-
	[[mode(w)]],
	cachedreg('H', H),
	heap_push(callexp('Tag', ["STR",callexp('HeapOffset', [H,"1"])])),
	dec(op(f_f,"BcP(f_f, 1)"),A),
	heap_push(A),
	dispatch("FTYPE_size(f_f)").

:- ins_op_format(unify_nil, 131, []).
unify_nil :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	get_atom([], Nil),
	unify_heap_atom(Nil, T1),
	dispatch("0").
unify_nil :-
	[[mode(w)]],
	get_atom([], Nil),
	heap_push(Nil),
	dispatch("0").

:- ins_op_format(unify_list, 132, []).
unify_list :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	unify_heap_list(T1,dispatch("0")).
unify_list :-
	[[mode(w)]],
	cachedreg('H', H),
	heap_push(callexp('Tag', ["LST",callexp('HeapOffset', [H,"1"])])),
	dispatch("0").

:- ins_op_format(unify_constant_neck_proceedq, 133, [f_Q,f_t]).
unify_constant_neck_proceedq :-
	[[mode(r)]],
	shift(f_Q), goto_ins(unify_constant_neck_proceed).
unify_constant_neck_proceedq :-
	[[mode(w)]],
	dec(op(f_t,"BcP(f_t, 2)"), A),
	heap_push(A),
	goto_ins(neck_proceed).

:- ins_op_format(unify_constant_neck_proceed, 134, [f_t], [label(r)]).
unify_constant_neck_proceed :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	dec(op(f_t,"BcP(f_t, 1)"), A),
	unify_heap_atom(A,T1),
	setmode(w),
	goto_ins(neck_proceed).
unify_constant_neck_proceed :-
	[[mode(w)]],
	dec(op(f_t,"BcP(f_t, 1)"), A),
	heap_push(A),
	goto_ins(neck_proceed).

:- ins_op_format(unify_nil_neck_proceed, 135, []).
unify_nil_neck_proceed :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	get_atom([], Nil),
	unify_heap_atom(Nil, T1),
	setmode(w),
	goto_ins(neck_proceed).
unify_nil_neck_proceed :-
	[[mode(w)]],
	get_atom([], Nil),
	heap_push(Nil),
	goto_ins(neck_proceed).

:- ins_op_format(u2_void_xvar, 136, [f_i,f_x]).
u2_void_xvar :-
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(void("BcP(f_i, 1)")),
	u1(var(B)),
	dispatch("(FTYPE_size(f_i)+FTYPE_size(f_x))").

:- ins_op_format(u2_void_yfvar, 139, [f_i,f_y]).
u2_void_yfvar :-
	computeE,
	goto_ins(u2_void_yvar).

:- ins_op_format(u2_void_yvar, 140, [f_i,f_y], [label(_)]).
u2_void_yvar :-
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(void("BcP(f_i, 1)")),
	u1(var(B)),
	dispatch("(FTYPE_size(f_i)+FTYPE_size(f_y))").

:- ins_op_format(u2_void_xval, 137, [f_i,f_x]).
u2_void_xval :-
	[[mode(r)]],
	goto_ins(u2_void_xlval).
u2_void_xval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(void("BcP(f_i, 1)")),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_i)+FTYPE_size(f_x))").

:- ins_op_format(u2_void_xlval, 138, [f_i,f_x], [label(r)]).
u2_void_xlval :-
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(void("BcP(f_i, 1)")),
	u1_dispatch(xlval(B), "(FTYPE_size(f_i)+FTYPE_size(f_x))").

:- ins_op_format(u2_void_yfval, 141, [f_i,f_y]).
u2_void_yfval :-
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(void("BcP(f_i, 1)")),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_i)+FTYPE_size(f_y))").

:- ins_op_format(u2_void_yval, 142, [f_i,f_y]).
u2_void_yval :-
	[[mode(r)]],
	goto_ins(u2_void_ylval).
u2_void_yval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(void("BcP(f_i, 1)")),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_i)+FTYPE_size(f_y))").

:- ins_op_format(u2_void_ylval, 143, [f_i,f_y], [label(r)]).
u2_void_ylval :-
	u1(void("BcP(f_i, 1)")),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1_dispatch(ylval(B), "(FTYPE_size(f_i)+FTYPE_size(f_y))").

:- ins_op_format(u2_xvar_void, 144, [f_x,f_i]).
u2_xvar_void :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	u1(var(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_i))").

:- ins_op_format(u2_xvar_xvar, 145, [f_x,f_x]).
u2_xvar_xvar :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(var(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xvar_yfvar, 148, [f_x,f_y]).
u2_xvar_yfvar :-
	computeE,
	goto_ins(u2_xvar_yvar).

:- ins_op_format(u2_xvar_yvar, 149, [f_x,f_y], [label(_)]).
u2_xvar_yvar :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xvar_xval, 146, [f_x,f_x]).
u2_xvar_xval :-
	[[mode(r)]],
	goto_ins(u2_xvar_xlval).
u2_xvar_xval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(var(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xvar_xlval, 147, [f_x,f_x], [label(r)]).
u2_xvar_xlval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(var(A)),
	u1_dispatch(xlval(B), "(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xvar_yfval, 150, [f_x,f_y]).
u2_xvar_yfval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xvar_yval, 151, [f_x,f_y]).
u2_xvar_yval :-
	[[mode(r)]],
	goto_ins(u2_xvar_ylval).
u2_xvar_yval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xvar_ylval, 152, [f_x,f_y], [label(r)]).
u2_xvar_ylval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1_dispatch(ylval(B), "(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_yfvar_void, 153, [f_y,f_i]).
u2_yfvar_void :-
	computeE,
	goto_ins(u2_yvar_void).

:- ins_op_format(u2_yvar_void, 154, [f_y,f_i], [label(_)]).
u2_yvar_void :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(var(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_i))").

:- ins_op_format(u2_yfvar_xvar, 155, [f_y,f_x]).
u2_yfvar_xvar :-
	computeE,
	goto_ins(u2_yvar_xvar).

:- ins_op_format(u2_yvar_xvar, 156, [f_y,f_x], [label(_)]).
u2_yvar_xvar :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(var(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yfvar_yvar, 157, [f_y,f_y]).
u2_yfvar_yvar :-
	computeE,
	goto_ins(u2_yvar_yvar).

:- ins_op_format(u2_yvar_yvar, 158, [f_y,f_y], [label(_)]).
u2_yvar_yvar :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yfvar_xval, 159, [f_y,f_x]).
u2_yfvar_xval :-
	[[mode(r)]],
	goto_ins(u2_yfvar_xlval).
u2_yfvar_xval :-
	[[mode(w)]],
	computeE,
	goto_ins(u2_yvar_xval).

:- ins_op_format(u2_yfvar_xlval, 161, [f_y,f_x], [label(r)]).
u2_yfvar_xlval :-
	computeE,
	goto_ins(u2_yvar_xlval).

:- ins_op_format(u2_yvar_xval, 160, [f_y,f_x], [label(w)]).
u2_yvar_xval :-
	[[mode(r)]],
	goto_ins(u2_yvar_xlval).
u2_yvar_xval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(var(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yvar_xlval, 162, [f_y,f_x], [label(_)]).
u2_yvar_xlval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(var(A)),
	u1_dispatch(xlval(B), "(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yfvar_yval, 163, [f_y,f_y]).
u2_yfvar_yval :-
	[[mode(r)]],
	goto_ins(u2_yfvar_ylval).
u2_yfvar_yval :-
	[[mode(w)]],
	computeE,
	goto_ins(u2_yvar_yval).

:- ins_op_format(u2_yfvar_ylval, 165, [f_y,f_y], [label(r)]).
u2_yfvar_ylval :-
	computeE,
	goto_ins(u2_yvar_ylval).

:- ins_op_format(u2_yvar_yval, 164, [f_y,f_y], [label(w)]).
u2_yvar_yval :-
	[[mode(r)]],
	goto_ins(u2_yvar_ylval).
u2_yvar_yval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yvar_ylval, 166, [f_y,f_y], [label(_)]).
u2_yvar_ylval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(var(A)),
	u1_dispatch(ylval(B), "(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yfval_void, 185, [f_y,f_i]).
u2_yfval_void :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(yfval(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_i))").

:- ins_op_format(u2_yfval_xvar, 188, [f_y,f_x]).
u2_yfval_xvar :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(yfval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yfval_yfval, 199, [f_y,f_y]).
u2_yfval_yfval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yfval(A)),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yfval_xval, 193, [f_y,f_x]).
u2_yfval_xval :-
	[[mode(r)]],
	goto_ins(u2_yfval_xlval).
u2_yfval_xval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(yfval(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yfval_xlval, 196, [f_y,f_x], [label(r)]).
u2_yfval_xlval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(yfval(A)),
	u1_dispatch(xlval(B), "(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yfval_yval, 202, [f_y,f_y]).
u2_yfval_yval :-
	[[mode(r)]],
	goto_ins(u2_yfval_ylval).
u2_yfval_yval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yfval(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yfval_ylval, 205, [f_y,f_y], [label(r)]).
u2_yfval_ylval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yfval(A)),
	u1_dispatch(ylval(B), "(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_xval_void, 167, [f_x,f_i]).
u2_xval_void :-
	[[mode(r)]],
	goto_ins(u2_xlval_void).
u2_xval_void :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	u1(xval(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_i))").

:- ins_op_format(u2_xlval_void, 168, [f_x,f_i], [label(r)]).
u2_xlval_void :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	u1(void("BcP(f_i, 2)")),
	dec(op(f_x,"BcP(f_x, 1)"), A),
	eunify(A,T1,"(FTYPE_size(f_x)+FTYPE_size(f_i))").
u2_xlval_void :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	u1(xlval(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_i))").

:- ins_op_format(u2_xval_xvar, 169, [f_x,f_x]).
u2_xval_xvar :-
	[[mode(r)]],
	goto_ins(u2_xlval_xvar).
u2_xval_xvar :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(xval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xlval_xvar, 170, [f_x,f_x], [label(r)]).
u2_xlval_xvar :-
	[[mode(r)]],
	dec(op(f_x,"BcP(f_x, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	t0(T0),
	T0 <- A,
	t1(T1),
	ref_heap_next(T1),
	u1(var(B)),
	eunify(T0,T1,"(FTYPE_size(f_x)+FTYPE_size(f_x))").
u2_xlval_xvar :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(xlval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xval_yfvar, 171, [f_x,f_y]).
u2_xval_yfvar :-
	[[mode(r)]],
	goto_ins(u2_xlval_yfvar).
u2_xval_yfvar :-
	[[mode(w)]],
	computeE,
	goto_ins(u2_xval_yvar).

:- ins_op_format(u2_xlval_yfvar, 172, [f_x,f_y], [label(r)]).
u2_xlval_yfvar :-
	computeE,
	goto_ins(u2_xlval_yvar).

:- ins_op_format(u2_xval_yvar, 173, [f_x,f_y], [label(w)]).
u2_xval_yvar :-
	[[mode(r)]],
	goto_ins(u2_xlval_yvar).
u2_xval_yvar :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xlval_yvar, 174, [f_x,f_y], [label(_)]).
u2_xlval_yvar :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xlval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xval_xval, 175, [f_x,f_x]).
u2_xval_xval :-
	[[mode(r)]],
	goto_ins(u2_xval_xlval).
u2_xval_xval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(xval(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xval_xlval, 177, [f_x,f_x], [label(r)]).
u2_xval_xlval :-
	[[mode(r)]],
	goto_ins(u2_xlval_xval).
u2_xval_xlval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(xval(A)),
	u1(xlval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xlval_xval, 176, [f_x,f_x], [label(r)]).
u2_xlval_xval :-
	[[mode(r)]],
	goto_ins(u2_xlval_xlval).
u2_xlval_xval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(xlval(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xlval_xlval, 178, [f_x,f_x], [label(r)]).
u2_xlval_xlval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(xlval(A)),
	u1_dispatch(xlval(B), "(FTYPE_size(f_x)+FTYPE_size(f_x))").

:- ins_op_format(u2_xval_yfval, 179, [f_x,f_y]).
u2_xval_yfval :-
	[[mode(r)]],
	goto_ins(u2_xlval_yfval).
u2_xval_yfval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xval(A)),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xlval_yfval, 180, [f_x,f_y], [label(r)]).
u2_xlval_yfval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xlval(A)),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xval_yval, 181, [f_x,f_y]).
u2_xval_yval :-
	[[mode(r)]],
	goto_ins(u2_xval_ylval).
u2_xval_yval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xval(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xval_ylval, 183, [f_x,f_y], [label(r)]).
u2_xval_ylval :-
	[[mode(r)]],
	goto_ins(u2_xlval_yval).
u2_xval_ylval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xval(A)),
	u1(ylval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xlval_yval, 182, [f_x,f_y], [label(r)]).
u2_xlval_yval :-
	[[mode(r)]],
	goto_ins(u2_xlval_ylval).
u2_xlval_yval :-
	[[mode(w)]],
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xlval(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_xlval_ylval, 184, [f_x,f_y], [label(r)]).
u2_xlval_ylval :-
	dec(op(f_x,"BcP(f_x, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(xlval(A)),
	u1_dispatch(ylval(B), "(FTYPE_size(f_x)+FTYPE_size(f_y))").

:- ins_op_format(u2_yval_void, 186, [f_y,f_i]).
u2_yval_void :-
	[[mode(r)]],
	goto_ins(u2_ylval_void).
u2_yval_void :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(yval(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_i))").

:- ins_op_format(u2_ylval_void, 187, [f_y,f_i], [label(r)]).
u2_ylval_void :-
	[[mode(r)]],
	t1(T1),
	ref_heap_next(T1),
	u1(void("BcP(f_i, 2)")),
	dec(op(f_y,"BcP(f_y, 1)"), A),
	eunify(A,T1,"(FTYPE_size(f_y)+FTYPE_size(f_i))").
u2_ylval_void :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	u1(ylval(A)),
	u1(void("BcP(f_i, 2)")),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_i))").

:- ins_op_format(u2_yval_xvar, 189, [f_y,f_x]).
u2_yval_xvar :-
	[[mode(r)]],
	goto_ins(u2_ylval_xvar).
u2_yval_xvar :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(yval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_ylval_xvar, 190, [f_y,f_x], [label(r)]).
u2_ylval_xvar :-
	[[mode(r)]],
	dec(op(f_y,"BcP(f_y, 1)"), A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	t1(T1),
	ref_heap_next(T1),
	u1(var(B)),
	eunify(A,T1,"(FTYPE_size(f_y)+FTYPE_size(f_x))").
u2_ylval_xvar :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(ylval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yval_yvar, 191, [f_y,f_y]).
u2_yval_yvar :-
	[[mode(r)]],
	goto_ins(u2_ylval_yvar).
u2_yval_yvar :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_ylval_yvar, 192, [f_y,f_y], [label(r)]).
u2_ylval_yvar :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(ylval(A)),
	u1(var(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yval_yfval, 200, [f_y,f_y]).
u2_yval_yfval :-
	[[mode(r)]],
	goto_ins(u2_ylval_yfval).
u2_yval_yfval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yval(A)),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_ylval_yfval, 201, [f_y,f_y], [label(r)]).
u2_ylval_yfval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(ylval(A)),
	u1(yfval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yval_xval, 194, [f_y,f_x]).
u2_yval_xval :-
	[[mode(r)]],
	goto_ins(u2_yval_xlval).
u2_yval_xval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(yval(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yval_xlval, 197, [f_y,f_x], [label(r)]).
u2_yval_xlval :-
	[[mode(r)]],
	goto_ins(u2_ylval_xval).
u2_yval_xlval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(yval(A)),
	u1(xlval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_ylval_xval, 195, [f_y,f_x], [label(r)]).
u2_ylval_xval :-
	[[mode(r)]],
	goto_ins(u2_ylval_xlval).
u2_ylval_xval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(ylval(A)),
	u1(xval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_ylval_xlval, 198, [f_y,f_x], [label(r)]).
u2_ylval_xlval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_x,"BcP(f_x, 2)"),B),
	u1(ylval(A)),
	u1_dispatch(xlval(B), "(FTYPE_size(f_y)+FTYPE_size(f_x))").

:- ins_op_format(u2_yval_yval, 203, [f_y,f_y]).
u2_yval_yval :-
	[[mode(r)]],
	goto_ins(u2_yval_ylval).
u2_yval_yval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yval(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_yval_ylval, 206, [f_y,f_y], [label(r)]).
u2_yval_ylval :-
	[[mode(r)]],
	goto_ins(u2_ylval_yval).
u2_yval_ylval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(yval(A)),
	u1(ylval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_ylval_yval, 204, [f_y,f_y], [label(r)]).
u2_ylval_yval :-
	[[mode(r)]],
	goto_ins(u2_ylval_ylval).
u2_ylval_yval :-
	[[mode(w)]],
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(ylval(A)),
	u1(yval(B)),
	dispatch("(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(u2_ylval_ylval, 207, [f_y,f_y], [label(r)]).
u2_ylval_ylval :-
	dec(op(f_y,"BcP(f_y, 1)"),A),
	dec(op(f_y,"BcP(f_y, 2)"),B),
	u1(ylval(A)),
	u1_dispatch(ylval(B), "(FTYPE_size(f_y)+FTYPE_size(f_y))").

:- ins_op_format(bump_counterq, 248, [f_Q,f_l]).
bump_counterq :- shift(f_Q), goto_ins(bump_counter).

:- ins_op_format(bump_counter, 249, [f_l], [label(_)]).
bump_counter :-
	gauge_incr_counter(t1),
	dispatch("FTYPE_size(f_l)").

:- ins_op_format(counted_neckq, 250, [f_Q,f_l,f_l]).
counted_neckq :- shift(f_Q), goto_ins(counted_neck).

:- ins_op_format(counted_neck, 251, [f_l,f_l], [label(_)]).
counted_neck :-
	cpp_if_defined('GAUGE'),
	if("w->next_alt", (
	  call('SetB', ["w->node"]),
	  if("B->next_alt", (
	    % retry counter
	    gauge_incr_counter(t1)
	  ),(
	    % try counter
	    gauge_incr_counter(t3)
	  ))
	)),
	cpp_endif,
	inc("P", "(FTYPE_size(f_l)+FTYPE_size(f_l))"),
	goto_ins(neck).

:- ins_op_format(fail, 67, []).
fail :-
	goto('fail').

:- ins_op_format(heapmargin_callq, 245, [f_Q,f_l,f_i]).
heapmargin_callq :- shift(f_Q), goto_ins(heapmargin_call).

:- ins_op_format(heapmargin_call, 246, [f_l,f_i], [label(_)]).
heapmargin_call :-
	cachedreg('H',H),
	if((callexp('HeapDifference', [H, "Heap_End"]), " < ", ["(intmach_t)","BcP(f_l, 1)"]),
	  ([[mode(M)]],
	   setmode(r),
	   call('explicit_heap_overflow', ["Arg",["(intmach_t)","BcP(f_l, 1)"],["(FTYPE_ctype(f_i_signed))","BcP(f_i, 3)"]]),
	   setmode(M),
	   t0(T0),
	   T0 <- "X(0)" % if followed by get_*_x0
	   )),
	dispatch("FTYPE_size(f_l)+FTYPE_size(f_i)").

:- ins_op_format(neck, 65, [], [label(_)]).
neck :-
	code_neck,
	dispatch("0").

:- ins_op_format(dynamic_neck_proceed, 236, [], [label(w)]).
:- ins_in_mode(dynamic_neck_proceed, w).
dynamic_neck_proceed :-
	unify_atom_internal(callexp('PointerToTerm',["ins"]),"X(3)"),
	if("!w->next_alt", goto_ins(proceed)),
	call('SetB', ["w->node"]),
	if("!B->next_alt && (def_clock = use_clock+1)==0xffff",(
	  setmode(r),
	  call('clock_overflow', ["Arg"]),
	  setmode(w)
	)),
	goto_ins(neck_proceed).

:- ins_op_format(neck_proceed, 66, [], [label(w)]).
:- ins_in_mode(neck_proceed, w).
neck_proceed :-
	code_neck_proceed.

:- ins_op_format(proceed, 64, [], [label(_)]).
proceed :-
	"w->local_top = 0;", fmt:nl,
	call('SetE', ["w->frame"]),
	"P = w->next_insn;", fmt:nl,
	profile_hook(proceed),
	dispatch("0").

% TODO: this a new instruction really needed here? consider special builtin functions
:- ins_op_format(restart_point, 262, [], [optional('PARBACK')]).
restart_point :-
	setmode_setH(r, "TagToPointer(w->node->term[0])"),
	setmode(w),
	"P" <- "(bcp_t)*TagToPointer(w->node->term[0])",
	"w->next_insn" <- "w->node->next_insn",
	call('pop_choicept', ["Arg"]),
	goto('enter_predicate').

% :- ins_op_format(ci_call, 241, [f_i,f_i]).
% :- ins_op_format(ci_inarg, 242, [f_i,f_i]).
% :- ins_op_format(ci_outarg, 243, [f_i,f_i]).
% :- ins_op_format(ci_retval, 244, [f_i,f_i]).

% ---------------------------------------------------------------------------
:- doc(section, "WAM execution tracing").

:- pred(pred_trace/1, [unfold]).
pred_trace(Kind) :-
	call('PredTrace', [Kind, "Func"]).

:- pred(trace/1, [unfold]).
trace(X) :-
	"ON_DEBUG({", fmt:nl,
	trace_(X),
        "});", fmt:nl.

:- pred(trace_/1, [unfold]).
trace_(wam_loop_begin) :-
	if("debug_threads",
	   "printf(\"Worker state address is %p\\n\", desc);").
trace_(wam_loop_exit) :-
	% "printf(\"Goal %p returning!\\n\", desc);", fmt:nl.
	true.
trace_(create_choicepoint) :-
        if("debug_choicepoints",
          ("fprintf(stderr, \"WAM created choicepoint (r), node = %x\\n\", (int)w->node);", fmt:nl)).
trace_(failing_choicepoint) :-
	if("debug_choicepoints",
	  ("fprintf(stderr, \"Failing: node = %x, next_node = %x, conc. node = %x\\n\", (int)w->node, (int)w->next_node, (int)TopConcChpt);", fmt:nl)),
	if("(w->misc->top_conc_chpt < w->node) && \
	    (w->misc->top_conc_chpt < w->next_node)", 
	   ("fprintf(stderr, \"********** what happened here?\\n\");", fmt:nl)).
trace_(deep_backtracking) :-
	if("debug_choicepoints",
	  ("fprintf(stderr, \"deep backtracking, node = %x\\n\", (int)w->node);", fmt:nl)).
trace_(restore_xregs_choicepoint(I)) :-
        if("debug_choicepoints",
	   ("fprintf(stderr, \"Reloading %d words from node %x\\n\", ", I, ", (int)w->node);", fmt:nl)).
trace_(worker_expansion_blt) :-
	"printf(\"wam() detected worker expanded by C predicate\\n\");", fmt:nl.
trace_(worker_expansion_cterm) :-
	"fprintf(stderr, \"Reallocation of wrb detected in wam()\\n\");", fmt:nl.
trace_(neck(I)) :-
	if("debug_choicepoints",
	  ("fprintf(stderr, \"Storing %d registers (r) in node %x\\n\", ", I, ", (int)w->next_node);", fmt:nl)).
trace_(retry_instance_debug_1) :-
	% Extended check
        if("debug_concchoicepoints",
          if(("(TagToRoot(X(RootArg))->behavior_on_failure != CONC_CLOSED) && ",
              "(IS_BLOCKING(X(InvocationAttr)))"),
              ("fprintf(stderr,", fmt:nl,
              "\"**wam(): failing on a concurrent closed pred, chpt=%x, failing chpt=%x .\\n\",", fmt:nl,
              "(int)w->node,(int)TopConcChpt);", fmt:nl))),
        if("debug_conc",
          if(("TagToRoot(X(RootArg))->x2_pending_on_instance || ",
	      "TagToRoot(X(RootArg))->x5_pending_on_instance"),
            ("fprintf(stderr, ", fmt:nl,
            "        \"**wam(): failing with invokations pending from root, type = %d.\\n\",", fmt:nl,
            "        (TagToRoot(X(RootArg))->behavior_on_failure));"))).
trace_(retry_instance_debug_2) :-
        if("debug_concchoicepoints",
        ("  fprintf(stderr,\"New topmost concurrent chpt = %x\\n\", (int)TopConcChpt);", fmt:nl)).
trace_(retry_instance_debug_3) :-
        if("debug_conc && TagToRoot(X(RootArg))->behavior_on_failure != DYNAMIC",
        ("  fprintf(stderr, ", fmt:nl,
        "         \"*** %d(%d)  backtracking on a concurrent predicate.\\n\",", fmt:nl,
        "          (int)Thread_Id, (int)GET_INC_COUNTER);", fmt:nl)),
        if(("debug_concchoicepoints && ",
            "TagToRoot(X(RootArg))->behavior_on_failure != DYNAMIC"),
	  ("fprintf(stderr, ", fmt:nl,
	   "         \"backtracking to chpt. = %x\\n\", (int)w->node);", fmt:nl)).


% ---------------------------------------------------------------------------
:- doc(section, "WAM profiling").

:- pred(profile_hook/1, [unfold]).
profile_hook(cut) :-
        call0('PROFILE__HOOK_CUT').
profile_hook(proceed) :-
	call0('PROFILE__HOOK_PROCEED').
profile_hook(neck_proceed) :-
        call0('PROFILE__HOOK_NECK_PROCEED').
profile_hook(fail) :-
	call0('PROFILE__HOOK_FAIL').
profile_hook(redo) :-
	call0('PROFILE__HOOK_REDO').

% ---------------------------------------------------------------------------

:- doc(section, "Gauge (profiling counters)").

:- pred(gauge_incr_counter/1, [unfold]).
gauge_incr_counter(t1) :- % Counter in bytecode "BcP(f_l, 1)"
	cpp_if_defined('GAUGE'),
	call('INCR_COUNTER', ["BcP(f_l, 1)"]),
	cpp_endif.
gauge_incr_counter(t3) :- % Counter in bytecode "BcP(f_l, 3)"
	cpp_if_defined('GAUGE'),
	call('INCR_COUNTER', ["BcP(f_l, 3)"]),
	cpp_endif.
gauge_incr_counter(alts) :- % Counter in Alts
	[[ Alts = "Alts" ]],
	( [[ mode(r) ]], [[ EntryCounter = (Alts, "->entry_counter+1") ]]
	; [[ mode(w) ]], [[ EntryCounter = (Alts, "->entry_counter") ]]
	),
	cpp_if_defined('GAUGE'),
	call('INCR_COUNTER', [EntryCounter]),
	cpp_endif.

% ---------------------------------------------------------------------------
:- doc(section, "Other files").

% Instruction definitions
:- pred(all_ins_op/0, [unfold]).
all_ins_op :-
	autogen_warning_comment,
	%
	'$all_ins_op'.

% TODO: refactor
% Engine info (for inclusion in Makefile)
:- pred(eng_info_mk/0, [unfold]).
eng_info_mk :-
	[[ findall(F, use_native(F, c), Cs) ]],
	[[ findall(F, use_native(F, h), Hs) ]],
	[[ findall(F, use_native(F, h_noalias), HsNoAlias) ]],
	makefile_def('ENG_STUBMAIN', ['main.c']), % TODO: hardwired (see Makefile for details)
	makefile_def('ENG_CFILES', Cs),
	makefile_def('ENG_HFILES', Hs),
	makefile_def('ENG_HFILES_NOALIAS', HsNoAlias).

:- pred(makefile_def/2, [unfold]).
makefile_def(X, Fs) :-
	fmt:atom(X), " = ", 
	'$foreach_sep'(" ", Fs, fmt_atom),
	[fmt:nl].

% Engine info (for inclusion in sh scripts)
:- pred(eng_info_sh/0, [unfold]).
eng_info_sh :-
	[[ findall(F, use_native(F, c), Cs) ]],
	[[ findall(F, use_native(F, h), Hs) ]],
	[[ findall(F, use_native(F, h_noalias), HsNoAlias) ]],
	sh_def('ENG_STUBMAIN', ['main.c']), % TODO: hardwired (see Makefile for details)
	sh_def('ENG_CFILES', Cs),
	sh_def('ENG_HFILES', Hs),
	sh_def('ENG_HFILES_NOALIAS', HsNoAlias).

:- pred(sh_def/2, [unfold]).
sh_def(X, Fs) :-
	fmt:atom(X), "=\"", 
	'$foreach_sep'(" ", Fs, fmt_atom),
	"\"",
	[fmt:nl].

:- pred(fmt_atom/1, [unfold]).
fmt_atom(X) :- fmt:atom(X).	

% Meta-information
:- pred(absmachdef/0, [unfold]).
absmachdef :-
	autogen_warning_comment,
	%
	[[ max_op(MaxOp) ]],
	[[ NumOp is MaxOp + 1 ]],
	cpp_define('INS_OPCOUNT', NumOp),
	%
	"absmachdef_t abscurr = {", fmt:nl,
	'$absmachdef',
	"};", fmt:nl,
	%
	insnames.

:- pred(ftype_id/1, [unfold]).
ftype_id(FType) :-
	[[ ftype_def(FType, Id, _) ]],
	fmt:number(Id).

:- pred(insnames/0, [unfold]).
insnames :-
	[[ max_op(MaxOp) ]],
	[[ NumOp is MaxOp + 1 ]],
	"char *ins_name[", NumOp, "] = {", fmt:nl,
	[[ range(0, MaxOp, Ops) ]],
	'$foreach_sep'(",\n", Ops, op_insname),
	"};", fmt:nl.

:- pred(op_insname/1, [unfold]).
op_insname(Op) :-
	[[ op_ins(Op, Ins) ]],
	[[ prop(Ins, ins_op(Op)) ]],
	"\"", fmt:atom(Ins), "\"".
op_insname(Op) :-
	[[ not(op_ins(Op, _)) ]],
	"\"(none)\"".

:- pred(autogen_warning_comment/0, [unfold]).
autogen_warning_comment :-
	"/***************************************************************************/", fmt:nl,
	"/*                             WARNING!!!                                  */", fmt:nl,
	"/*                      D O   N O T   M O D I F Y                          */", fmt:nl,
	"/*                This file is autogenerated by emugen                     */", fmt:nl,
	"/***************************************************************************/", fmt:nl,
	fmt:nl.

% ---------------------------------------------------------------------------
:- doc(section, "The WAM loop function").

% KERNEL OF EMULATOR

% If the wam() local variables are changed, those on task_areas.h should
% be changed as well to reflect the current state! They should as well be
% saved and recovered in SAVE_WAM_STATE and RECOVER_WAM_STATE

/* --------------------------------------------------------------------------- */

:- pred(op_macros/0, [unfold]).
op_macros :-
        cpp_define('SetB(X)', "(pt1 = (tagged_t *)(X))"),
        cpp_define('LoadH',"(H = w->global_top)"),
        cpp_define('StoreH',"(w->global_top = H)"),
        cpp_define('Htab',"((sw_on_key_t *)pt1)"),
        cpp_define('SetHtab(X)',"(pt1 = (tagged_t *)(X))"),
        cpp_define('HtabNode',"((sw_on_key_node_t *)P)"),
        cpp_define('SetHtabNode(X)',"(P = (bcp_t)(X))"),
        cpp_define('Alts',"((try_node_t *)pt1)"),
        cpp_define('SetAlts(X)',"(pt1 = (tagged_t *)(X))"),
        %
        % address for a bytecode operand (offset measured in multiples of f_o)
        cpp_define('PoffR(X)',"BCoff(P, ((X)-1)*FTYPE_size(f_o))"),
        %
        cpp_define('BcOPCODE',"BcFetchOPCODE()"),
        cpp_define('BcP(Ty,X)',"(*(FTYPE_ctype(Ty) *)PoffR((X)))").

:- pred(wam_loop_defs/0, [unfold]).
wam_loop_defs :-
	autogen_warning_comment,
	%
	op_macros,
	wam__2_proto,
	wam_def,
	wam__2_def.

:- pred(wam_def/0, [unfold]).
wam_def :-
	"CFUN__PROTO(",
	"wam", ",",
	"int", ",",
	argdecl("goal_descriptor_t *", "desc"),
	")", " ",
	"{", fmt:nl,
	% We separate the catch block from wam__2 to make sure that
	% the implicit setjmp in EXCEPTION__CATCH do not affect
	% negatively to the optimizations in the main engine loop.
	"definition_t *", "func", ";", fmt:nl,
	"func = (definition_t *)NULL;", fmt:nl,
	goto('again'),
	label('again'),
	"EXCEPTION__CATCH({", fmt:nl, % try
	"return wam__2(Arg, desc, func);", fmt:nl,
	"}, {", fmt:nl, % catch
	"tagged_t *pt1; int i;", fmt:nl,
	code_neck, % Force neck if not done
	"X(0) = MakeSmall(ErrCode);", fmt:nl, % Error code
	"X(1) = init_atom_check(ErrFuncName);", fmt:nl, % Builtin name
	"X(2) = MakeSmall(ErrFuncArity);", fmt:nl, % Builtin arity
	"X(4) = Culprit;", fmt:nl, % Culprit arg.
	"X(3) = MakeSmall(ErrArgNo);", fmt:nl, % w. number
	"func = address_error;", fmt:nl,
	"goto again;", fmt:nl,
	"});", fmt:nl,
	"}", fmt:nl.

:- pred(wam__2_proto/0, [unfold]).
wam__2_proto :-
	"CFUN__PROTO(",
	"wam__2", ",",
	"int", ",",
	argdecl("goal_descriptor_t *", "desc"), ",",
	argdecl("definition_t *", "start_func"),
	");", fmt:nl.

:- pred(wam__2_def/0, [unfold]).
wam__2_def :-
	"CFUN__PROTO(",
	"wam__2", ",",
	"int", ",",
	argdecl("goal_descriptor_t *", "desc"), ",",
	argdecl("definition_t *", "start_func"),
	")", " ",
	"{", fmt:nl,
	wam_loop,
	"}", fmt:nl.

:- pred(wam_loop/0, [unfold]).
wam_loop :-
	wam_loop_decls,
	code_loop_begin,
	% MISCELLANEOUS SUPPORT
	%
	labeled_block('unify_t0_t1', code_unify_t0t1),
	%
	% Func, H must be live.
	labeled_block('suspend_on_t1', code_suspend_on_t1),
	%
	labeled_block('suspend_t3_on_t1', code_suspend_t3_on_t1),
	%
	labeled_block('escape_to_p2', escape_to_p2),
	%
	labeled_block('escape_to_p', escape_to_p),
	%
	% FAILING (undo goals)
	labeled_block('undo', code_undo),
	% FAILING
	labeled_block('fail', code_fail),
	%
	% ENTERING A PREDICATE:  H always live.
	% Take into account attributed variables !!
	labeled_block('enter_predicate', code_enter_pred),
	%
	labeled_block('switch_on_pred', switch_on_pred),
	%
	labeled_block('switch_on_pred_sub', code_switch_on_pred_sub),
	%
	alt_ins_dispatcher,
	%
	labeled_block('exit_toplevel', code_exit_toplevel),
	%
	labeled_block('illop', code_illop).

% Local variable declarations for the WAM loop
:- pred(wam_loop_decls/0, [unfold]).
wam_loop_decls :-
	% Seemingly available registers in an i386 processor: ebx esi edi
	"CIAO_REG_1(bcp_t, p);", fmt:nl,
%	"bcp_t p;", fmt:nl,
	"CIAO_REG_2(tagged_t *, pt1);", fmt:nl,
	"CIAO_REG_3(tagged_t *, pt2);", fmt:nl,
	%
	vardecl("intmach_t", "i"), % TODO: refine?
	% temps for terms (decreasing importance) 
	vardecl("tagged_t", "t0"),
	vardecl("tagged_t", "t1"),
	vardecl("tagged_t", "t2"),
	vardecl("tagged_t", "t3"),
	vardecl("bcp_t", "ptemp", "NULL"), % reg. decl. not critical
	%
	vardecl("int", "wam_exit_code", "0"), % halt/0, abort/0, reinitialise/0
	vardecl("instance_t *", "ins"), % clause/2, instance/2
	vardecl("worker_t *", "new_worker"), % Temp - for changes in regbanksize
	%
	"pt1" <- "NULL",
	"pt2" <- "NULL",
	%
	"t0" <- "~0",
	"t1" <- "~0",
	"t2" <- "~0",
	"t3" <- "~0",
	"i" <- "~0".

% Begin emulation in WAM loop
:- pred(code_loop_begin/0, [unfold]).
code_loop_begin :-
	[[update(mode(r))]],
	trace(wam_loop_begin),
	[[mode(M)]],
	if("start_func != NULL", (
          % Directly execute a predicate (used to call from an exception 
          % throwed from C)
          "P" <- "(bcp_t)start_func",
	  call('SetB', ["w->node"]),
	  % TODO: this should not be necessary, right?
	  % compute_Ltop("B"),
	  setmode(w), % switch_on_pred expects we are in write mode, load H
	  goto('switch_on_pred')
        )),
	[[update(mode(M))]],
	%
	if(("desc", " && ", "(desc->action & BACKTRACKING)"), (
          call0('RECOVER_WAM_STATE'),
	  goto('fail') % Probably...
        )),
	goto_ins(proceed).

:- pred(code_unify_t0t1/0, [unfold]).
code_unify_t0t1 :-
	sw_on_var("t0","i",
	  goto('t0_is_hva'),
	  goto('t0_is_cva'),
	  goto('t0_is_sva'),
	  ";"),
	% one non variable
	sw_on_var("t1","i",
	  (bind(hva,"t1","t0"), goto('unify_t0t1_done')),
	  (bind(cva,"t1","t0"), goto('unify_t0t1_done')),
	  (bind(sva,"t1","t0"), goto('unify_t0t1_done')),
	  ";"),
	% two non variables
	if("!(t1 ^= t0)", % are they equal?
	  goto('unify_t0t1_done'),
	  if("t1>=QMask", % not the same type?
	    goto('fail'),
	    if("!(t0 & TagBitComplex)", % atomic?
	      goto('fail'),
	      if("!(t0 & TagBitFunctor)", % lists?
	        ("t1 ^= t0;", % restore t1
		if("cunify_args(Arg,2,TagToCar(t0),TagToCar(t1))",
		  goto('unify_t0t1_done'),
		  goto('fail'))),
	        % structures
	        ("t1 ^= t0;", % restore t1
	         if("TagToHeadfunctor(t0) != (i=TagToHeadfunctor(t1))",
	           goto('fail'),
	           if("i&QMask", % large number
	             (for("i = LargeArity(i)-1; i>0; i--", 
	                if("*TagToArg(t0,i) != *TagToArg(t1,i)", goto('fail'))),
	              goto('unify_t0t1_done')),
	        	if("cunify_args(Arg,Arity(i),TagToArg(t0,1),TagToArg(t1,1))",
	        	   goto('unify_t0t1_done'),
	                 goto('fail'))))))))),
        label('t0_is_hva'),
	sw_on_var("t1","i",
	  if("t0==t1",
	     ";",
	     if("YoungerHeapVar(TagToHVA(t1),TagToHVA(t0))",
		bind(hva,"t1","t0"),
                bind(hva,"t0","t1"))),
	  bind(hva,"t0","t1"),
	  bind(sva,"t1","t0"),
	  bind(hva,"t0","t1")),
	goto('unify_t0t1_done'),
	%
	label('t0_is_cva'),
	sw_on_var("t1","i",
	  bind(hva,"t1","t0"),
	  if("t0==t1",
	     ";",
	     if("YoungerHeapVar(TagToCVA(t1),TagToCVA(t0))",
	       bind(cva,"t1","t0"),
	       bind(cva,"t0","t1"))),
	  bind(sva,"t1","t0"),
	  bind(cva,"t0","t1")),
	goto('unify_t0t1_done'),
	%
	label('t0_is_sva'),
	for("; TagIsSVA(t1); t1 = i",
	  ("RefSVA(i,t1);",
           if("t1 == i", 
             (if("t0==t1", 
               goto('unify_t0t1_done'),
               if("YoungerStackVar(TagToSVA(t1),TagToSVA(t0))",
	         bind(sva,"t1","t0"),
		 bind(sva,"t0","t1"))),
	      goto('unify_t0t1_done'))))),
        bind(sva,"t0","t1"),
	goto('unify_t0t1_done'),
	label('unify_t0t1_done'),
	goto_ins_dispatch.

:- pred(code_suspend_on_t1/0, [unfold]).
code_suspend_on_t1 :-
	[[update(mode(w))]],
	emul_to_goal,
	if(callexp('TagIsSVA', ["t1=X(0)"]), % t1 may have been globalised
	  call('RefSVA', ["t1","X(0)"])).

:- pred(code_suspend_t3_on_t1/0, [unfold]).
code_suspend_t3_on_t1 :-
	[[update(mode(w))]],
	% suspend the goal  t3  on  t1.  Func, H must be live.
	if(callexp('TagIsHVA', ["t1"]),
	  (load(cva, "t0"),
	   "pt1" <- "w->trail_top",
	   if(callexp('CondHVA', ["t1"]),
	     ("TrailPush(pt1,t1);",
	      "*TagToHVA(t1)" <- "t0"),
	     "*TagToHVA(t1)" <- "t0"),
	   goto('check_trail')),
	  if(("!", callexp('CondCVA', ["t1"])),
	    (heap_push("*TagToGoal(t1)"),
	     heap_push("*TagToDef(t1)"),
	     cachedreg('H', H),
	     "*TagToGoal(t1)" <- callexp('Tag', ["LST", callexp('HeapOffset', [H,-2])]),
	     "*TagToDef(t1)" <- callexp('Tag', ["LST", H]),
	     goto('no_check_trail')),
	    (load(cva, "t0"),
	     heap_push(callexp('Tag', ["LST", callexp('TagToGoal', ["t1"])])),
	     cachedreg('H', H),
	     heap_push(callexp('Tag', ["LST", callexp('HeapOffset', [H,1])])),
	     "pt1" <- "w->trail_top",
	     "TrailPush(pt1,t1);",
	     "*TagToCVA(t1)" <- "t0",
	     goto('check_trail')))),
	label('check_trail'),
	"w->trail_top" <- "pt1",
	if("ChoiceYounger(w->node,TrailOffset(pt1,CHOICEPAD))",
	  "choice_overflow(Arg,CHOICEPAD);"),
	goto('no_check_trail'),
	label('no_check_trail'),
	heap_push("t3"),
	heap_push("PointerToTerm(Func)"),
	goto_ins(proceed).

:- pred(escape_to_p2/0, [unfold]).
escape_to_p2 :-
	[[update(mode(w))]],
	"t2" <- "PointerToTerm(Func->code.intinfo)",
	goto('escape_to_p').

:- pred(escape_to_p/0, [unfold]).
escape_to_p :-	
	[[update(mode(w))]],
	emul_to_goal,
	"P" <- "ptemp",
	"X(0)" <- "t3",
	"X(1)" <- "t2",
	goto('switch_on_pred').

:- pred(code_undo/0, [unfold]).
code_undo :-
	[[update(mode(r))]],
	"w->trail_top" <- "pt2",
	"w->frame" <- "B->frame",
	"w->next_insn" <- "B->next_insn",
	call('SetE', ["NodeLocalTop(B)"]),
	"E->frame" <- "w->frame",
	"E->next_insn" <- "w->next_insn",
	"w->frame" <- "E",
	"w->next_insn" <- "failcode",
	call('SetA', ["E","Offset(E,EToY0)"]),
	setmode(w),
	"X(0)" <- "t0",
	goto('call1').

:- pred(code_fail/0, [unfold]).
code_fail :-
	[[update(mode(r))]],
	% The profiling code must be here
	profile_hook(fail),
	% (w->node->next_alt!=NULL);
	trace(failing_choicepoint),
	"Heap_Warn_Soft" <- "Int_Heap_Warn",
	call('SetB', ["w->node"]),
	%
	"ON_TABLING( MAKE_TRAIL_CACTUS_STACK; );", fmt:nl,
	%
	if("TrailYounger(pt2=w->trail_top,t1=(tagged_t)TagToPointer(B->trail_top))",
	  (do_while(
	    ("PlainUntrail(", "pt2", ",", "t0", ",", "{", fmt:nl,
	     goto('undo'),
	     "});", fmt:nl),
	    "TrailYounger(pt2,t1)"),
	  "w->trail_top" <- "pt2")),
	%
	"RestoreGtop(B);", fmt:nl,
	%
	if("(P = (bcp_t)w->next_alt) == NULL",
	  deep_backtrack),
        %
	profile_hook(redo),
	%
	if("(w->next_alt = ((try_node_t *)P)->next)==NULL",
	  (call('SetB', ["w->next_node"]),
	   "w->node" <- "B",
	  "ON_TABLING({", fmt:nl,
	  % To avoid sharing wrong trail - it might be associated to the
	  % previous frozen choice point
	  if("FrozenChpt(B)",
	    call('push_choicept', ["w","address_nd_fake_choicept"])),
	  "});", fmt:nl,
	  "SetShadowregs(B);", fmt:nl)),
	%
	"P" <- "((try_node_t *)P)->emul_p",
	"t0" <- "X(0)",
	if("!IsVar(t0)", goto_ins_dispatch),
	setmode(w),
	% TODO: check for emugen error reporting
	%bug_nondet,
	%bug_nosol,
	goto_ins_dispatch.

% % TODO: Check for emugen error reporting
% :- pred(bug_nondet/0, [unfold]).
% bug_nondet :- "a".
% bug_nondet :- "b".
% 
% :- pred(bug_nosol/0, [unfold]).
% bug_nosol :- [[ 1 = 2 ]]. % (force fail)

:- pred(deep_backtrack/0, [unfold]).
deep_backtrack :-
	% deep backtracking
	trace(deep_backtracking),
	% 7-8 contiguous moves
	"P" <- "(bcp_t)B->next_alt",
	"w->frame" <- "B->frame",
	"w->next_insn" <- "B->next_insn",
	"RestoreLtop(B);", fmt:nl,
	%
	% Dirty hack: always pop n registers (heuristic measure, I guess) and
	% see later if we need to reload more; had we needed less, we simply do
	% not use the additional ones.  I have simplified the code (see below),
	% in part because it was giving problems in some architectures: the
	% initial choicepoint of a concurrent goal was being accessed out of
	% the scope of the memory allocated for the choicepoint stack.  MCL.
	%
	% % X(0) = B->term[0];
        % % X(1) = B->term[1];
        % % X(2) = B->term[2];
        % % X(3) = B->term[3];
        % % i = ((try_node_t *)P)->node_offset;
        % % w->next_node = ChoiceCharOffset(B,-i);
        % % if (i>ArityToOffset(3)) {
        % %   S = (tagged_t *)w->next_node;
        % %   i = OffsetToArity(i)-3;
        % %   do
        % %     (w->term+2)[i] = ChoiceNext(S);
        % %   while (--i);
        % % }
	%
	"i" <- "((try_node_t *)P)->node_offset",
	"w->next_node" <- "ChoiceCharOffset(B,-i)",
	if("i>ArityToOffset(0)", (
	  "tagged_t *wt = w->term;", fmt:nl,
	  %
	  "S" <- "(tagged_t *)w->next_node",
	  "i" <- "OffsetToArity(i) - 1",
	  trace(restore_xregs_choicepoint("i")),
	  "while(i >= 0) ", "wt[i--]" <- "ChoiceNext(S)")).

:- pred(code_enter_pred/0, [unfold]).
code_enter_pred :-
	[[update(mode(w))]],
	"ON_ANDPARALLEL({", fmt:nl,
	if("Suspend == TOSUSPEND",
	  ("Suspend" <- "SUSPENDED",
	   "Wait_Acquire_lock(Waiting_For_Work_Lock);",
	   "Cond_Var_Wait(Waiting_For_Work_Cond_Var,Waiting_For_Work_Lock);",
	   "Suspend" <- "RELEASED",
	   "Release_lock(Waiting_For_Work_Lock);")),
        % if (Cancel_Goal_Exec && Safe_To_Cancel) {
        %   Cancel_Goal_Exec = FALSE;
        %   Safe_To_Cancel = FALSE;
        %   SetShadowregs(w->node);
        %   goto fail;
        % }
        % 
        % if (Cancel_Goal_Exec_Handler != NULL && Safe_To_Cancel) {
        %   Cancel_Goal_Exec_Handler = NULL;
        %   Safe_To_Cancel = FALSE;
        %   // Metacut
        %   w->node = Current_Init_ChP;
        %   w->trail_top = Current_Trail_Top;
        %   SetShadowregs(w->node);
        %   goto fail;
        % }
        "});",
        % #if defined(PARBACK)
        %   if (Suspend == CHECK_SUSP) {
        %     //Save argument registers
        %     tagged_t *Htmp = H = w->global_top;
        %     if (HeapDifference(w->global_top,Heap_End) < CONTPAD + 1 + Func->arity)
        %       explicit_heap_overflow(w, CONTPAD + 1 + Func->arity, 0);
        %     HeapPush(H,(tagged_t)P);
        %     int i;
        %     for (i = 0; i < Func->arity; i++) HeapPush(H,X(i));
        %     w->global_top = H;
        %     push_choicept(Arg,address_nd_suspension_point);
        %     w->node->term[0] = TagHVA(Htmp);
        %     //w->node->next_insn = w->misc->backInsn;
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
	if("OffHeaptop(H,Heap_Warn_Soft)",
	  ("int wake_count;",
	  %
	  if("Stop_This_Goal(Arg)", goto('exit_toplevel')),
	  %
	  "wake_count" <- "WakeCount",
	  %
	  if("OffHeaptop(H+4*wake_count,Heap_Warn)",
	    ("SETUP_PENDING_CALL(address_true);",
	     setmode(r),
	     "heap_overflow(Arg,SOFT_HEAPPAD+4*wake_count);",
	     setmode(w))),
	  if("wake_count>0",
	    if("wake_count==1",
              ("SETUP_PENDING_CALL(address_uvc);",
              "collect_one_pending_unification(Arg);", % does not touch H
              "DEREF(t0,X(1));",
              if(callexp('TagIsCVA', ["t0"]),
	        (% X(1)=*TagToGoal(t0);
	         "X(1)" <- "t0",
	         % patch prev. SETUP_PENDING_CALL
	         call('Setfunc', ["address_ucc"])))),
	      % wake_count > 1
	      ([[update(mode(w))]],
	       "SETUP_PENDING_CALL(address_pending_unifications);",
	       setmode(r),
	       "collect_pending_unifications(Arg,wake_count);",
	       setmode(w)))),
	  if("OffStacktop(w->frame,Stack_Warn)",
	    ("SETUP_PENDING_CALL(address_true);",
	     "stack_overflow(Arg);")),
	  if("Int_Heap_Warn != (Heap_Warn_Soft = Heap_Warn)",
	    ("SETUP_PENDING_CALL(address_help);",
	     "control_c_normal(Arg);")))),
	goto('switch_on_pred').

:- pred(switch_on_pred/0, [unfold]).
switch_on_pred :-
	"i" <- "Func->enter_instr",
	goto('switch_on_pred_sub').

:- pred(pred_enter_undefined/0, [unfold]).
pred_enter_undefined :-
	[[update(mode(w))]],
	case('ENTER_FASTCODE_INDEXED'), fmt:nl,
	goto('enter_undefined'),
	case('ENTER_FASTCODE'), fmt:nl,
	goto('enter_undefined'),
	case('ENTER_UNDEFINED'), fmt:nl,
	label('enter_undefined'),
	pred_trace("\"U\""),
	"ptemp" <- "(bcp_t)address_undefined_goal",
	goto('escape_to_p').

:- pred(pred_enter_interpreted/0, [unfold]).
pred_enter_interpreted :-
	[[update(mode(w))]],
	case('ENTER_INTERPRETED'), fmt:nl,
	% pred_trace("\"I\""),
	"ptemp" <- "(bcp_t)address_interpret_c_goal",
	goto('escape_to_p2').

:- pred(pred_enter_c/0, [unfold]).
pred_enter_c :-
	[[update(mode(w))]],
	case('ENTER_C'), fmt:nl,
	pred_trace("\"C\""),
	setmode(r),
	% Changed by DCG to handle errors in Prolog
	"i" <- "(*Func->code.cinfo)(Arg)",
	if("Expanded_Worker",
	  (trace(worker_expansion_blt),
	  if("desc == NULL",
	    (% JFKK this is temp sometimes wam is called without gd
	     "fprintf(stderr, \"bug: invalid WAM expansion\\n\");", fmt:nl,
	     "abort();", fmt:nl)),
	  "desc->worker_registers = Arg = Expanded_Worker;", fmt:nl,
	  "Expanded_Worker" <- "NULL")),
	if("i",goto_ins(proceed),goto('fail')).

:- pred(pred_enter_builtin_true/0, [unfold]).
pred_enter_builtin_true :-
	[[update(mode(w))]],
	case('BUILTIN_TRUE'),
	pred_trace("\"B\""),
	goto_ins(proceed).

:- pred(pred_enter_builtin_fail/0, [unfold]).
pred_enter_builtin_fail :-
	[[update(mode(w))]],
	case('BUILTIN_FAIL'),
	pred_trace("\"B\""),
	goto('fail').

:- pred(pred_enter_builtin_current_instance/0, [unfold]).
pred_enter_builtin_current_instance :-
	[[update(mode(w))]],
	case('BUILTIN_CURRENT_INSTANCE'),
	pred_trace("\"B\""),
	setmode(r),
	"ins" <- "current_instance(Arg)",
	if("!ins", goto('fail')),
	"P" <- "(bcp_t)ins->emulcode",
	goto_ins_dispatch.

:- pred(pred_enter_builtin_compile_term/0, [unfold]).
pred_enter_builtin_compile_term :-
	[[update(mode(w))]],
	case('BUILTIN_COMPILE_TERM'),
	pred_trace("\"B\""),
	setmode(r),
	if("!compile_term(Arg, &new_worker)",goto('fail')),
	if("new_worker",
	  (if("desc == NULL",
	    (% JFKK this is temp sometimes wam is called without gd
	     "fprintf(stderr, \"bug: invalid WAM expansion\\n\");", fmt:nl,
	     "abort();", fmt:nl)),
	  "desc->worker_registers = Arg = new_worker;", fmt:nl,
	  trace(worker_expansion_cterm))),
	goto_ins(proceed).

:- pred(pred_enter_builtin_instance/0, [unfold]).
pred_enter_builtin_instance :-
	[[update(mode(w))]],
	case('BUILTIN_INSTANCE'),
	% ASSERT: X(2) is a dereferenced integer
	pred_trace("\"B\""),
	load(hva, "X(3)"),
	"ins" <- "TagToInstance(X(2))",
	"P" <- "(bcp_t)ins->emulcode",
	goto_ins_dispatch.

:- pred(pred_enter_builtin_geler/0, [unfold]).
pred_enter_builtin_geler :-
	[[update(mode(w))]],
	case('BUILTIN_GELER'),
	pred_trace("\"B\""),
	"t1" <- "X(0)",
	deref_sw("t1","t0",";"),
	"t3" <- "X(1)",
	deref_sw("t3","t0",";"),
	call('Setfunc', [callexp('find_definition', ["predicates_location","t3","&w->structure","TRUE"])]),
	goto('suspend_t3_on_t1').

% Like pred_enter_builtin_syscall/0, bug fails on undefined
:- pred(pred_enter_builtin_nodebugcall/0, [unfold]).
pred_enter_builtin_nodebugcall :-
	[[update(mode(w))]],
	case('BUILTIN_NODEBUGCALL'),
	pred_trace("\"B\""),
	"t0" <- "X(0)",
	deref_sw("t0","X(0)",";"),
	do_builtin_call(nodebugcall).

% Like pred_enter_builtin_call/0, bug ignores Current_Debugger_Mode
:- pred(pred_enter_builtin_syscall/0, [unfold]).
pred_enter_builtin_syscall :-
	[[update(mode(w))]],
	case('BUILTIN_SYSCALL'),
	pred_trace("\"B\""),
	"t0" <- "X(0)", 
	deref_sw("t0","X(0)",";"),
	goto('call1'),
	label('call1'),
	%
	do_builtin_call(syscall).

:- pred(pred_enter_builtin_call/0, [unfold]).
pred_enter_builtin_call :-
	[[update(mode(w))]],
	case('BUILTIN_CALL'),
	pred_trace("\"B\""),
	"t0" <- "X(0)",
	deref_sw("t0","X(0)",";"),
	do_builtin_call(call).

:- pred(do_builtin_call/1, [unfold]).
do_builtin_call(CallMode) :-
	call('Setfunc', [callexp('find_definition', ["predicates_location","t0","&w->structure","FALSE"])]),
	% Undefined?
	( [[ CallMode = nodebugcall ]],
	  if("Func==NULL",goto('fail'))
	; ( [[ CallMode = syscall ]]
	  ; [[ CallMode = call ]]
	  ),
	  if("Func==NULL",
	    (call('Setfunc', ["address_undefined_goal"]),
	     goto('switch_on_pred')))
	),
	% Debug hook?
	( ( [[ CallMode = nodebugcall ]]
	  ; [[ CallMode = syscall ]]
	  )
	; [[ CallMode = call ]],
	  if("Current_Debugger_Mode != atom_off",
	    (call('Setfunc', ["address_trace"]),
	     goto('switch_on_pred')))
	),
	%
	"i" <- "Func->enter_instr",
	goto('call4').

:- pred(pred_call4/0, [unfold]).
pred_call4 :-
	label('call4'),
	switch("i", 
	  (pred_call_interpreted,
	  pred_call_builtin_dif,
	  pred_call_spypoint,
	  pred_call_waitpoint,
	  pred_call5,
	  pred_call_default)).

:- pred(pred_call_interpreted/0, [unfold]).
pred_call_interpreted :-
	[[update(mode(w))]],
	case('ENTER_INTERPRETED'),
	% pred_trace("\"I\""),
	"X(1)" <- "PointerToTerm(Func->code.intinfo)",
	call('Setfunc', ["address_interpret_goal"]),
	goto('switch_on_pred').

:- pred(pred_call_builtin_dif/0, [unfold]).
pred_call_builtin_dif :-
	[[update(mode(w))]],
	case('BUILTIN_DIF'),
	pred_trace("\"B\""),
	"pt1" <- "w->structure",
	call('RefHeapNext', ["t0","pt1"]),
	deref_heap_sw("t0","t2",";"),
	call('RefHeapNext', ["t1","pt1"]),
	deref_heap_sw("t1","t2",";"),
	goto('dif1').

:- pred(pred_call_spypoint/0, [unfold]).
pred_call_spypoint :-
	[[update(mode(w))]],
	case('SPYPOINT'),
	if("!Func->properties.wait", goto('call5')).

:- pred(pred_call_waitpoint/0, [unfold]).
pred_call_waitpoint :-
	[[update(mode(w))]],
	case('WAITPOINT'),
	call('RefHeap', ["t0","w->structure"]),
	deref_heap_sw("t0","t1",
	  ("t3" <- "X(0)",
	   goto('suspend_t3_on_t1'))),
	goto('call5').

:- pred(pred_call5/0, [unfold]).
pred_call5 :-
	label('call5'),
	"i" <- "Func->predtyp",
	goto('call4').

:- pred(pred_call_default/0, [unfold]).
pred_call_default :-
	[[update(mode(w))]],
	label('default'),
	if("(t0 = Func->arity)",
          (setmode(r),
	   "pt1" <- "w->term",
	   "pt2" <- "w->structure",
	   do_while(
	     call('PushRefHeapNext', ["pt1","pt2"]),
	     "--t0"),
           setmode(w))),
	goto('switch_on_pred_sub').

% NOTE: see prolog_dif
:- pred(pred_enter_builtin_dif/0, [unfold]).
pred_enter_builtin_dif :-
	[[update(mode(w))]],
	case('BUILTIN_DIF'),
	pred_trace("\"B\""),
	"t0" <- "X(0)",
	deref_sw("t0","t2",";"),
	"t1" <- "X(1)",
	deref_sw("t1","t2",";"),
	"w->structure" <- "NULL",
	goto('dif1'),
	% check fast cases first
	label('dif1'),
	[[update(mode(w))]],
	if("t0==t1",
	  goto('fail'),
	  if("(!IsVar(t0 & t1)) && (IsAtomic(t0) || IsAtomic(t1))",
	    goto_ins(proceed),
	    ("X(0)" <- "t0",
	     "X(1)" <- "t1",
	     setmode(r),
	     goto('dif2')))),
	label('dif2'),
	[[update(mode(r))]],
	if("!prolog_dif(Arg,Func)", goto('fail')),
	goto_ins(proceed).

:- pred(pred_enter_builtin_abort/0, [unfold]).
pred_enter_builtin_abort :-
	[[update(mode(w))]],
	case('BUILTIN_ABORT'),
	% cut all the way and fail, leaving wam with a return code
	pred_trace("\"B\""),
	"t0" <- "X(0)",
	deref_sw("t0","t1",";"),
	"wam_exit_code" <- "GetSmall(t0)",
	"w->next_node" <- "InitialNode",
	do_cut,
	goto('fail').

:- pred(pred_enter_spypoint/0, [unfold]).
pred_enter_spypoint :-
	[[update(mode(w))]],
	case('SPYPOINT'),
	if("Current_Debugger_Mode != atom_off",
	  ("ptemp" <- "(bcp_t)address_trace",
	   goto('escape_to_p'))),
	if("!Func->properties.wait", goto('nowait')),
	goto('waitpoint').

:- pred(pred_enter_waitpoint/0, [unfold]).
pred_enter_waitpoint :-
	[[update(mode(w))]],
	case('WAITPOINT'),
	label('waitpoint'),
	"t1" <- "X(0)",
	deref_sw("t1","X(0)",goto('suspend_on_t1')),
	goto('nowait'),
	label('nowait'),
	"i" <- "Func->predtyp",
	goto('switch_on_pred_sub').

:- pred(pred_enter_breakpoint/0, [unfold]).
pred_enter_breakpoint :-
	[[update(mode(w))]],
	case('BREAKPOINT'),
	"i" <- "Func->predtyp",
	goto('switch_on_pred_sub').

:- pred(pred_enter_compactcode_indexed/0, [unfold]).
pred_enter_compactcode_indexed :-
	[[update(mode(w))]],
	case('ENTER_PROFILEDCODE_INDEXED'),
	goto('enter_compactcode_indexed'),
	case('ENTER_COMPACTCODE_INDEXED'),
	label('enter_compactcode_indexed'),
	pred_trace("\"E\""),
	"t0" <- "X(0)",
	deref_sw("t0","X(0)", tryeach("Func->code.incoreinfo->varcase")),
	setmode(r),
	% non variable
	if("t0 & TagBitComplex",
	  if("t0 & TagBitFunctor",
            ("S" <- "TagToArg(t0,0)",
	     "t1" <- "HeapNext(S)"),
	    ("S" <- "TagToLST(t0)",
	     tryeach("Func->code.incoreinfo->lstcase"))),
	   "t1" <- "t0"),
	%
	call('SetHtab', ["Func->code.incoreinfo->othercase"]),
	%
	for(("i=0, t2=t1, t1 &= Htab->mask;",
	     ";",
	     "i+=sizeof(sw_on_key_node_t), t1=(t1+i) & Htab->mask"),
	    (call('SetHtabNode', ["SW_ON_KEY_NODE_FROM_OFFSET(Htab, t1)"]),
	     if("HtabNode->key==t2 || !HtabNode->key", 
	       tryeach("HtabNode->value.try_chain")))).

:- pred(pred_enter_compactcode/0, [unfold]).
pred_enter_compactcode :-
	[[update(mode(w))]],
	case('ENTER_PROFILEDCODE'),
	goto('enter_compactcode'),
	case('ENTER_COMPACTCODE'),
	label('enter_compactcode'),
	pred_trace("\"E\""),
	[[update(mode(w))]],
	tryeach("Func->code.incoreinfo->varcase").

:- pred(tryeach/1, [unfold]).
tryeach(Alts) :-
	"{",
	call('SetAlts', [Alts]),
	( [[ mode(r) ]], goto('tryeach_r')
	; [[ mode(w) ]], goto('tryeach_w')
	),
	"}".

:- pred(code_switch_on_pred_sub/0, [unfold]).
code_switch_on_pred_sub :-
	switch("i",
	  (pred_enter_undefined,
          pred_enter_interpreted,
	  pred_enter_c,
	  pred_enter_builtin_true,
	  pred_enter_builtin_fail,
	  pred_enter_builtin_current_instance,
	  pred_enter_builtin_compile_term,
	  pred_enter_builtin_instance,
	  pred_enter_builtin_geler,
	  pred_enter_builtin_nodebugcall,
	  pred_enter_builtin_syscall,
	  pred_enter_builtin_call,
	  pred_call4,
	  pred_enter_builtin_dif,
	  pred_enter_builtin_abort,
	  pred_enter_spypoint,
	  pred_enter_waitpoint,
	  pred_enter_breakpoint,
	  pred_enter_compactcode_indexed,
	  pred_enter_compactcode)).

:- pred(code_exit_toplevel/0, [unfold]).
code_exit_toplevel :-
	"w->insn" <- "P",
	% What should we save here? MCL
	% w->node = B;
	% w->frame = E->frame;
	if("desc && (desc->action & KEEP_STACKS)",
	  (% We may backtrack
	   call0('SAVE_WAM_STATE'))),
	% We may have been signaled and jumped here from enter_predicate:
	if("Stop_This_Goal(Arg)",
	  "wam_exit_code" <- "WAM_INTERRUPTED"),
	trace(wam_loop_exit),
	"return wam_exit_code;".

:- pred(code_illop/0, [unfold]).
code_illop :-
	"SERIOUS_FAULT(\"unimplemented WAM instruction\");", fmt:nl.

% Alternative and instruction dispatcher
:- pred(alt_ins_dispatcher/0, [unfold]).
alt_ins_dispatcher :-
	alt_ins_dispatcher(r),
	alt_ins_dispatcher(w).

% Alternative and instruction dispatcher (read or write mode)
:- pred(alt_ins_dispatcher/1, [unfold]).
alt_ins_dispatcher(Mode) :-
	[[ update(mode(Mode)) ]],
	alt_dispatcher,
	ins_dispatcher.

:- pred(alt_dispatcher/0, [unfold]).
alt_dispatcher :-
	( [[ mode(r) ]], [[ TryEach = 'tryeach_r' ]]
	; [[ mode(w) ]], [[ TryEach = 'tryeach_w' ]]
	),
	label(TryEach),
	%
	gauge_incr_counter(alts),
	%
	[[ Alts = "Alts" ]],
	( [[ mode(r) ]], [[ EmulP = (Alts, "->emul_p2") ]]
	; [[ mode(w) ]], [[ EmulP = (Alts, "->emul_p") ]]
	),
	"P" <- EmulP,
	%
        "w->next_node" <- "w->node",
        if(("(w->next_alt","=",Alts,"->next)!=NULL"),
          (call('SetB', ["w->node"]),
          compute_Ltop("B"),
          call('SetB', ["ChoiceCharOffset(B,w->next_alt->node_offset)"]),
          "w->node" <- "B",
          "ON_DEBUG_NODE({",
	  "B->functor" <- "NULL",
	  "});", fmt:nl,
          "B->next_alt" <- "NULL",
          "B->trail_top" <- "w->trail_top",
	  cachedreg('H',H),
          call('SaveGtop', ["B", H]),
          call('NewShadowregs', [H]),
	  trace(create_choicepoint),
          % segfault patch -- jf
	  maybe_choice_overflow)),
	goto_ins_dispatch.

:- pred(goto_ins_dispatch/0, [unfold]).
goto_ins_dispatch :-
	ins_dispatch_label(DispatchLabel),
	goto(DispatchLabel).

:- pred(ins_dispatch_label/1, [unfold]).
% TODO: define special meta-predicates? (Label is a output meta-argument)
:- pred(ins_dispatch_label(Label), [in_moded('ins_dispatch_label/1')]).
ins_dispatch_label(Label) :-
	( [[ mode(r) ]], [[ Label = 'ReadMode' ]] /* Here with H in memory. */
	; [[ mode(w) ]], [[ Label = 'WriteMode' ]] /* Here with H in register. */
	).

:- pred(ins_dispatcher/0, [unfold]).
ins_dispatcher :-
	ins_dispatch_label(Label),
	label(Label),
	[[ all_insns(Insns) ]],
	switch("BcOPCODE",
	  (% (all instructions)
	  '$foreach'(Insns, inswrap),
	  label('default'),
	  goto('illop'))).

% Wrapper for instructions
:- pred(inswrap/1, [unfold]).
inswrap(I) :-
	[[ prop(I, optional(Flag)) ]],
	% Emit optional instructions (based on C preprocessor flags)
	cpp_if_defined(Flag),
	inswrap_(I),
	cpp_endif.
inswrap(I) :-
	[[ not(prop(I, optional(_))) ]],
	inswrap_(I).

:- pred(inswrap_/1, [unfold]).
inswrap_(I) :-
	[[ prop(I, format(Format)) ]],
	[[ update(format(Format)) ]],
	ins_label(I),
	ins_case(I),
	inswrap__(I).

:- pred(inswrap__/1, [unfold]).
inswrap__(I) :-
	[[ prop(I, in_mode(M)) ]],
	in_mode(M, I, '$unfold'(I)).
inswrap__(I) :-
	[[ not(prop(I, in_mode(_))) ]],
	'$unfold'(I).

% 'case' statement for the instruction
:- pred(ins_case/1, [unfold]).
ins_case(Ins) :-
	[[ uppercase(Ins, InsUp) ]],
	case(InsUp).

% 'label' statement for the instruction
:- pred(ins_label/1, [unfold]).
ins_label(Ins) :-
	[[ mode(M) ]],
	( [[ prop(Ins, label(M)) ]],
	  [[ get_ins_label(Ins, M, Label) ]],
	  label(Label)
	; [[ not(prop(Ins, label(M))) ]]
	).

:- doc(section, "Instruction set switch").
% NOTE: declaration order is important (for performance)

:- iset(instruction_set/0).
instruction_set :- iset_init.
instruction_set :- iset_call.
instruction_set :- iset_put.
instruction_set :- iset_get1.
instruction_set :- iset_cut.
instruction_set :- iset_choice.
instruction_set :- iset_misc1.
instruction_set :- iset_get2.
instruction_set :- entry(branch).
instruction_set :- iset_blt.
instruction_set :- entry(get_constraint).
instruction_set :- iset_unify.
instruction_set :- iset_u2.
instruction_set :- iset_misc2.

:- pred(iset_init/0, [unfold]).
iset_init :-
	( entry(inittrue)
	; entry(firsttrue_n)
	; entry(initcallq) ; entry(initcall)
	).
:- pred(iset_call/0, [unfold]).
iset_call :- [[mode(r)]],
	( entry(firstcall_nq) ; entry(firstcall_n)
	; entry(firstcall_8q) ; entry(firstcall_8)
	; entry(firstcall_7q) ; entry(firstcall_7)
	; entry(firstcall_6q) ; entry(firstcall_6)
	; entry(firstcall_5q) ; entry(firstcall_5)
	; entry(firstcall_4q) ; entry(firstcall_4)
	; entry(firstcall_3q) ; entry(firstcall_3)
	; entry(firstcall_2q) ; entry(firstcall_2)
	; entry(firstcall_1q) ; entry(firstcall_1)
	; entry(firstcallq) ; entry(firstcall)
	; entry(call_nq) ; entry(call_n)
	; entry(call_8q) ; entry(call_8)
	; entry(call_7q) ; entry(call_7)
	; entry(call_6q) ; entry(call_6)
	; entry(call_5q) ; entry(call_5)
	; entry(call_4q) ; entry(call_4)
	; entry(call_3q) ; entry(call_3)
	; entry(call_2q) ; entry(call_2)
	; entry(call_1q) ; entry(call_1)
	; entry(callq) ; entry(call)
	; entry(lastcall_nq) ; entry(lastcall_n)
	; entry(lastcall_8q) ; entry(lastcall_8)
	; entry(lastcall_7q) ; entry(lastcall_7)
	; entry(lastcall_6q) ; entry(lastcall_6)
	; entry(lastcall_5q) ; entry(lastcall_5)
	; entry(lastcall_4q) ; entry(lastcall_4)
	; entry(lastcall_3q) ; entry(lastcall_3)
	; entry(lastcall_2q) ; entry(lastcall_2)
	; entry(lastcall_1q) ; entry(lastcall_1)
	; entry(lastcallq) ; entry(lastcall)
	; entry(executeq) ; entry(execute)
	).
iset_call :- [[mode(w)]],
	( entry(firstcall_nq)
	; entry(firstcall_8q)
	; entry(firstcall_7q)
	; entry(firstcall_6q)
	; entry(firstcall_5q)
	; entry(firstcall_4q)
	; entry(firstcall_3q)
	; entry(firstcall_2q)
	; entry(firstcall_1q)
	; entry(firstcallq)
	; entry(call_nq)
	; entry(call_8q)
	; entry(call_7q)
	; entry(call_6q)
	; entry(call_5q)
	; entry(call_4q)
	; entry(call_3q)
	; entry(call_2q)
	; entry(call_1q)
	; entry(callq)
	; entry(lastcall_nq)
	; entry(lastcall_8q)
	; entry(lastcall_7q)
	; entry(lastcall_6q)
	; entry(lastcall_5q)
	; entry(lastcall_4q)
	; entry(lastcall_3q)
	; entry(lastcall_2q)
	; entry(lastcall_1q)
	; entry(lastcallq)
	; entry(firstcall_n)
	; entry(firstcall_8)
	; entry(firstcall_7)
	; entry(firstcall_6)
	; entry(firstcall_5)
	; entry(firstcall_4)
	; entry(firstcall_3)
	; entry(firstcall_2)
	; entry(firstcall_1)
	; entry(firstcall)
	; entry(call_n)
	; entry(call_8)
	; entry(call_7)
	; entry(call_6)
	; entry(call_5)
	; entry(call_4)
	; entry(call_3)
	; entry(call_2)
	; entry(call_1)
	; entry(call)
	; entry(lastcall_n)
	; entry(lastcall_8)
	; entry(lastcall_7)
	; entry(lastcall_6)
	; entry(lastcall_5)
	; entry(lastcall_4)
	; entry(lastcall_3)
	; entry(lastcall_2)
	; entry(lastcall_1)
	; entry(lastcall)
	; entry(execute) ; entry(executeq)
	).
:- pred(iset_put/0, [unfold]).
iset_put :-
	( entry(put_x_void)
	; entry(put_x_variable)
	; entry(put_xval_xval)
	; entry(put_x_value)
	; entry(put_x_unsafe_value)
	; entry(put_y_first_variable)
	; entry(put_y_variable)
	; entry(put_yfvar_yvar)
	; entry(put_yvar_yvar)
	; [[mode(r)]], entry(put_yval_yval)
	; [[mode(w)]],
	  ( entry(put_constantq) ; entry(put_constant)
	  ; entry(put_nil)
	  ; entry(put_largeq) ; entry(put_large)
	  ; entry(put_structureq) ; entry(put_structure)
	  ; entry(put_list)
	  )
	; entry(put_y_value)
	; entry(put_y_unsafe_value)
	; [[mode(r)]],
	  ( entry(put_constantq) ; entry(put_constant)
	  ; entry(put_nil)
	  ; entry(put_largeq) ; entry(put_large)
	  ; entry(put_structureq) ; entry(put_structure)
	  ; entry(put_list)
	  )
	; [[mode(w)]], entry(put_yval_yval)
	; entry(put_yval_yuval)
	; entry(put_yuval_yval)
	; entry(put_yuval_yuval)
	).
:- pred(iset_blt/0, [unfold]).
iset_blt :-
	( entry(function_1q) ; entry(function_1)
	; entry(function_2q) ; entry(function_2)
	; entry(builtin_1q) ; entry(builtin_1)
	; entry(builtin_2q) ; entry(builtin_2)
	; entry(builtin_3q) ; entry(builtin_3)
	; entry(retry_instance)
	).
:- pred(iset_get1/0, [unfold]).
iset_get1 :-
	( entry(get_x_value)
	; entry(get_y_first_value)
	; entry(get_y_value)
	; entry(get_constantq) ; entry(get_constant)
	; entry(get_largeq) ; entry(get_large)
	; entry(get_structureq) ; entry(get_structure)
	; entry(get_nil)
	; entry(get_list)
	; entry(get_constant_neck_proceedq) ; entry(get_constant_neck_proceed)
	; entry(get_nil_neck_proceed)
	).
:- pred(iset_cut/0, [unfold]).
iset_cut :-
	( entry(cutb_x)
	; entry(cutb_x_neck)
	; entry(cutb_neck)
	; entry(cutb_x_neck_proceed)
	; entry(cutb_neck_proceed)
	; entry(cute_x)
	; entry(cute_x_neck)
	; entry(cute_neck)
	; [[mode(r)]], entry(cutf_x)
	; [[mode(r)]], entry(cutf)
	; [[mode(w)]], entry(cutf)
	; [[mode(w)]], entry(cutf_x)
	; entry(cut_y)
	).
:- pred(iset_choice/0, [unfold]).
iset_choice :-
	( entry(choice_x)
	; entry(choice_yf)
	; entry(choice_y)
	).
:- pred(iset_misc1/0, [unfold]).
iset_misc1 :-
	( entry(kontinue)
	; entry(leave)
	; entry(exit_toplevel)
	; entry(retry_cq) ; entry(retry_c)
	).
:- pred(iset_get2/0, [unfold]).
iset_get2 :-
	( entry(get_structure_x0q) ; entry(get_structure_x0)
	; entry(get_large_x0q) ; entry(get_large_x0)
	; entry(get_constant_x0q) ; entry(get_constant_x0)
	; entry(get_nil_x0)
	; entry(get_list_x0)
	; entry(get_xvar_xvar)
	; entry(get_x_variable)
	; entry(get_y_first_variable)
	; entry(get_y_variable)
	; entry(get_yfvar_yvar)
	; entry(get_yvar_yvar)
	).
:- pred(iset_unify/0, [unfold]).
iset_unify :-
	( entry(unify_void)
	; [[mode(r)]],
	  ( entry(unify_void_1)
	  ; entry(unify_void_2)
	  ; entry(unify_void_3)
	  ; entry(unify_void_4)
	  )
	; [[mode(w)]],
	  ( entry(unify_void_4)
	  ; entry(unify_void_3)
	  ; entry(unify_void_2)
	  ; entry(unify_void_1)
	  )
	; entry(unify_x_variable)
	; entry(unify_x_value)
	; entry(unify_x_local_value)
	; entry(unify_y_first_variable)
	; entry(unify_y_variable)
	; entry(unify_y_first_value)
	; entry(unify_y_value)
	; entry(unify_y_local_value)
	; entry(unify_constantq) ; entry(unify_constant)
	; entry(unify_largeq) ; entry(unify_large)
	; entry(unify_structureq) ; entry(unify_structure)
	; entry(unify_nil)
	; entry(unify_list)
	; entry(unify_constant_neck_proceedq) ; entry(unify_constant_neck_proceed)
	; entry(unify_nil_neck_proceed)
	).
:- pred(iset_u2/0, [unfold]).
iset_u2 :-
	( entry(u2_void_xvar)
	; entry(u2_void_yfvar)
	; entry(u2_void_yvar)
	; entry(u2_void_xval)
	; entry(u2_void_xlval)
	; entry(u2_void_yfval)
	; entry(u2_void_yval)
	; entry(u2_void_ylval)
	; entry(u2_xvar_void)
	; entry(u2_xvar_xvar)
	; entry(u2_xvar_yfvar)
	; entry(u2_xvar_yvar)
	; entry(u2_xvar_xval)
	; entry(u2_xvar_xlval)
	; entry(u2_xvar_yfval)
	; entry(u2_xvar_yval)
	; entry(u2_xvar_ylval)
	; entry(u2_yfvar_void)
	; entry(u2_yvar_void)
	; entry(u2_yfvar_xvar)
	; entry(u2_yvar_xvar)
	; entry(u2_yfvar_yvar)
	; entry(u2_yvar_yvar)
	; entry(u2_yfvar_xval)
	; [[mode(r)]], entry(u2_yfvar_xlval)
	; [[mode(r)]], entry(u2_yvar_xval)
	; [[mode(w)]], entry(u2_yvar_xval)
	; [[mode(w)]], entry(u2_yfvar_xlval)
	; entry(u2_yvar_xlval)
	; entry(u2_yfvar_yval)
	; [[mode(r)]], entry(u2_yfvar_ylval)
	; [[mode(r)]], entry(u2_yvar_yval)
	; [[mode(w)]], entry(u2_yvar_yval)
	; [[mode(w)]], entry(u2_yfvar_ylval)
	; entry(u2_yvar_ylval)
	; entry(u2_yfval_void)
	; entry(u2_yfval_xvar)
	; entry(u2_yfval_yfval)
	; entry(u2_yfval_xval)
	; entry(u2_yfval_xlval)
	; entry(u2_yfval_yval)
	; entry(u2_yfval_ylval)
	; entry(u2_xval_void)
	; entry(u2_xlval_void)
	; entry(u2_xval_xvar)
	; entry(u2_xlval_xvar)
	; entry(u2_xval_yfvar)
	; [[mode(r)]], entry(u2_xlval_yfvar)
	; [[mode(r)]], entry(u2_xval_yvar)
	; [[mode(w)]], entry(u2_xval_yvar)
	; [[mode(w)]], entry(u2_xlval_yfvar)
	; entry(u2_xlval_yvar)
	; entry(u2_xval_xval)
	; entry(u2_xval_xlval)
	; entry(u2_xlval_xval)
	; entry(u2_xlval_xlval)
	; entry(u2_xval_yfval)
	; entry(u2_xlval_yfval)
	; entry(u2_xval_yval)
	; entry(u2_xval_ylval)
	; entry(u2_xlval_yval)
	; entry(u2_xlval_ylval)
	; entry(u2_yval_void)
	; entry(u2_ylval_void)
	; entry(u2_yval_xvar)
	; entry(u2_ylval_xvar)
	; entry(u2_yval_yvar)
	; entry(u2_ylval_yvar)
	; entry(u2_yval_yfval)
	; entry(u2_ylval_yfval)
	; entry(u2_yval_xval)
	; entry(u2_yval_xlval)
	; entry(u2_ylval_xval)
	; entry(u2_ylval_xlval)
	; entry(u2_yval_yval)
	; entry(u2_yval_ylval)
	; entry(u2_ylval_yval)
	; entry(u2_ylval_ylval)
	).
:- pred(iset_misc2/0, [unfold]).
iset_misc2 :-
	( entry(bump_counterq) ; entry(bump_counter)
	; entry(counted_neckq) ; entry(counted_neck)
	; entry(fail)
	; entry(heapmargin_callq) ; entry(heapmargin_call)
	; entry(neck)
	; entry(dynamic_neck_proceed)
	; entry(neck_proceed)
	; entry(proceed)
	; entry(restart_point)
	).



