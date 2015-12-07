/*
 *  registers.h
 *
 *  Principal WAM areas and registers.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_REGISTERS_H
#define _CIAO_REGISTERS_H

/* Access macros for the principal WAM registers, bytecode offsets, etc. */

/* (needed for USE_DYNAMIC_ATOM_SIZE) */
#include <ciao/alloc_init.h>

/*
  Macros CIAO_REG_n, used to declare a variable and its type, force
  the GCC compiler to store a variable in a register.  In principle,
  they are ranked in ascending order of effectiveness, according to
  empiric measurements.  This can of course change if compilers (GCC,
  in this case) change the way registers are assigned, and it makes
  sense to have an order because there are processors whose
  instruction set is not orthogonal to register names.  n currently
  goes from 1 to 4, but it can be enlarged for additional
  architectures.

  Undefined the macro MAP_TO_HW_REGISTERS to disable this mapping a
  fall back to what the compiler thinks is reasonable.

  Register assignments (for registers 1 to 3) have been made for i686.
*/

#if defined(__clang__)
/* clang/llvm does not yet support explicit registers variables */
#else
#define MAP_TO_HW_REGISTERS
#endif

/* TODO: missing x86_64 */
#if defined(__GNUC__) && defined(MAP_TO_HW_REGISTERS) 
# if defined(i686) && (defined(LINUX) || defined(BSD))
#  define CIAO_REG_1(Type, Var) register Type Var asm("esi")
#  define CIAO_REG_2(Type, Var) register Type Var asm("ebx")
#  define CIAO_REG_3(Type, Var) register Type Var asm("edi")
#  define CIAO_REG_4(Type, Var) Type Var
# elif defined(i686) && defined(DARWIN)
#  define CIAO_REG_1(Type, Var) register Type Var asm("esi")
#  define CIAO_REG_2(Type, Var) register Type Var asm("edi")
#  define CIAO_REG_3(Type, Var) Type Var
#  define CIAO_REG_4(Type, Var) Type Var
# else
#  define CIAO_REG_1(Type, Var) Type Var
#  define CIAO_REG_2(Type, Var) Type Var
#  define CIAO_REG_3(Type, Var) Type Var
#  define CIAO_REG_4(Type, Var) Type Var
# endif
#else
# define CIAO_REG_1(Type, Var) Type Var
# define CIAO_REG_2(Type, Var) Type Var
# define CIAO_REG_3(Type, Var) Type Var
# define CIAO_REG_4(Type, Var) Type Var
#endif

#define Arg w

#define WToX0		(SIZEOF_FLEXIBLE_STRUCT(worker_t, tagged_t, 0)/sizeof(tagged_t))
#define Xb(I)		(*CharOffset(w,I)) /* I as bytecode operand */
#define X(I)		(w->term[I]) /* I as zero-based */
#define Xop(X)	(((X)+WToX0)*sizeof(tagged_t))
#define Xinv(X)	(((X)/sizeof(tagged_t))-WToX0)

/* # X regs used for control in choicepoints for dynamic code */
/*
   X(0) -
   X(1) -
   X(2) - x2_next clause pointer / handle.
   X(3) -
   X(4) - clock (used even in conc. predicates, although ignored).
   X(5) - x5_next clause pointer / handle.
   ------ The next ones, only meaningful for concurrent predicates.
   X(6) - predicate root (needed in case there are no clause pointers - MCL).
   X(7) - blocking/non-blocking and exited/non exited (MCL).
   X(8) - pointer to previous dynamic concurrent choicepoint.
*/


#if defined(ANDPARALLEL) || defined(PARBACK)
#define LOCAL     FALSE
#define REMOTE    TRUE
#endif

#define X2_CHN 2
#define ClockSlot 4
#define X5_CHN 5
#define RootArg 6
#define InvocationAttr 7
#define PrevDynChpt 8
#define DynamicPreserved 9

#define BLOCKIDX (1<<0)
#define EXECIDX  (1<<1)

#define SET_BLOCKING(arg) (arg) = ((arg) | BLOCKIDX)
#define SET_NONBLOCKING(arg) (arg) = ((arg) & ~BLOCKIDX)
#define IS_BLOCKING(arg) ((arg) & BLOCKIDX)
#define IS_NONBLOCKING(arg) !((arg) & BLOCKIDX)

#define SET_EXECUTING(arg) (arg) = ((arg) | EXECIDX)
#define SET_NONEXECUTING(arg) (arg) = ((arg) & ~EXECIDX)
#define EXECUTING(arg) ((arg) & EXECIDX)
#define NONEXECUTING(arg) !((arg) & EXECIDX)


/* initial choicepoint */
#define InitialNode ChoiceCharOffset(Choice_Start,ArityToOffset(1))

/* initial value_trail size: leave room for an extra choicept */
#define InitialValueTrail (-(SIZEOF_FLEXIBLE_STRUCT(node_t, tagged_t, 0)/sizeof(tagged_t)))

#define EToY0		(SIZEOF_FLEXIBLE_STRUCT(frame_t, tagged_t, 0)/sizeof(tagged_t))
#define Yb(I)		(*CharOffset(E,I)) /* I as bytecode operand */
#define Y(I)		(E->term[I]) /* I as zero-based */
#define Yop(X)	(((X)+EToY0)*sizeof(tagged_t))
#define Yinv(X)	(((X)/sizeof(tagged_t))-EToY0)

#define FrameSize(L) BCOp0((L), FTYPE_ctype(f_e), -FTYPE_size(f_e))
#define FrameSizeToCount(O)	((O)/sizeof(tagged_t)-EToY0)

/* Private areas for a thread, related to the overall wam status */

#if defined(TABLING)
#define StackFReg                       (w->misc->stack_freg)
#define HeapFReg                        (w->misc->heap_freg)
#define FirstNodeTR(Node)               ((node_tr_t*)(Node)->local_top)
#define SetFirstNodeTR(Node,Value)      ((Node)->local_top = (frame_t*)(Value))
#define LastNodeTR                      (w->misc->last_node_tr)
#define FrozenChpt(Node)   ((Node)->global_top == (tagged_t*)&(HeapFReg))
#define NODE_TR_SIZE(NodeTR)            (((node_tr_t*)(NodeTR))->size)
#define NODE_TR_TRAIL_SG(NodeTR)        (((node_tr_t*)(NodeTR))->trail_sg)
#define NODE_TR_NEXT(NodeTR)            (((node_tr_t*)(NodeTR))->next)
#define NODE_TR_CHAIN(NodeTR)           (((node_tr_t*)(NodeTR))->chain)
#endif

#if defined(ANDPARALLEL)
#define Goal_List_Start                   (w->misc->goal_list_start)
#define Goal_List_Top                     (w->misc->goal_list_top)
#define Goal_Cache                        (w->misc->goal_cache)
#define Dep_Size                          (w->misc->dep_size_exec)
#define Dep_Id                            (w->misc->dep_id_exec)
#define Goal_List_Lock                    (w->misc->goal_list_l)
#define Event_Queue_Start                 (w->misc->event_queue_start)
#define Event_Queue_Top                   (w->misc->event_queue_top)
#define Event_Queue_Lock                  (w->misc->event_queue_l)
#define Last_Parallel_Exec                (w->misc->last_parallel_exec)
#define Cancel_Goal_Exec                  (w->misc->cancel_goal_exec)
#define Safe_To_Cancel                    (w->misc->safe_to_cancel)
#define Mutex_Lock                        (w->misc->mutex_l)
#define Mode                              (w->misc->mode)
#define Waiting_For_Work_Cond_Var         (w->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock             (w->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work        (w->misc->suspended_waiting_for_work)
#define Suspend                           (w->misc->suspend)
#if defined(Solaris)
#define Total_Suspending_Time             (w->misc->total_suspending_time)
#define Suspending_Time_Cont              (w->misc->suspending_time_cont)
#endif
#define Next_Wam                          (w->misc->next_wam)

#define Goal_List_Start_Of(x)             (x->misc->goal_list_start)
#define Goal_List_Top_Of(x)               (x->misc->goal_list_top)
#define Goal_Cache_Of(x)                  (x->misc->goal_cache)
#define Dep_Size_Of(x)                    (x->misc->dep_size_exec)
#define Dep_Id_Of(x)                      (x->misc->dep_id_exec)
#define Goal_List_Lock_Of(x)              (x->misc->goal_list_l)
#define Event_Queue_Start_Of(x)           (x->misc->event_queue_start)
#define Event_Queue_Top_Of(x)             (x->misc->event_queue_top)
#define Event_Queue_Lock_Of(x)            (x->misc->event_queue_l)
#define Last_Parallel_Exec_Of(x)          (x->misc->last_parallel_exec)
#define Cancel_Goal_Exec_Of(x)            (x->misc->cancel_goal_exec)
#define Safe_To_Cancel_Of(x)              (x->misc->safe_to_cancel)
#define Mutex_Lock_Of(x)                  (x->misc->mutex_l)
#define Mode_Of(x)                        (x->misc->mode)
#define Waiting_For_Work_Cond_Var_Of(x)   (x->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock_Of(x)       (x->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work_Of(x)  (x->misc->suspended_waiting_for_work)
#define Suspend_Of(x)                     (x->misc->suspend)
#if defined(Solaris)
#define Total_Suspending_Time_Of(x)       (x->misc->total_suspending_time)
#define Suspending_Time_Cont_Of(x)        (x->misc->suspending_time_cont)
#endif
#define Next_Wam_Of(x)                    (x->misc->next_wam)

/* Value types for controlling agents execution when expanding stack
   sets */
#define RELEASED                          0
#define TOSUSPEND                         1
#define SUSPENDED                         2
#define WAITING                           3

/* Accion types for releasing execution of deterministic parallel goals */
#define FORWARD_EXEC                      0
#define UNWIND                            1

#if defined(VISANDOR)
#define Count_Calls(x)                    (x->misc->my_count_calls)
#define Time_Each_Call(x)                 (x->misc->my_time_each_call)
#define Pcall_Level(x)                    (x->misc->pcall_level)
#define TmpEvPtr(x)                       (x->misc->tmpevptr)
#define NextEvent(x)                      (x->misc->nextevent)
#define FirstEvent(x)                     (x->misc->firstevent)
#define LastEvent(x)                      (x->misc->lastevent)

/* Event types for tracing tool VisAndOr */
#define FORK         1
#define START_GOAL   2
#define FINISH_GOAL  3
#define JOIN         4

#define START_TIME   5
#define STOP_TIME    6

#define AGENT_BUSY   7  /* working */
#define AGENT_IDLE   8  /* waiting for work */

// Never used by VisAndOr!
// #define CREATE_WAM   9
// #define CREATE_AGENT 10
#endif

#endif

#if defined(PARBACK)
#define Act_PF                            (w->misc->pf)
#define Goal_List_Start                   (w->misc->goal_list_start)
#define Goal_List_Top                     (w->misc->goal_list_top)
#define Cancel_Goal_List_Start            (w->misc->cancel_goal_list_start)
#define Cancel_Goal_List_Top              (w->misc->cancel_goal_list_top)
#define Back_Goal_List_Start              (w->misc->back_goal_list_start)
#define Back_Goal_List_Top                (w->misc->back_goal_list_top)
#define Goal_To_Cancel                    (w->misc->goal_to_cancel)
#define Safe_To_Cancel                    (w->misc->safe_to_cancel)
#define Mutex_Lock                        (w->misc->mutex_l)
#define Waiting_For_Work_Cond_Var         (w->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock             (w->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work        (w->misc->suspended_waiting_for_work)
#define Suspend                           (w->misc->suspend)
#define Next_Wam                          (w->misc->next_wam)

#define Goal_List_Start_Of(x)             (x->misc->goal_list_start)
#define Goal_List_Top_Of(x)               (x->misc->goal_list_top)
#define Cancel_Goal_Exec_Handler_Of(x)    (x->misc->cancel_goal_exec)
#define Safe_To_Cancel_Of(x)              (x->misc->safe_to_cancel)
#define Mutex_Lock_Of(x)                  (x->misc->mutex_l)
#define Waiting_For_Work_Cond_Var_Of(x)   (x->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock_Of(x)       (x->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work_Of(x)  (x->misc->suspended_waiting_for_work)
#define Suspend_Of(x)                     (x->misc->suspend)
#define Next_Wam_Of(x)                    (x->misc->next_wam)

/* Value types for controlling agents execution when expanding stack
   sets */
#define RELEASED                          0
#define TOSUSPEND                         1
#define SUSPENDED                         2
#define WAITING                           3

#endif

#define Heap_Start          w->heap_start
#define Heap_End            w->heap_end
#define Heap_Warn_Soft      w->heap_warn_soft
#define Heap_Warn           w->heap_warn
#define Int_Heap_Warn       w->int_heap_warn
#define Stack_Start         w->stack_start
#define Stack_End           w->stack_end
#define Stack_Warn          w->stack_warn
#define Choice_End          w->choice_end
#define Choice_Start        w->choice_start


#define USE_TAGGED_CHOICE_START

#if defined(USE_TAGGED_CHOICE_START)
#define Tagged_Choice_Start w->tagged_choice_start
#endif

#define Trail_Start         w->trail_start
#define Trail_End           w->trail_end


/* These are related to the I/O pointers */

#define Input_Stream_Ptr    w->streams->input_stream_ptr
#define Output_Stream_Ptr   w->streams->output_stream_ptr
#define Error_Stream_Ptr    w->streams->error_stream_ptr


/* These access the private stack for bignum operations */

#define Numstack_Top        w->numstack_top
#define Numstack_End        w->numstack_end
#define Numstack_First      w->numstack_first
#define Numstack_Last       w->numstack_last


/* These keep the current state of the debugger. */

#define Current_Debugger_State w->debugger_info->current_debugger_state
#define Current_Debugger_Mode  w->debugger_info->current_debugger_mode


/* Local space to generate atoms and other general string operations */

#define Atom_Buffer          w->atom_buffer
#define Atom_Buffer_Length   w->atom_buffer_length


/* Incore compiling */

#define Last_Insn            (w->last_insn)

/* Expanded worker (not always active) */

#define Expanded_Worker (w->misc->expanded_worker)

#define Next_Worker(w)  (w->misc->next_worker)

#define Stop_This_Goal(w) (w->misc->stop_this_goal)

/* Global variables */

#define GLOBVAR(i) w->misc->globalvar[i]

/* The local (per-thread) definitions for garbage collection */

#define Gc_Total_Grey    (w->misc->gc_total_grey)
#define Gcgrey           (w->misc->gcgrey)
#define Total_Found      (w->misc->total_found)
#define Cvas_Found       (w->misc->cvas_found)
#define Gc_Aux_Node      (w->misc->gc_aux_node)
#define Gc_Choice_Start  (w->misc->gc_choice_start)
#define Gc_Trail_Start   (w->misc->gc_trail_start)
#define Gc_Heap_Start    (w->misc->gc_heap_start)
#define Gc_Stack_Start   (w->misc->gc_stack_start)


/* Topmost choicepoint for calls to concurrent facts. */

#define TopConcChpt     w->misc->top_conc_chpt

/* Global registers */

#define GlobReg(Which) w->misc->globalreg[Which]

/* Throwing exceptions */

#define ErrArgNo w->misc->errargno
#define ErrCode w->misc->errcode
#define ErrFuncName w->misc->errfuncname
#define ErrFuncArity w->misc->errfuncarity
#define Culprit w->misc->culprit

#if defined(TABLING)
#define NodeLocalTop(Node)					\
  (((Node)->global_top != (tagged_t*)(&(HeapFReg))) ?		\
   (Node)->local_top : StackFReg)

#define NodeGlobalTop(Node)					\
  (((Node)->global_top != (tagged_t*)(&(HeapFReg))) ?		\
   (Node)->global_top : HeapFReg)

#else
#define NodeLocalTop(Node)  (Node)->local_top

#define NodeGlobalTop(Node) (Node)->global_top
#endif

/* If you edit this defn you must update ComputeE as well. */
#define ComputeA(A,B) \
{ \
  if (w->local_top) \
    (A) = w->local_top; \
  else if (!StackYounger((A) = NodeLocalTop(B),w->frame)) \
    (A) = StackCharOffset(w->frame,FrameSize(w->next_insn)); \
}

#define NewShadowregs(Gtop) \
{ \
  w->global_uncond = TagHVA(Gtop); \
  w->local_uncond = TagSVA(w->local_top); \
}

#define SetShadowregs(Chpt) \
{ \
  w->global_uncond = TagHVA(NodeGlobalTop(Chpt));     \
  w->local_uncond = TagSVA(NodeLocalTop(Chpt));      \
}

#define SaveGtop(Chpt,Gtop) \
  (Chpt)->global_top = (Gtop);

#define RestoreGtop(Chpt) \
{ \
  w->global_top = NodeGlobalTop(Chpt); \
}

#define SaveLtop(Chpt) \
  (Chpt)->local_top = w->local_top;

#define RestoreLtop(Chpt) \
  w->local_top = NodeLocalTop(Chpt);

#if defined(ANDPARALLEL) && defined(VISANDOR)
/* Event output macros for tracing tool VisAndOr */

#if defined(SYMM)
#define USCLK_EXISTS
#undef  NSCLK_EXISTS
#define TIME_INIT  usclk_init()
#define TIME       ((float)getusclk() - time_at_event_start*1e6)
#endif

/* #if defined(Solaris) */
#if defined(DARWIN) || defined(LINUX) || defined(Solaris) || defined(Win32) || defined(BSD)
#undef  USCLK_EXISTS
#define NSCLK_EXISTS
#define TIME_INIT   
#define TIME   ((gethrtime()/1000) - (time_at_event_start*1e6))
#endif

#define EVENT(Event, PPF, NFork) \
 { \
   if ( gen_event_file ) { \
     if ( (TmpEvPtr(w) = (NextEvent(w))++) == LastEvent(w) ) { \
          fprintf(stderr, "Agent 0x%p event table overflow.\n", w); \
          gen_event_file = FALSE; \
     } \
     else { \
          TmpEvPtr(w)->timestamp = (uintmach_t)TIME; \
          TmpEvPtr(w)->evtype = (Event); \
          TmpEvPtr(w)->ppf = (PPF); \
          TmpEvPtr(w)->nfork = (NFork); \
          TmpEvPtr(w)->wam = w; \
     } \
   } \
 }
#endif

/* EVENTS    ------------------------------------- */

#define SetEvent	(TestEvent ? 0 : (Heap_Warn_Soft = Heap_Start))
#define TestEvent	(Heap_Warn!=Heap_Warn_Soft)

/* MARGINS   ------------------------------------- */

/* Update heap margins (which depends on dynamic CALLPAD) */
#if defined(USE_DYNAMIC_ATOM_SIZE)
#define UpdateHeapMargins() { \
    int wake_count = WakeCount; \
    Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn \
                     ? HeapOffset(Heap_End,-SOFT_HEAPPAD) \
                     : Heap_Start); \
    Heap_Warn = HeapOffset(Heap_End,-SOFT_HEAPPAD); \
    if (wake_count>=0) \
      Heap_Warn_Soft = HeapCharOffset(Heap_Start,-wake_count); \
    else \
      Heap_Warn_Soft = Int_Heap_Warn; \
}
#else
#define UpdateHeapMargins() {}
#endif

#endif /* _CIAO_REGISTERS_H */
