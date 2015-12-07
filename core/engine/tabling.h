/*
 *  tabling.h
 *
 *  Support for tabling.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_TABLING_H
#define _CIAO_TABLING_H

#if defined(TABLING)

#define GLOBAL_TABLE_SIZE        (100000*kCells)
#define TABLING_STK_SIZE         (10000*kCells)

#define CONSUMER    0         
#define GENERATOR   1

tagged_t functor_forward_trail;

double trail_time;

tagged_t *global_table;
tagged_t *tabling_stack;

tagged_t *global_table_free;
tagged_t *tabling_stack_free;
tagged_t *global_table_end;
tagged_t *tabling_stack_end;

#define TABLING_STK_TOP tabling_stack_free

#define INIT_GLOBAL_TABLE						\
  {									\
    global_table = (tagged_t*) malloc (GLOBAL_TABLE_SIZE * sizeof(tagged_t*)); \
    global_table_free = global_table;					\
    global_table_end = global_table + GLOBAL_TABLE_SIZE;		\
  }

#define INIT_TABLING_STACK						\
  {									\
    tabling_stack = (tagged_t*) malloc (TABLING_STK_SIZE * sizeof(tagged_t*)); \
    tabling_stack_free = tabling_stack;					\
    tabling_stack_end = tabling_stack + TABLING_STK_SIZE;		\
  }

#define ALLOC_GLOBAL_TABLE(PTR,PTR_TYPE,SIZE)				\
  {									\
    (PTR) = (PTR_TYPE)global_table_free;				\
    global_table_free += (SIZE) / sizeof(tagged_t*);			\
    if (global_table_free >= global_table_end)				\
      fprintf(stderr, "Global table memory exhausted\n");		\
  }

#define ALLOC_TABLING_STK(PTR,PTR_TYPE,SIZE)				\
  {									\
    (PTR) = (PTR_TYPE)tabling_stack_free;				\
    tabling_stack_free += (SIZE) / sizeof(tagged_t*);			\
    if (tabling_stack_free >= tabling_stack_end)			\
      fprintf(stderr, "Tabling stack exhausted\n");			\
  }

#define DEALLOC_GLOBAL_TABLE					\
  {								\
    global_table_free  = global_table;				\
  }

#define DEALLOC_TABLING_STK(PTR)				\
  {								\
    tabling_stack_free = (tagged_t *)(PTR);			\
  }

#define INIT_NODE_TR(NodeTR)						\
  {									\
    ALLOC_TABLING_STK((NodeTR),node_tr_t*,sizeof(node_tr_t));		\
    NODE_TR_SIZE(NodeTR) = 0;						\
    NODE_TR_TRAIL_SG(NodeTR) = NULL;					\
    NODE_TR_NEXT(NodeTR) = NULL;					\
    NODE_TR_CHAIN(NodeTR) = NULL;					\
  }

#define INIT_REG_NODE_TR(NodeTR)					\
  {									\
    ALLOC_TABLING_STK((NodeTR), tagged_t, sizeof(node_tr_t));		\
    NODE_TR_NEXT(NodeTR) = NULL;					\
    NODE_TR_CHAIN(NodeTR) = NULL;					\
    NODE_TR_SIZE(NodeTR) = (w->trail_top - TagToPointer(B->trail_top)) * 2; \
    ALLOC_TABLING_STK							\
      (NODE_TR_TRAIL_SG(NodeTR), tagged_t*,				\
       NODE_TR_SIZE(NodeTR) * sizeof(tagged_t));			\
  }

#define MAKE_TRAIL_CACTUS_STACK						\
  {									\
    if (FrozenChpt(B) && (FirstNodeTR(B) != NULL))			\
      {									\
	/* Create current node_trail */					\
	INIT_REG_NODE_TR(t0);						\
	/*printf("\nMAKING CACTUS STACK %p\n",t0);*/			\
									\
	if (NODE_TR_SIZE(t0))						\
	  {								\
	    tagged_t *pt3 = NODE_TR_TRAIL_SG(t0);		\
	    for (pt2 = w->trail_top-1,					\
		   t2=(tagged_t)TagToPointer(B->trail_top);		\
		 !TrailYounger(t2,pt2); pt2--)				\
	      {								\
		t3 = *pt2;						\
		if (IsVar(t3))						\
		  {							\
		    *pt3 = t3; pt3++;					\
		    *pt3 = *TagToPointer(t3); pt3++;			\
		  }							\
		else							\
		  {							\
		    if (*TagToPointer(t3) == functor_forward_trail)	\
		      { 						\
			*pt3 = t3; pt3++;				\
			*pt3 = t3; pt3++;				\
		      } 						\
		    else						\
		      NODE_TR_SIZE(t0) = NODE_TR_SIZE(t0) - 2;		\
		  }							\
	      }								\
	    								\
	    /*children trail segments point to the new node_trail*/	\
	    for (t1 = (tagged_t)NODE_TR_CHAIN(FirstNodeTR(B));		\
		 t1 != (tagged_t)NULL;					\
		 t1 = (tagged_t)NODE_TR_CHAIN(t1))			\
	      {								\
		NODE_TR_NEXT(t1) = (node_tr_t *)t0;			\
		/*printf("\n%p parent of %p\n",t0,t1);*/		\
	      }								\
									\
	    /*Link from first_node_tr*/					\
	    NODE_TR_CHAIN(FirstNodeTR(B)) = (node_tr_t *)t0;		\
	    /*printf("\nChain of %p = %p\n",FirstNodeTR(B),t0);*/	\
									\
	    /*Update last_node_tr*/					\
	    LastNodeTR = (node_tr_t *)t0;				\
	    /*printf("\nLastNodeTR %p\n",LastNodeTR);*/			\
	  }								\
	/*Mark current choice_pt*/					\
	SetFirstNodeTR(B,NULL);						\
      }									\
  }

////////////////////////////////////////////////////////////////////////////
/////////   GLOBAL TABLE  //////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

typedef struct trie_node_ trie_node_t;
typedef trie_node_t *TrNode;
struct trie_node_ {
  tagged_t entry;
  trie_node_t *parent;
  trie_node_t *child;
  trie_node_t *next;
};

struct sf 
{
  int isGen;
  int size;
  tagged_t *vars;
  int attr_size;
  tagged_t *attrs;
};

struct attrs 
{
  int size;
  tagged_t *attrs;
};

struct l_ans
{
  trie_node_t *node;
  void *space;
  struct l_ans *next;
};

struct cons_list
{
  int type;
  struct cons *cons;
  struct cons_list *next;
};

struct cons
{
  struct sf* sf;
  node_tr_t *node_tr;
  frame_t *frame;
  bcp_t next_insn;
  struct l_ans *last_ans;
  struct gen *ptcp;
  struct gen *gen;
  tagged_t ans_space;
  tagged_t attr_vars;
};

//TODO - only essential info, this is global memory!
struct gen 
{
  long id;                        //generator identifier
  struct gen *leader;             //for precise SCC management.
  tagged_t on_exec;               //free var if the generator is on execution.
  long state;                     //state of the call.
  struct gen *ptcp;               //to represent Global Dependence Tree (GDT).

  struct sf* sf;                  //substitution factor of the generator.

  struct cons_list* first_cons;   //list of consumers.
  struct cons_list* last_cons;    //last consumer.

  trie_node_t *trie_ans;          //to check for repetitions.

  struct l_ans *first_ans;       //list of answers.
  struct l_ans *last_ans;        //last answer.

  frame_t *local_top;             //original stack freg.
  tagged_t *global_top;            //original heap freg.
  frame_t *stack_freg;            //original stack freg.
  tagged_t *heap_freg;            //original heap freg.
  tagged_t *tabl_stk_top;         //original tabling_stack_top
  node_tr_t *last_node_tr;        //original value of LastNodeTR.
  node_tr_t *cons_node_tr;        //NodeTR if gen -> cons
  
  node_t *node;                   //generator choice point
  struct cons_list *cons;         //pointer to consumer (non-leader generator)

  //SWAPPING structures
  int extern_cons;                //Do i have to update orig_fregs?
  struct sf* sf_priv;             //private sf of the generator.
  node_t *answer_cp;              //choice point of last found answer
  tagged_t *answer_tr;            //trail of last found answer       
  node_tr_t *answer_node_tr;      //LastNodeTR when last answer was found.
  struct sf* swap_sf;             //copy of substitution factor for swapping execution.

  struct gen *prev;               //Double generator linked list - prev
  struct gen *post;               //Double generator linked list - post
};

#endif

//TRAIL CACTUS STACK IMPLEMENTATION
//=================================
//
//A - On consumer suspension, all the choices are marked as frozen 
//    (using global_top - HeapFReg) and the Last_Node_tr is saved 
//    (using local_top).
//
//B - On backtracking, from initial node_tr (local_top) until
//    Last_Node_tr point to the save father.
//
//C - On consumer resumption, its node_tr is used as Last_Node_tr.

#endif /* _CIAO_TABLING_H */
