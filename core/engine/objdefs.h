/*
 *  objdefs.h
 *
 *  Definitions for symbol tables, databases, predicates.
 *
 *  Copyright (C) 1988,1989 Swedish Institute of Computer Science and
 *    The Aerospace Corporation.
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_OBJDEFS_H
#define _CIAO_OBJDEFS_H

/* Object identifiers and emulator "pseudo instructions" - never seen
   by main dispatchers. */
typedef short enter_instr_t;

/* These six must be first, in this order. */
#define ENTER_COMPACTCODE 0
#define ENTER_COMPACTCODE_INDEXED 1
#define ENTER_PROFILEDCODE 2
#define ENTER_PROFILEDCODE_INDEXED 3
#define ENTER_FASTCODE 4
#define ENTER_FASTCODE_INDEXED 5
/* */
#define ENTER_UNDEFINED 6
#define ENTER_C 7
#define ENTER_INTERPRETED 8
#define BUILTIN_ABORT 9
#define BUILTIN_APPLY 10
#define BUILTIN_CALL 11
#define BUILTIN_SYSCALL 12
#define BUILTIN_NODEBUGCALL 13
#define BUILTIN_TRUE 14
#define BUILTIN_FAIL 15
#define BUILTIN_CURRENT_INSTANCE 16
#define BUILTIN_RESTORE 17
#define BUILTIN_COMPILE_TERM 18
#define BUILTIN_GELER 19
#define BUILTIN_INSTANCE 20
#define BUILTIN_DIF 21
#define SPYPOINT 22
#define WAITPOINT 23
#define BREAKPOINT 24
/* Other object identifiers -- these are used to inform the gc */
#define TABLE 25
#define EMUL_INFO 26
#define OTHER_STUFF 27

typedef struct instance_handle_ instance_handle_t;
typedef struct instance_ instance_t;
typedef struct int_info_ int_info_t;
typedef struct sw_on_key_ sw_on_key_t;

// uncomment to enable atom GC (incomplete)
// #define ATOMGC 1

/* OBJECT AREA ----------------------------------------------------*/ 

#define CACHE_INCREMENTAL_CLAUSE_INSERTION

typedef unsigned short int instance_clock_t;

#define ACTIVE_INSTANCE(ARG,I,TIME,CHAINP) \
  ((I)==NULL ? NULL : \
   ((TIME) >= (I)->birth && (TIME) < (I)->death) ? (I) : \
   active_instance(ARG,I,TIME,CHAINP))


 /* This one does not look at time creation of clauses. */

#define ACTIVE_INSTANCE_conc(ARG,I,ROOT)  \
     active_instance_conc(ARG,I,ROOT) 

CFUN__PROTO(active_instance, instance_t *, instance_t *i,int time,bool_t normal);

/* p->count = (intmach_t *)((char *)p + objsize) - p->counters */

#define NumberOfCounters(cl) \
  ((intmach_t *)((char *)cl + cl->objsize) - cl->counters)

/* Clauses of compiled predicates are stored as a linked list of
   records. The forward link "next" of the last clause contains the
   total number of clauses. */

typedef struct emul_info_ emul_info_t;

typedef union clause_link_ clause_link_t;
union clause_link_ {
  /* Clause pointer or number (used in emul_info to store links to
     the next clause or the total number of clauses) */
  uintmach_t number; /* clause number */
  emul_info_t *ptr;    /* clause pointer */   
};

struct emul_info_ {
  clause_link_t next;          /* next clause OR no. of clauses */
  definition_t *subdefs;
  intmach_t objsize;	                         /* total # chars */
#if defined(GAUGE)
  intmach_t *counters;	    /* Pointer to clause's first counter. */
#endif
  char emulcode[FLEXIBLE_SIZE];
};

/* TODO: (JFMC) give a better name to IS_CLAUSE_TAIL */

/* CLAUSE_TAIL */
#define CLAUSE_TAIL_INSNS_SIZE FTYPE_size(f_o) /* TODO: (JFMC) why? */
#define IS_CLAUSE_TAIL(EP) (EP->objsize == 0)
#define ALLOC_CLAUSE_TAIL(EP) {						\
    EP = checkalloc_FLEXIBLE(emul_info_t, char, CLAUSE_TAIL_INSNS_SIZE); \
    EP->subdefs = NULL;							\
    EP->objsize = 0;							\
  }

/* All invocations looking at an instance of an concurrent predicate will
   actually have a pointer to a pointer to the instance.  Every clause has a
   pointer to a queue of handles to itself.  Erasing a pointed to instance 
   will change the pointer to the instance itself (Confused? OK, ask me, I
   have a nice drawing on a napkin --- MCL) */

struct instance_handle_ {
  instance_t *inst_ptr;                    /* Pointer to the instance */
  tagged_t head;                            /* Goal called; allows indexing. */
  instance_handle_t *next_handle;             
  instance_handle_t *previous_handle;             
};

/* Terms recorded under a key or clauses of an interpreted predicate
   are stored as a doubly linked list of records.  The forward link is
   terminated by NULL; the backward link wraps around.  Each instance
   points back to the beginning of the list.  The rank field defines a
   total order on a list.  Two timestamps are associated with each
   instance to support proper semantics for dynamic code
   updates. (MCL, with help from the manual).  There are also two
   pointers (one for unindexed and other for indexed accesses) to
   queues which maintain the list of calls looking at each
   instance. */

struct instance_ {
  instance_t *forward;
  instance_t *backward;
  int_info_t *root;
  instance_t *next_forward;
  instance_t *next_backward;
  tagged_t key;
  tagged_t rank;
  instance_clock_t birth, death;                          /* Dynamic clause lifespan */

  instance_handle_t *pending_x2;       /* Seen by invocations looking @ here */
  instance_handle_t *pending_x5;       /* Seen by invocations looking @ here */

  int objsize;		                                 /* total # chars */
  char emulcode[FLEXIBLE_SIZE];
};

/* Indexing information for dynamic predicates? First simple, common cases,
   then hash table indexing.  Includes information on openness/closeness of
   concurrent facts. (MCL) */

typedef enum {DYNAMIC, CONC_OPEN, CONC_CLOSED} int_behavior_t;

struct int_info_ {
  volatile int_behavior_t  behavior_on_failure;/* behavior if no clauses match. */

#if defined(CONDITIONAL_VARS)
  condition_t clause_insertion_cond;
#else
  /*  SLOCK clause_insertion_cond;            */
  condition_t clause_insertion_cond;
#endif

  instance_handle_t *x2_pending_on_instance;     /* Used when pred. is empty */
  instance_handle_t *x5_pending_on_instance;

  instance_t  *first  ;
  instance_t  *varcase;
  instance_t  *lstcase;
  sw_on_key_t *indexer;
};

typedef struct und_info_ und_info_t;
struct und_info_ {
  int unused;
};

/* TODO: refine (use OptimComp defs) */
typedef bool_t   (*CInfo)();
typedef tagged_t (*TInfo)();

extern stream_node_t *root_stream_ptr;

 /* Information about atoms */

typedef struct atom_ atom_t;
struct atom_ {        
  unsigned int has_squote:1;
  unsigned int has_dquote:1;
  unsigned int has_special:1;
  unsigned int index:29;
                               /* support for locking on atom names. MCL. */
#if defined(THREADS)                    /* Do not waste space otherwise */
  LOCK atom_lock_l;                      /* May be held for a long time */
#if defined(GENERAL_LOCKS)
  volatile intmach_t atom_lock_counter;         /* For general semaphores */
  SLOCK counter_lock;                            /* Held for a short time */
#endif
#endif
  uintmach_t atom_len;
  char name[FLEXIBLE_SIZE];
};

/* For a given goal and an emulated predicate, alternatives that might match
   the goal are stored as a linked list of records */

/* try-retry-trust repr. as a linked list. */

struct try_node_ {
  try_node_t *next;                      /* Next alternative or NULL */
  bcp_t emul_p;                    /* write mode or not first alternative */
  bcp_t emul_p2;		          /* read mode, first alternative */
  short node_offset;		       /* offset from choicepoint to next */
  /*short number;*/
  uintmach_t number;		    /* clause number for this alternative */
                             /* Gauge specific fields MUST come after this*/
#if defined(GAUGE)
  intmach_t *entry_counter;        /* Offset of counter for clause entry */
#endif
  };



#define ArityToOffset(A)  \
  (((A)+(SIZEOF_FLEXIBLE_STRUCT(node_t, tagged_t, 0)/sizeof(tagged_t))) * sizeof(tagged_t))
#define OffsetToArity(O)						\
  (((O)/sizeof(tagged_t))-(SIZEOF_FLEXIBLE_STRUCT(node_t, tagged_t, 0)/sizeof(tagged_t)))

#define SwitchSize(X) (((X)->mask / sizeof(sw_on_key_node_t))+1) 
#define SizeToMask(X) (((X)-1) * sizeof(sw_on_key_node_t))

typedef struct sw_on_key_node_ sw_on_key_node_t;
struct sw_on_key_node_ {
  /* NOTE: Make sure that the size of this structure is a power of 2
     (for efficiency) */
  tagged_t key;
  union {
    try_node_t *try_chain;         /* try-retry-trust as linked list */
    instance_t *instp;             /* int. clauses or recorded terms */
    sw_on_key_node_t *node;
    definition_t *def;                       /* predicate definition */
    module_t *mod;                              /* module definition */
    int_info_t *irootp;                             /* indexer info? */
    atom_t *atomp;               /* Info on atoms and main functors  */
    TInfo tinfo;                          /* CFUN C function pointer */
    CInfo cinfo;                         /* CBOOL C function pointer */
  } value;
};

/* Indexing table are used in indexing on first argument in calls and in
   looking up predicates.  They are operated as hash tables with quadratic
   overflow handling.  Hash table access is performed by using some of the
   low order bits of the key as array index, and then searching for a hit or
   for a zero key indicating that the key is absent from the table. 
   
   MCL: changed to make room for erased atoms: now a key == 1 indicates
   that the entry has been erased (but the search chain continues).  New 
   atoms are placed in the first free entry.

   NOTE (MCL): data structure different from that in Sicstus 1.8!!
*/

struct sw_on_key_ {
  uintmach_t mask;                                          /* bitmask */
  intmach_t count;
#if defined(ATOMGC)
  intmach_t next_index;
#endif
  sw_on_key_node_t node[FLEXIBLE_SIZE];
};

/* Node from a byte offset */
#define SW_ON_KEY_NODE_FROM_OFFSET(Tab, Offset) \
  ((sw_on_key_node_t *)((char *)&(Tab)->node[0] + (Offset)))

typedef struct incore_info_ incore_info_t;
struct incore_info_ {
  clause_link_t clauses;	                          /* first clause */
  /* Pointer to either "next" field of last clause or "clauses"  */
  clause_link_t *clauses_tail;
#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)
  emul_info_t *last_inserted_clause;        /* Pointer to the last clause */
  uintmach_t last_inserted_num;           /* Number of last ins. clause */
#endif
  try_node_t *varcase;
  try_node_t *lstcase;
  sw_on_key_t *othercase;
};

#define SetEnterInstr(F,I) \
{ \
  (F)->predtyp = (I); \
  (F)->enter_instr = \
    (F)->properties.spy ? SPYPOINT : \
      (F)->properties.wait ? WAITPOINT : \
	(F)->properties.breakp ? BREAKPOINT : \
	  (F)->predtyp; \
}

typedef union definfo_ definfo_t;
union definfo_ {
  int_info_t *intinfo;
  und_info_t *undinfo;
  incore_info_t *incoreinfo;
#if 0 /* was GAUGE */
  c_code_info_t *cinfo;
#else
  CInfo cinfo;
#endif
};

struct definition_ {
  enter_instr_t enter_instr;
  short arity; /*  */
  tagged_t printname;	                        /* or sibling pointer | 1 */
                                                /* or parent pointer | 3 */
  struct {
    unsigned int spy:1;
    unsigned int breakp:1;
    /* unsigned int public:1; */
                      /* concurrent obeys declaration and implies dynamic */
    unsigned int concurrent:1;      /* 1 if wait on it, 0 if closed (MCL) */
    unsigned int wait:1;                             /* obeys declaration */
    unsigned int dynamic:1;                                       /* -""- */
    unsigned int multifile:1;                                     /* -""- */
    unsigned int nonvar:1;             /* seen P(X,...) :- var(X), !, ... */
    unsigned int var:1;             /* seen P(X,...) :- nonvar(X), !, ... */
  } properties;
  enter_instr_t predtyp;
  definfo_t code;
};

#define DEF_SIBLING(F) \
  ((F)->printname&2 ? NULL : (definition_t *)TagToPointer((F)->printname))

struct module_ {
  tagged_t printname;
  struct {
    unsigned int is_static:1;
  } properties;
};

/* Classified somewhere else */
extern sw_on_key_node_t **atmtab;
extern sw_on_key_t *ciao_atoms;
extern CInfo builtintab[];

typedef struct statistics_ statistics_t;
struct statistics_ {
  inttime_t ss_tick;		             /* time spent stack_shifting */
  intmach_t ss_global;		                       /* # global shifts */
  intmach_t ss_local;		                       /* # local shifts  */
  intmach_t ss_control;	                        /* # control/trail shifts */
  inttime_t gc_tick;		                 /* Total GC ticks (sec) */
  intmach_t gc_count;		                 /* # garbage collections */
  intmach_t gc_acc;		            /* Total reclaimed heap space */

  inttime_t starttick;
  inttime_t lasttick;

  inttime_t startwalltick;
  inttime_t lastwalltick;
  inttime_t wallclockfreq;

  inttime_t startusertick;
  inttime_t lastusertick;
  inttime_t userclockfreq;

  inttime_t startsystemtick;
  inttime_t lastsystemtick;
  inttime_t systemclockfreq;
};

extern statistics_t ciao_statistics; /* Shared, I guess */

#if defined(GAUGE)

typedef struct c_code_info_ c_code_info_t;
struct c_code_info_ {
  CInfo procedure; 
  intmach_t counter;
};

#define INCR_COUNTER(c)   ((*(c))++)

#endif

typedef struct other_stuff_ other_stuff_t;
struct other_stuff_ {     /* Usable type for passing data areas to gc etc */
  char *pointer;
  intmach_t size;
};

#endif /* _CIAO_OBJDEFS_H */
