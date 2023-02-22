/*
 *  tabling.h
 *
 *  Support for tabling.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_TABLING_H
#define _CIAO_TABLING_H

#if defined(TABLING)

#define GLOBAL_TABLE_SIZE        (300000*kCells)
#define TABLING_STK_SIZE         (30000*kCells)

#define CONSUMER    0         
#define GENERATOR   1

extern tagged_t functor_forward_trail;

extern double trail_time;

extern tagged_t *global_table;
extern tagged_t *tabling_stack;

extern tagged_t *global_table_free;
extern tagged_t *tabling_stack_free;
extern tagged_t *global_table_end;
extern tagged_t *tabling_stack_end;

#define TABLING_STK_TOP tabling_stack_free

#define INIT_GLOBAL_TABLE                                               \
  {                                                                     \
    global_table = (tagged_t*) checkalloc (GLOBAL_TABLE_SIZE * sizeof(tagged_t*)); \
    global_table_free = global_table;                                   \
    global_table_end = global_table + GLOBAL_TABLE_SIZE;                \
  }

#define INIT_TABLING_STACK                                              \
  {                                                                     \
    tabling_stack = (tagged_t*) checkalloc (TABLING_STK_SIZE * sizeof(tagged_t*)); \
    tabling_stack_free = tabling_stack;                                 \
    tabling_stack_end = tabling_stack + TABLING_STK_SIZE;               \
  }

#define ALLOC_GLOBAL_TABLE(PTR,PTR_TYPE,SIZE)                           \
  {                                                                     \
    (PTR) = (PTR_TYPE)global_table_free;                                \
    global_table_free += (SIZE) / sizeof(tagged_t*);                    \
    if (global_table_free >= global_table_end)                          \
      fprintf(stderr, "Global table memory exhausted\n");               \
  }

#define ALLOC_TABLING_STK(PTR,PTR_TYPE,SIZE)                            \
  {                                                                     \
    (PTR) = (PTR_TYPE)tabling_stack_free;                               \
    tabling_stack_free += (SIZE) / sizeof(tagged_t*);                   \
    if (tabling_stack_free >= tabling_stack_end)                        \
      fprintf(stderr, "Tabling stack exhausted\n");                     \
  }

#define DEALLOC_GLOBAL_TABLE                                    \
  {                                                             \
    global_table_free  = global_table;                          \
  }

#define DEALLOC_TABLING_STK(PTR)                                \
  {                                                             \
    tabling_stack_free = (tagged_t *)(PTR);                     \
  }

#define INIT_NODE_TR(NodeTR)                                            \
  {                                                                     \
    ALLOC_TABLING_STK((NodeTR),node_tr_t*,sizeof(node_tr_t));           \
    NODE_TR_SIZE(NodeTR) = 0;                                           \
    NODE_TR_TRAIL_SG(NodeTR) = NULL;                                    \
    NODE_TR_NEXT(NodeTR) = NULL;                                        \
    NODE_TR_CHAIN(NodeTR) = NULL;                                       \
  }

#define INIT_REG_NODE_TR(NodeTR)                                        \
  {                                                                     \
    ALLOC_TABLING_STK((NodeTR), tagged_t, sizeof(node_tr_t));           \
    NODE_TR_NEXT(NodeTR) = NULL;                                        \
    NODE_TR_CHAIN(NodeTR) = NULL;                                       \
    NODE_TR_SIZE(NodeTR) = (w->trail_top - TrailTopUnmark(B->trail_top)) * 2; \
    ALLOC_TABLING_STK                                                   \
      (NODE_TR_TRAIL_SG(NodeTR), tagged_t*,                             \
       NODE_TR_SIZE(NodeTR) * sizeof(tagged_t));                        \
  }

#define MAKE_TRAIL_CACTUS_STACK                                         \
  {                                                                     \
    if (FrozenChpt(B) && (FirstNodeTR(B) != NULL))                      \
      {                                                                 \
        tagged_t t0; \
        tagged_t t1; \
        /* Create current node_trail */                                 \
        INIT_REG_NODE_TR(t0);                                           \
        /*printf("\nMAKING CACTUS STACK %p\n",t0);*/                    \
                                                                        \
        if (NODE_TR_SIZE(t0))                                           \
          {                                                             \
            tagged_t *pt3 = NODE_TR_TRAIL_SG(t0);               \
            for (tagged_t *pt2 = w->trail_top-1,                                  \
                   t2=(tagged_t)TrailTopUnmark(B->trail_top);             \
                 !TrailYounger(t2,pt2); pt2--)                          \
              {                                                         \
                tagged_t t3 = *pt2;                                     \
                if (IsVar(t3))                                          \
                  {                                                     \
                    *pt3 = t3; pt3++;                                   \
                    *pt3 = *TaggedToPointer(t3); pt3++;                    \
                  }                                                     \
                else                                                    \
                  {                                                     \
                    if (*TaggedToPointer(t3) == functor_forward_trail)     \
                      {                                                 \
                        *pt3 = t3; pt3++;                               \
                        *pt3 = t3; pt3++;                               \
                      }                                                 \
                    else                                                \
                      NODE_TR_SIZE(t0) = NODE_TR_SIZE(t0) - 2;          \
                  }                                                     \
              }                                                         \
                                                                        \
            /*children trail segments point to the new node_trail*/     \
            for (t1 = (tagged_t)NODE_TR_CHAIN(FirstNodeTR(B));          \
                 t1 != (tagged_t)NULL;                                  \
                 t1 = (tagged_t)NODE_TR_CHAIN(t1))                      \
              {                                                         \
                NODE_TR_NEXT(t1) = (node_tr_t *)t0;                     \
                /*printf("\n%p parent of %p\n",t0,t1);*/                \
              }                                                         \
                                                                        \
            /*Link from first_node_tr*/                                 \
            NODE_TR_CHAIN(FirstNodeTR(B)) = (node_tr_t *)t0;            \
            /*printf("\nChain of %p = %p\n",FirstNodeTR(B),t0);*/       \
                                                                        \
            /*Update last_node_tr*/                                     \
            LastNodeTR = (node_tr_t *)t0;                               \
            /*printf("\nLastNodeTR %p\n",LastNodeTR);*/                 \
          }                                                             \
        /*Mark current choice_pt*/                                      \
        SetFirstNodeTR(B,NULL);                                         \
      }                                                                 \
  }



#endif

//TRAIL CACTUS STACK IMPLEMENTATION
//=================================
//
//A - On consumer suspension, all the choices are marked as frozen 
//    (using heap_top - HeapFReg) and the Last_Node_tr is saved 
//    (using local_top).
//
//B - On backtracking, from initial node_tr (local_top) until
//    Last_Node_tr point to the save father.
//
//C - On consumer resumption, its node_tr is used as Last_Node_tr.

#endif /* _CIAO_TABLING_H */
