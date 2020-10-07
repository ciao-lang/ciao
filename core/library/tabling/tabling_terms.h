#ifndef _CIAO_TABLING_TABLING_TERMS_H
#define _CIAO_TABLING_TABLING_TERMS_H

#if defined(TABLING)

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
  intmach_t isGen;
  intmach_t size;
  tagged_t *vars;
  intmach_t attr_size;
  tagged_t *attrs;
};

struct attrs 
{
  intmach_t size;
  tagged_t *attrs;
};

struct l_gen
{
  struct gen *node;
  TrNode space; 
  TrNode orig_space;
  // struct space *clone_space;
  TrNode orig_attrs;
  struct l_gen *next;
};

struct l_ans
{
  trie_node_t *node;
  TrNode space;
  TrNode ans_attrs;
  intmach_t valid;
  struct l_ans *next;
};

struct cons_list
{
  intmach_t type;
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
  intmach_t id;                        //generator identifier
  struct gen *leader;             //for precise SCC management.
  tagged_t on_exec;               //free var if the generator is on execution.
  intmach_t state;                     //state of the call.
  struct gen *ptcp;               //to represent Global Dependence Tree (GDT).
  tagged_t realcall;          //temporal DELETE


  struct sf* sf;                  //substitution factor of the generator.

  struct cons_list* first_cons;   //list of consumers.
  struct cons_list* last_cons;    //last consumer.

  trie_node_t *trie_ans;          //to check for repetitions.

  struct l_ans *first_ans;       //list of answers.
  struct l_ans *last_ans;        //last answer.

  frame_t *local_top;             //original stack freg.
  tagged_t *heap_top;             //original heap freg.
  frame_t *stack_freg;            //original stack freg.
  tagged_t *heap_freg;            //original heap freg.
  tagged_t *tabl_stk_top;         //original tabling_stack_top
  node_tr_t *last_node_tr;        //original value of LastNodeTR.
  node_tr_t *cons_node_tr;        //NodeTR if gen -> cons
  
  choice_t *choice;               //generator choice point
  struct cons_list *cons;         //pointer to consumer (non-leader generator)

  //SWAPPING structures
  intmach_t extern_cons;          //Do i have to update orig_fregs?
  struct sf* sf_priv;             //private sf of the generator.
  choice_t *answer_cp;            //choice point of last found answer
  tagged_t *answer_tr;            //trail of last found answer       
  node_tr_t *answer_node_tr;      //LastNodeTR when last answer was found.
  struct sf* swap_sf;             //copy of substitution factor for swapping execution.

  struct gen *prev;               //Double generator linked list - prev
  struct gen *post;               //Double generator linked list - post
};




#endif

#endif /* _CIAO_TABLING_TABLING_TERMS_H */
