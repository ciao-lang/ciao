/*
 *  own_malloc.c
 *
 *  Own memory manager.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 *
 *  Authors:
 *    Manuel Carro
 *    Jose F. Morales (minor changes)
 */

#include <ciao/eng.h>

#if !defined(OPTIM_COMP)
#define OWNMALLOC_ALIGN sizeof(tagged_t) /* blocks suitably aligned for any use */ 
#endif

#include <stdlib.h>
#include <string.h>

#if !defined(Win32)
# include <strings.h>
#endif

#if defined(USE_OWN_MALLOC)
#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
#include <ciao/own_mmap.h>
#endif
#endif

/* TODO: share more definitions of both LINEAR and BINARY versions */

#if defined(USE_OWN_MALLOC)

/* First-fit linear search-based allocator (simpler but not optimal) */
//#define USE_OWN_MALLOC_LINEAR 1
/* Unbalanced binary tree search-based allocator */
#define USE_OWN_MALLOC_BINARY 1
#define OWN_MALLOC_UPPER 1 /* TODO: it was enabled with LINEAR, why? */

#define ALIGN OWNMALLOC_ALIGN
#define ALIGN_TO(A,X) ((((X)-1) & -(A))+(A))
#define ALIGN_SIZE(Chars) ALIGN_TO(OWNMALLOC_ALIGN, (Chars))

#define ADJUST_BLOCK(SIZE) ((SIZE) < ALIGN_SIZE(OWNMALLOC_BLOCKSIZE) ? ALIGN_SIZE(OWNMALLOC_BLOCKSIZE) : (SIZE))

#define HEADER_SIZE ALIGN_SIZE(sizeof(mem_block_t))
#define MEM_BLOCK_SIZE(Units) (HEADER_SIZE+(Units))

#define MEM_BLOCK_PTR(BLOCK) ((char *)(BLOCK) + HEADER_SIZE)
#define PTR_TO_MEM_BLOCK(PTR) ((mem_block_t *)((char *)(PTR) - HEADER_SIZE))

//#define USE_RTBLOCKCHECK 1
/* TODO: make block checking optional, improve and make it work for
   both linear and binary versions of alloc */
#if defined(USE_RTBLOCKCHECK)
void rtblockcheck(char *f, intmach_t l);
#define RTBLOCKCHECK rtblockcheck(__FUNCTION__, __LINE__)
#else
#define RTBLOCKCHECK
#endif

#endif /* USE_OWN_MALLOC */

/* --------------------------------------------------------------------------- */

#if defined(USE_OWN_MALLOC)
/* List of free and assigned blocks. Kept in memory order, to allow
   recompaction upon block freeing.  Use forward and backwards link
   for this.  Compaction is not always possible: blocks of memory can
   come from sparse addresses. */

typedef struct mem_block mem_block_t;
#if defined(USE_OWN_MALLOC_BINARY)
typedef struct block_tree block_tree_t;
#endif

struct mem_block {
  bool_t block_is_free; /* Might be only one bit */
  mem_block_t *fwd, *bck;
  mem_block_t *next_free, *prev_free;
#if defined(USE_OWN_MALLOC_BINARY)
  block_tree_t *which_node;
#endif
  intmach_t size; /* measured in bytes (aligned to ALIGN) */
};

#if defined(USE_OWN_MALLOC_BINARY)
struct block_tree {
  intmach_t size_this_node;
  mem_block_t *block_list;
  block_tree_t *left, *right, *parent;
  intmach_t max_left, max;
};
#endif

static void insert_block(mem_block_t *block, 
                         mem_block_t *previous,
                         mem_block_t *next);
static void insert_free_block(mem_block_t *block);
static void remove_free_block(mem_block_t *block);

static mem_block_t *block_list = NULL;  /* Shared, locked */

#endif /* USE_OWN_MALLOC */

/* --------------------------------------------------------------------------- */

#if defined(USE_OWN_MALLOC_LINEAR)

static mem_block_t *free_block_list = NULL;

static mem_block_t *locate_block(intmach_t size);

#if defined(USE_RTBLOCKCHECK)
void rtblockcheck(char *f, intmach_t l) {
  mem_block_t *running;
  for (running = free_block_list; running != NULL; running = running->next_free) {
    if (running->block_is_free != 0 && running->block_is_free != 1) {
      TRACE_PRINTF("{%s:%" PRIdm ": inconsistent free block at %p}\n", f, l, running);
    }
  }
}
#endif

/* Search for a block in the free_block_list with enough memory.
   Requested size is aligned to ALIGN.
   If no block found, return NULL. */

#define LOCATE_BLOCK(BLOCK, SIZE) (BLOCK) = locate_block((SIZE))

static mem_block_t *locate_block(intmach_t requested_size) {
  mem_block_t *running;

  DEBUG__TRACE(debug_mem,
               "locate_block was requested a block of %" PRIdm " bytes\n",
               requested_size);

  RTCHECK({
    if (requested_size < ALIGN)
      TRACE_PRINTF("Uh? locate_block was requested a block of %" PRIdm " bytes!\n",
                   requested_size);
  });

  RTBLOCKCHECK;

  running = free_block_list;
  while(running && running->size < requested_size) {
    running = running->next_free;
  }

#if defined(DEBUG_TRACE)
  if (!running) {
    DEBUG__TRACE(debug_mem,
                 "locate_block did not find a block of %" PRIdm " bytes\n",
                 requested_tagged_w);
  }
#endif
  
  return running;
}

/* Link a block into the list.  previous == NULL if first block, next ==
   NULL if last block . */

static void insert_block(mem_block_t *block,
                         mem_block_t *previous,
                         mem_block_t *next) {
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL) {
    previous->fwd = block;
  } else {
    /* Then block is the first in the list */
    block_list = block;
  }
  if (next != NULL) next->bck = block;
}


/* Link a block into the free blocks list. */

static void insert_free_block(mem_block_t *block) {
  block->prev_free = NULL;
  block->next_free = free_block_list;
  if (free_block_list)
    free_block_list->prev_free = block;
  free_block_list = block;    
  DEBUG__TRACE(debug_mem, "*Put back block of %" PRIdm " bytes\n", block->size);
}

/* Remove a block from the free blocks list. */

static void remove_free_block(mem_block_t *block) {
  mem_block_t *previous = block->prev_free;
  mem_block_t *next = block->next_free;

  if (next)
    next->prev_free = previous;
  if (previous)
    previous->next_free = next;
  else 
    free_block_list = next;
}

#endif /* USE_OWN_MALLOC_LINEAR */

/* --------------------------------------------------------------------------- */

#if defined(USE_OWN_MALLOC_BINARY)

/* All of them locked */

static block_tree_t *free_blocks_tree = NULL;
static block_tree_t *disposed_tree_nodes = NULL;

static block_tree_t *search_free_block(intmach_t units);
static block_tree_t *create_new_node(mem_block_t *block, block_tree_t *parent);
static void remove_block_from_node(block_tree_t *node, mem_block_t  *block);
static void add_block_to_node(block_tree_t *node, mem_block_t  *block);
#if defined(USE_LOWRTCHECKS)
static void test_tree(void);
static void test(block_tree_t *tree);
#endif

/* Search for a block with memory enough.
   Requested is aligned to ALIGN.
   If no block found, return NULL. */

#define LOCATE_BLOCK(BLOCK, SIZE) { \
  block_tree_t *node; \
  node = search_free_block((SIZE)); \
  if (node == NULL) { \
    (BLOCK) = NULL; \
  } else { \
    (BLOCK) = node->block_list; \
  } \
}

static block_tree_t *search_free_block(intmach_t requested_size) {
  block_tree_t *running;

  DEBUG__TRACE(debug_mem,
               "search_free_block was requested a block of %" PRIdm " bytes\n",
               requested_size);

  RTCHECK({
    if (requested_size < ALIGN)
      TRACE_PRINTF("Uh? search_free_block was requested a block of %" PRIdm " bytes!\n",
                   requested_size);
  });

  running = free_blocks_tree;
  while(running) {
    if (running->left && requested_size <= running->max_left)
      running = running->left;
    else if (requested_size <= running->size_this_node)
      break;
    else if (running->right && requested_size <= running->max)
      running = running->right;
    else
      running = NULL;
  }

#if defined(DEBUG_TRACE)
  if (!running) {
    DEBUG__TRACE(debug_mem,
                 "search_free_block did not find a block of %" PRIdm " bytes\n",
                 requested_size);
  }
#endif
  RTCHECK({
    if (!running) test_tree();
  });
  
  return running;
}

/* Link a block into the free blocks pool. */

static void insert_free_block(mem_block_t *block) {
  intmach_t size = block->size;
  block_tree_t *parent, *running, *new_node;

  if (!free_blocks_tree) { /* Empty tree */
    free_blocks_tree = create_new_node(block, NULL);
  } else {
    running = free_blocks_tree;
    parent = NULL;
    while (running){
      parent = running; 
      if (size < running->size_this_node) {
        if (size > running->max_left) { /* Update sizes as we go down */
          running->max_left = size;
        }
        running = running->left;
      } else if (size > running->size_this_node) {
        if (size > running->max) {
          running->max = size;
        }
        running = running->right;
      } else break;
    }
    
    if (running == parent) { /* Do not create node */
      add_block_to_node(running, block);
    } else { /* Change parent */
      new_node = create_new_node(block, parent);
      if (size < parent->size_this_node) {
        parent->left = new_node;
        parent->max_left = size;
      } else {
        parent->right = new_node;
        parent->max = size;
      }
    }
  }
  DEBUG__TRACE(debug_mem, "*Put back block of %" PRIdm " bytes\n", block->size);
}

/* Remove a block from the free blocks pool. */

static void remove_free_block(mem_block_t *block) {
  block_tree_t *node = block->which_node;
  block_tree_t *parent, *running, *successor, *child;

  remove_block_from_node(node, block);
  if (!node->block_list) {         /* Delete node.  Keep the tree sorted. */
    if (!node->right && !node->left){
      /* Change father's link and recompute upwards partial maxima */
      if (!(parent = node->parent)) 
        free_blocks_tree = NULL;
      else
        if (parent->left == node)
          parent->left = NULL;
        else {
          parent->right = NULL;
          parent->max = parent->size_this_node;
          while((running = parent->parent) && running->right == parent) {
            running->max = parent->max;
            parent = running;
          }
          if (running && running->left == parent)
            running->max_left = parent->max;
        }
    }
    
    if (!node->right && node->left){ 
      parent = node->parent;
      child = node->left;
      child->parent = parent;
      if (!parent)
        free_blocks_tree = child;
      else if (parent->left == node) {
        parent->left = child;
        parent->max_left = child->max;
      } else {
        parent->right = child;
        parent->max = child->max;
        while((running = parent->parent) && running->right == parent) {
            running->max = parent->max;
            parent = running;
        }
        if (running && running->left == parent)
          running->max_left = parent->max;
      }
    }

    if (node->right && !node->left){ 
      /* Link right father son to  */
      parent = node->parent;
      child = node->right;
      child->parent = parent;
      if (!parent)
        free_blocks_tree = child;
      else if (parent->left == node)
        parent->left = child;
      else parent->right  = child;
    }
    
    if (node->right && node->left){ 
      /* Search the successor, and replace deleted node by it.
         Nothing has to be done upwards the deleted node. */
      successor = node->right;
      while(successor->left) {
        successor = successor->left;
      }
      if (successor == node->right) {                    /* Immediate son */
        successor->left = node->left;
        successor->max_left = node->max_left; 
        if (successor->left) 
          successor->left->parent = successor;
        parent = node->parent;
        successor->parent = parent;
        if (!parent)
          free_blocks_tree = successor;
        else if (node == parent->left)
          parent->left = successor;
        else parent->right = successor;
      } else {                                        /* Unlink successor */
        successor->parent->left = successor->right;
        if (successor->right) 
          successor->right->parent = successor->parent;
        successor->left = node->left;
        successor->right = node->right;
        successor->max_left = node->max_left;
        successor->max = node->max;
        successor->left->parent = successor;
        successor->right->parent = successor;
        parent = node->parent;
        successor->parent = node->parent;
        if (!parent)
          free_blocks_tree = successor;
        else if (node == parent->right)
          parent->right = successor;
        else parent->left = successor;
      }
    }
    /* Dispose this tree node -- but keep it for ourselves! */
    node->right = disposed_tree_nodes;
    disposed_tree_nodes = node;
  }
}

#if defined(USE_LOWRTCHECKS)
static void test_tree(void) {
  if (free_blocks_tree) {
    test(free_blocks_tree);
  }
}

/* 
   Each son points back to its parent.
   Each node has left and right sizes correctly set.
*/

static void test(block_tree_t *t) {
  if (!t) return;
  if (t->right){
    if (t->right->parent != t)
      TRACE_PRINTF("***** Right node does not point back to parent!\n");
    if (t->max <= t->size_this_node)
      TRACE_PRINTF("***** Max subtree size less than node size!\n");
    if (t->max != t->right->max)
      TRACE_PRINTF("***** Max size does not agree with right subtree size!\n");
    test(t->right);
  } else {
    if (t->max != t->size_this_node)
      TRACE_PRINTF("***** Max node and subtree size do not agree!\n");
  }
  if (t->left){
    if (t->left->parent != t)
      TRACE_PRINTF("***** Left node does not point back to parent!\n");
    if (t->max_left != t->left->max)
      TRACE_PRINTF("***** Left max size does not agree with left subtree size!\n");
    test(t->left);
  }
}
#endif

/* Creates a new tree node and initializes all of its fields. */

static block_tree_t *create_new_node(mem_block_t *block,
                                     block_tree_t *parent) {
  block_tree_t *node;

  if (disposed_tree_nodes) {
    node = disposed_tree_nodes;
    disposed_tree_nodes = disposed_tree_nodes->right;
  } else node = (block_tree_t *)malloc(sizeof(block_tree_t));

  node->left = node->right = NULL;
  block->next_free = block->prev_free = NULL;
  node->size_this_node = node->max_left = node->max = block->size;
  node->parent = parent;
  node->block_list = block;
  block->which_node = node;
  return node;
}

/* Add a block to the node; put it first in the list.  Prec.: there is a
   nonempty block list hanging from the node */

static void add_block_to_node(block_tree_t *node, mem_block_t *block) {
  mem_block_t *first = node->block_list;

  block->prev_free = NULL;
  block->next_free = first;
  first->prev_free = block;
  node->block_list = block;
  block->which_node = node;
}

static void remove_block_from_node(block_tree_t *node, mem_block_t *block) {
  mem_block_t *previous = block->prev_free;
  mem_block_t *next = block->next_free;

  if (next)
    next->prev_free = previous;
  if (previous)
    previous->next_free = next;
  else 
    node->block_list = next;
}

/* Link a block into the list.  previous == NULL if first block, next ==
   NULL if last block . */

static void insert_block(mem_block_t *block,
                         mem_block_t *previous,
                         mem_block_t *next) {
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL) {
    previous->fwd = block;
  } else {
    /* Then block is the first in the list */
    block_list = block;
  }
  if (next != NULL) next->bck = block;
}

#if defined(USE_LOWRTCHECKS)
/* TODO: recover and enhance memory block checks */
void dump_blocks(void) {
  mem_block_t *running = block_list;

  while (running) {
    TRACE_PRINTF("%" PRIdm " bytes (%s) from %p to %p\n",
                 running->size,
                 running->block_is_free ? "free" : "used",
                 MEM_BLOCK_PTR(running),
                 (MEM_BLOCK_PTR(running) + running->size));
    running = running->fwd;
  }
  TRACE_PRINTF("\n");
}
#endif

#endif /* USE_OWN_MALLOC_BINARY */

/* --------------------------------------------------------------------------- */
/* (Common) */

#if defined(USE_OWN_MALLOC)

#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
static char *mmap_base = NULL;
#endif

static mem_block_t *new_block_at(char *mem, intmach_t size) {
  mem_block_t *new_block = (mem_block_t *)mem;
  new_block->size = size;
  insert_block(new_block, NULL, block_list);  
  new_block->block_is_free = TRUE;
  insert_free_block(new_block);
  return new_block;
}

void init_own_malloc(void) {
#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
  intmach_t mmap_size = OWNMALLOC_MmapSize; /* in bytes */
  mem_block_t *new_block;

  mmap_base = (char *)SMALLPTR_BASE; 
  if (own_fixed_mmap((void *) mmap_base, mmap_size)) {
    fprintf(stderr, "PANIC: cannot mmap() own memory at %p!!!\n", mmap_base);
    exit(-1);
  }

  /* What follows is basically a create_new_block which allocates 
     all memory we can address */
  new_block = new_block_at(mmap_base, ALIGN_SIZE(mmap_size - 2*HEADER_SIZE));
#endif
}

#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
/* Create a new block with a given size, rounded upwards to be a multiple of
   an ALIGNed word.  If the creation was sucessful, insert them into the
   general blocks list and into the free blocks list. */
/* pre: size is aligned to ALIGN */
static mem_block_t *create_new_block(intmach_t size) {
  char *mem;

  mem = (char *)malloc(MEM_BLOCK_SIZE(size));
  if (!mem) {
    DEBUG__TRACE(debug_mem,
                 "malloc: could not allocate %" PRIdm " bytes of memory\n", 
                 MEM_BLOCK_SIZE(size));
    return NULL;
  }
  return new_block_at(mem, size);
}
#endif

/* Given a block which has enough room to allocate the requested chars,
   return pointer to memory area and split the block into two if needed. */
/* pre: req_chars is aligned to ALIGN */
static char *reserve_block(intmach_t req_chars, mem_block_t *block) {
  RTCHECK({
    if (block->size < req_chars) {
      fprintf(stderr, "{panic: reserve_block received a block smaller than needed}\n");
      return NULL;
    }
  });

  if (block->size >= MEM_BLOCK_SIZE(req_chars)) {
    /* Note: >= and not >, since blocks of size==0 can be merged to
       contiguous free blocks */
    mem_block_t *new_block;
    /* Split the block */
    remove_free_block(block);
#if defined(OWN_MALLOC_UPPER)
    /* Allocate in the upper parts of the reserved memory first */
    block->size -= MEM_BLOCK_SIZE(req_chars);
    insert_free_block(block);
    //
    new_block = (mem_block_t *)((char *)block + MEM_BLOCK_SIZE(block->size));
    new_block->size = req_chars;
    insert_block(new_block, block, block->fwd);
    new_block->block_is_free = FALSE;
#else
    /* Allocate in the lower parts of the reserved memory first */
    mem_block_t *block0;
    block0 = (mem_block_t *)((char *)block + MEM_BLOCK_SIZE(req_chars));
    block0->size = block->size - MEM_BLOCK_SIZE(req_chars);
    insert_block(block0, block, block->fwd);
    block0->block_is_free = TRUE;
    //
    new_block = block;
    new_block->block_is_free = FALSE;
    new_block->size = req_chars;
    insert_free_block(block0);
#endif
    return MEM_BLOCK_PTR(new_block);
  } else {
    /* Remaning part too small */
    /* (??) OC: Exactly the size we want */
    block->block_is_free = FALSE;
    remove_free_block(block);
    return MEM_BLOCK_PTR(block);
  }
}

char *own_malloc(intmach_t size) {
  mem_block_t *block;
  char *pointer_returned;
  intmach_t size_in_chars;

  RTCHECK({
    if (size <= 0) {
      TRACE_PRINTF("own_malloc received a request of %" PRIdm " chars... what should I do?\n",
              size);
      return NULL;
    }
  });
  
  size_in_chars = ALIGN_SIZE(size);
  LOCATE_BLOCK(block, size_in_chars);
  if (block == NULL) {
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
    block = create_new_block(ADJUST_BLOCK(size_in_chars));
    if (block == NULL) {
#endif
      /* malloc could not stand it! */
      DEBUG__TRACE(debug_mem,
                   "own_malloc: could not reserve %" PRIdm " chars\n", size);
      return NULL;
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
    }
#endif
  }
  pointer_returned = reserve_block(size_in_chars, block);
  DEBUG__TRACE(debug_mem,
               "own_malloc returned %p, %" PRIdm " chars\n", 
               pointer_returned, size);

  return pointer_returned;
}

/* Mark a block as unused.  Collapse it with the surronding ones if possible */
static void dealloc_block(mem_block_t *block) {
  mem_block_t *next, *prev;

  block->block_is_free = TRUE;

  if (((next = block->fwd) != NULL) &&        /* Check if next block free */
      (next->block_is_free == TRUE) &&
      ((char *)block + MEM_BLOCK_SIZE(block->size) == (char *)next)) {
    block->size += MEM_BLOCK_SIZE(next->size);
    if ((block->fwd = next->fwd) != NULL) {
      block->fwd->bck = block;
    }
    remove_free_block(next);
  }
  
  if (((prev = block->bck) != NULL) &&
      (prev->block_is_free == TRUE) &&
      ((char *)prev + MEM_BLOCK_SIZE(prev->size) == (char *)block)) {
    remove_free_block(prev);
    prev->size += MEM_BLOCK_SIZE(block->size);
    insert_free_block(prev);
    if ((prev->fwd = block->fwd) != NULL) {
      prev->fwd->bck = prev;
    }
  } else {
    insert_free_block(block);
  }
}

void own_free(char *ptr) {
  mem_block_t *block_to_dealloc;

  block_to_dealloc = PTR_TO_MEM_BLOCK(ptr);
  if (block_to_dealloc != NULL) {
    dealloc_block(block_to_dealloc);
  }
}

/* From the manpages:
   realloc() changes the size of the block pointed to by ptr to size bytes
   and returns a pointer to the (possibly moved) block.  The contents will
   be unchanged up to the lesser of the new and old sizes.  If ptr is NULL,
   realloc() behaves like malloc() for the specified size.  If size is zero
   and ptr is not a null pointer, the object pointed to is freed.
*/

#define MIN(A,B) ((A) <= (B) ? (A) : (B))

/* size in chars */
char *own_realloc(char *ptr, intmach_t size) {
  mem_block_t *old_block;
  intmach_t size_to_copy;

  if (ptr == NULL) {
    return own_malloc(size);
  } else if (size == 0) {
    own_free(ptr);
    return NULL;
  }

  old_block = PTR_TO_MEM_BLOCK(ptr);
  if (old_block == NULL) return NULL;

  {
    mem_block_t *new_block;
    char *new_mem_area;
    intmach_t size_in_chars = ALIGN_SIZE(size);

    LOCATE_BLOCK(new_block, size_in_chars);
    if (new_block == NULL) {
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
      new_block = create_new_block(ADJUST_BLOCK(size_in_chars));
      if (new_block == NULL) {
#endif
        /* malloc could not stand it! */
        DEBUG__TRACE(debug_mem,
                     "own_realloc: could not reserve %" PRIdm " chars!\n", size);
        return NULL;
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
      } 
#endif
    }
    new_mem_area = reserve_block(size_in_chars, new_block);
    size_to_copy = MIN(old_block->size, size_in_chars);
    (void)memcpy(new_mem_area, 
                 MEM_BLOCK_PTR(old_block),
                 size_to_copy);
    own_free(ptr);
    DEBUG__TRACE(debug_mem,
                 "own_realloc returned %p, %" PRIdm " chars\n",
                 new_mem_area, size);

    return new_mem_area;
  }
} 

#endif /* USE_OWN_MALLOC */
