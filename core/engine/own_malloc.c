/*
 *  own_malloc.c
 *
 *  Own memory manager.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *  Copyright (C) 2020 Ciao Development Team
 *
 *  Author:
 *    Manuel Carro
 *    Jose F. Morales (minor changes)
 */

// #if !defined(OPTIM_COMP)
// #define OPTIM_COMP 1
// #endif

#if defined(OPTIM_COMP)
#include <ciao/basiccontrol.native.h>
#else
#include <ciao/datadefs.h>
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

#if defined(DEBUG)
#include <stdio.h>
#endif

/* TODO: share more definitions of both LINEAR and BINARY versions */

#if defined(USE_OWN_MALLOC)

/* First-fit linear search-based allocator (simpler but not optimal) */
//#define USE_OWN_MALLOC_LINEAR 1
/* Unbalanced binary tree search-based allocator */
#define USE_OWN_MALLOC_BINARY 1
#define OWN_MALLOC_UPPER 1 /* TODO: it was enabled with LINEAR, why? */

#define ALIGN OWNMALLOC_ALIGN
#define TW_TO_CHARS(Tw) (Tw)*ALIGN
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)

#define ADJUST_BLOCK(SIZE) ((SIZE) < CHARS_TO_TW(OWNMALLOC_BLOCKSIZE) ? CHARS_TO_TW(OWNMALLOC_BLOCKSIZE) : (SIZE))

#define HEADER_SIZE ((CHARS_TO_TW(sizeof(mem_block_t))) - 1)
#define MEM_BLOCK_SIZE(Units) (HEADER_SIZE+(Units))

#define MEM_BLOCK_PTR(BLOCK) (&((BLOCK)->mem_ptr))
#define PTR_TO_MEM_BLOCK(Ptr) ((mem_block_t *)((Ptr) - HEADER_SIZE))

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
  intmach_t size; /* Measured in tagged_t words */
  tagged_t mem_ptr; /* 1st word of the allocated memory */
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

static mem_block_t *locate_block(intmach_t tagged_w);

/* Search for a block with memory enough. Requested size comes in tagged_t
   words.  If no block found, return NULL. */

#define LOCATE_BLOCK(BLOCK, SIZE) (BLOCK) = locate_block((SIZE))

static mem_block_t *locate_block(intmach_t requested_tagged_w) {
  mem_block_t *running;

#if defined(DEBUG)
  if (debug_mem)
    printf("locate_block was requested a block of %d words\n",
           requested_tagged_w);
  if (requested_tagged_w < 1)
    printf("Uh? locate_block was requested a block of %d words!\n",
           requested_tagged_w);
#endif

    running = free_block_list;
    while(running && running->size < requested_tagged_w) 
      running = running->next_free;

#if defined(DEBUG)
  if (debug_mem) {
    if (!running )
      printf("locate_block did not find a block of %d words\n",
             requested_tagged_w);
  }
#endif
  
  return running;
}

/* Link a block into the list.  previous == NULL if first block, next ==
   NULL if last block . */

static void insert_block(mem_block_t *block,
                         mem_block_t *previous,
                         mem_block_t *next)
{
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL)
    previous->fwd = block;
  else                             /* Then block is the first in the list */
    block_list = block;
  if (next != NULL) next->bck = block;
}


/* Link a block into the free blocks list. */

static void insert_free_block(mem_block_t *block)
{
  block->prev_free = NULL;
  block->next_free = free_block_list;
  if (free_block_list)
    free_block_list->prev_free = block;
  free_block_list = block;    
#if defined(DEBUG)
  if (debug_mem)
    printf("*Put back block of %d words\n", block->size);
#endif
}

/* Remove a block from the free blocks list. */

static void remove_free_block(mem_block_t *block)
{
  mem_block_t *previous = block->prev_free,
            *next     = block->next_free;

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

static block_tree_t *search_free_block(intmach_t tagged_w);
static block_tree_t *create_new_node(mem_block_t *block, block_tree_t *parent);
static void remove_block_from_node(block_tree_t *node, mem_block_t  *block);
static void add_block_to_node(block_tree_t *node, mem_block_t  *block);

#if defined(DEBUG)
static void test_tree(void);
static void test(block_tree_t *tree);
#endif

/* Search for a block with memory enough. Requested size comes in tagged_t
   words.  If no block found, return NULL. */

#define LOCATE_BLOCK(BLOCK, SIZE) { \
  block_tree_t *node; \
  node = search_free_block((SIZE)); \
  if (node == NULL) { \
    (BLOCK) = NULL; \
  } else { \
    (BLOCK) = node->block_list; \
  } \
}

static block_tree_t *search_free_block(intmach_t requested_tagged_w)
{
  block_tree_t *running;

#if defined(DEBUG)
  if (debug_mem)
    printf("search_free_block was requested a block of %d words\n",
           requested_tagged_w);
  if (requested_tagged_w < 1)
    printf("Uh? search_free_block was requested a block of %d words!\n",
           requested_tagged_w);
#endif

  running = free_blocks_tree;
  while(running) {
    if (running->left && requested_tagged_w <= running->max_left)
      running = running->left;
    else if (requested_tagged_w <= running->size_this_node)
      break;
    else if (running->right && requested_tagged_w <= running->max)
      running = running->right;
    else
      running = NULL;
  }

#if defined(DEBUG)
  if (debug_mem) {
    if (!running )
      printf("search_free_block did not find a block of %d words\n",
             requested_tagged_w);
    test_tree();
  }
#endif
  
  return running;
}

/* Link a block into the free blocks pool. */

static void insert_free_block(mem_block_t *block)
{
  intmach_t size = block->size;
  block_tree_t *parent, *running, *new_node;

  if (!free_blocks_tree) { /* Empty tree */
    free_blocks_tree = create_new_node(block, NULL);
  } else {
    running = free_blocks_tree;
    parent = NULL;
    while (running){
      parent = running; 
      if (size < running->size_this_node){
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
      if (size < parent->size_this_node){
        parent->left = new_node;
        parent->max_left = size;
      } else {
        parent->right = new_node;
        parent->max = size;
      }
    }
  }
#if defined(DEBUG)
  if (debug_mem)
    printf("*Put back block of %d words\n", block->size);
#endif
}


/* Remove a block from the free blocks pool. */

static void remove_free_block(mem_block_t *block)
{
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

#if defined(DEBUG)
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
      printf("***** Right node does not point back to parent!\n");
    if (t->max <= t->size_this_node)
      printf("***** Max subtree size less than node size!\n");
    if (t->max != t->right->max)
      printf("***** Max size does not agree with right subtree size!\n");
    test(t->right);
  } else {
    if (t->max != t->size_this_node)
      printf("***** Max node and subtree size do not agree!\n");
  }
  if (t->left){
    if (t->left->parent != t)
      printf("***** Left node does not point back to parent!\n");
    if (t->max_left != t->left->max)
      printf("***** Left max size does not agree with left subtree size!\n");
    test(t->left);
  }
}

#endif

/* Creates a new tree node and initializes all of its fields. */

block_tree_t *create_new_node(mem_block_t *block,
                            block_tree_t *parent)
{
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

void add_block_to_node(block_tree_t *node,
                       mem_block_t *block)
{
  mem_block_t *first = node->block_list;

  block->prev_free = NULL;
  block->next_free = first;
  first->prev_free = block;
  node->block_list = block;
  block->which_node = node;
}

void remove_block_from_node(block_tree_t *node,
                            mem_block_t *block)
{
  mem_block_t *previous = block->prev_free,
            *next     = block->next_free;

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
                         mem_block_t *next)
{
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL)
    previous->fwd = block;
  else                             /* Then block is the first in the list */
    block_list = block;
  if (next != NULL) next->bck = block;
}

#if defined(DEBUG)
void dump_blocks(void)
{
  mem_block_t *running = block_list;

  while (running) {
    printf("%d tags (%s) from %x to %x\n",
           (int)running->size, 
           running->block_is_free ? "free" : "used",
           (int)MEM_BLOCK_PTR(running),
           (int)(MEM_BLOCK_PTR(running) + running->size));
    running = running->fwd;
  }
  printf("\n");
}
#endif

#endif /* USE_OWN_MALLOC_BINARY */

/* --------------------------------------------------------------------------- */
/* (Common) */

#if defined(USE_OWN_MALLOC)

#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
tagged_t *mmap_base = NULL;
#endif

static mem_block_t *new_block_at(tagged_t *mem, intmach_t size_in_tagged_w) {
  mem_block_t *new_block = (mem_block_t *)mem;
  new_block->size = size_in_tagged_w;
  insert_block(new_block, NULL, block_list);  
  new_block->block_is_free = TRUE;
  insert_free_block(new_block);
  return new_block;
}

void init_own_malloc(void) {
#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
  intmach_t mmap_size = OWNMALLOC_MmapSize; // In bytes
  mem_block_t *new_block;

  mmap_base = (tagged_t *)SMALLPTR_BASE; 

  if (own_fixed_mmap((void *) mmap_base, mmap_size)) {
    fprintf(stderr, "PANIC: cannot mmap() own memory at %p!!!\n", mmap_base);
    exit(-1);
  }

  // What follows is basically a create_new_block which allocates 
  // all memory we can address
  new_block = new_block_at(mmap_base, CHARS_TO_TW(mmap_size - TW_TO_CHARS(2*HEADER_SIZE)));
#endif
}

/* Create a new block with a given size, rounded upwards to be a multiple of
   an ALIGNed word.  If the creation was sucessful, insert them into the
   general blocks list and into the free blocks list. */

#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
static mem_block_t *create_new_block(intmach_t size_in_tagged_w) {
  tagged_t *mem;

  mem = (tagged_t *)malloc(TW_TO_CHARS(MEM_BLOCK_SIZE(size_in_tagged_w)));
  if (!mem) {
#if defined(DEBUG)
    printf("malloc: could not allocate %d words of memory\n", 
           MEM_BLOCK_SIZE(size_in_tagged_w));
#endif
    return NULL;
  }
  return new_block_at(mem, size_in_tagged_w);
}
#endif


/* Given a block which has enough room to allocate the requested chars,
   return pointer to memory area and split the block into two if needed. */

static tagged_t *reserve_block(intmach_t req_tagged, mem_block_t *block) {
#if defined(DEBUG)
  if (block->size < req_tagged){
    printf("**** Fatal error: reserve_block received a block smaller than needed\n");
    return NULL;
  }
#endif

  if (block->size >= MEM_BLOCK_SIZE(req_tagged)) {
    /* Note: >= and not >, since blocks of size==0 can be merged to
       contiguous free blocks */
    mem_block_t *new_block;
    /* Split the block */
    remove_free_block(block);
#if defined(OWN_MALLOC_UPPER)
    block->size -= MEM_BLOCK_SIZE(req_tagged);
    insert_free_block(block);
    //
    new_block = (mem_block_t *)((tagged_t *)block + MEM_BLOCK_SIZE(block->size));
    new_block->size = req_tagged;
    insert_block(new_block, block, block->fwd);
    new_block->block_is_free = FALSE;
#else
    /* Allocate in the lower parts of the reserved memory first */
    mem_block_t *block0;
    block0 = (mem_block_t *)((tagged_t *)block + MEM_BLOCK_SIZE(req_tagged));
    block0->size = block->size - MEM_BLOCK_SIZE(req_tagged);
    insert_block(block0, block, block->fwd);
    block0->block_is_free = TRUE;
    //
    new_block = block;
    new_block->block_is_free = FALSE;
    new_block->size = req_tagged;
    insert_free_block(block0);
#endif
    return MEM_BLOCK_PTR(new_block);
  } else {
    /* Remaning part too small */
    block->block_is_free = FALSE;
    remove_free_block(block);
    return MEM_BLOCK_PTR(block);
  }
}

/* Our three beloved calls: alloc, dealloc, realloc. */

tagged_t *own_malloc(intmach_t size)
{
  mem_block_t *block;
  tagged_t *pointer_returned;
  intmach_t size_to_reserve;

#if defined(DEBUG)
  if (size <= 0){
    printf("own_malloc received a request of %d chars... what should I do?\n",
           size);
    return NULL;
  }
#endif
  
  size_to_reserve = CHARS_TO_TW(size);
  LOCATE_BLOCK(block, size_to_reserve);
  if (block == NULL) {
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
    block = create_new_block(ADJUST_BLOCK(size_to_reserve));
    if (block == NULL) {
      /* malloc could not stand it! */
#endif
#if defined(DEBUG)
      printf("own_malloc: could not reserve %d chars\n", size);
#endif
      return NULL;
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
    }
#endif
  }
  pointer_returned = reserve_block(size_to_reserve, block);
#if defined(DEBUG)
  if (debug_mem)
    printf("own_malloc returned %x, %d chars\n", 
           (unsigned int)pointer_returned, size);
#endif

  return pointer_returned;
}

/* Mark a block as unused.  Collapse it with the surronding ones if possible */
static void dealloc_block(mem_block_t *block)
{
  mem_block_t *next, *prev;

  block->block_is_free = TRUE;

  if (((next = block->fwd) != NULL) &&        /* Check if next block free */
      (next->block_is_free == TRUE) &&
      ((tagged_t *)block + MEM_BLOCK_SIZE(block->size) == (tagged_t *)next)){
    block->size += MEM_BLOCK_SIZE(next->size);
    if ((block->fwd = next->fwd) != NULL) {
      block->fwd->bck = block;
    }
    remove_free_block(next);
  }
  
  if (((prev = block->bck) != NULL) &&
      (prev->block_is_free == TRUE) &&
      ((tagged_t *)prev + MEM_BLOCK_SIZE(prev->size) == (tagged_t *)block)){
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

void own_free(tagged_t *ptr)
{
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
/* size: size in chars */
tagged_t *own_realloc(tagged_t *ptr, intmach_t size) {
  mem_block_t *old_block;
  intmach_t size_to_copy;

  if (ptr == NULL) {
    return own_malloc(size);
  } else if (size == 0) {
    own_free(ptr);
    return NULL;
  }

  //  fprintf(stderr, "\nRealloc asked for %d words\n", CHARS_TO_TW(size));

  old_block = PTR_TO_MEM_BLOCK(ptr);
  if (old_block == NULL) return NULL;

  {
    mem_block_t *new_block;
    tagged_t *new_mem_area;
    intmach_t size_in_tagged_w = CHARS_TO_TW(size);

    LOCATE_BLOCK(new_block, size_in_tagged_w);
    if (new_block == NULL) {
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
      new_block = create_new_block(ADJUST_BLOCK(size_in_tagged_w));
      if (new_block == NULL) {
#endif
#if defined(DEBUG)
        printf("own_realloc: could not reserve %d chars!\n", size);
#endif
        return NULL;
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
      } 
#endif
    }

    new_mem_area = reserve_block(size_in_tagged_w, new_block);
    size_to_copy = old_block->size > size_in_tagged_w ?
                   size_in_tagged_w : old_block->size;
    (void)memcpy(new_mem_area, 
                 MEM_BLOCK_PTR(old_block),
                 TW_TO_CHARS(size_to_copy));
    own_free(ptr);
#if defined(DEBUG)
    if (debug_mem)
      printf("own_realloc returned %x, %d chars\n",
             (unsigned int)new_mem_area, size);
#endif    

    return new_mem_area;
  }
} 

#endif /* USE_OWN_MALLOC */
