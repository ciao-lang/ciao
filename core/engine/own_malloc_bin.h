/*
 *  own_malloc_bin.h
 *
 *  Binary tree implementation of new memory manager.
 *
 *  NOTE: Included from own_malloc.c!!!
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author:
 *    Manuel Carro
 */

/* List blocks. Kept in (ascending?) memory order, to allow recompaction
   upon block freeing.  Use forward and backwards link for this.  Compaction
   is not always possible: blocks of memory can come from sparse
   addresses. */

typedef struct mem_block_str_ mem_block_str_t;
typedef struct block_tree_ block_tree_t;

struct mem_block_str_ {
  bool_t block_is_free;                            /* Might be only one bit */
  mem_block_str_t *fwd, *bck;
  mem_block_str_t *next_free, *prev_free;
  block_tree_t *which_node;
  intmach_t size;                                   /* Measured in tagged_t words */
  tagged_t mem_ptr;                     /* 1st word of the allocated memory */
};

typedef mem_block_str_t MEM_BLOCK;

#define HEADER_SIZE ((CHARS_TO_TW(sizeof(MEM_BLOCK))) - 1)

static MEM_BLOCK *block_list = NULL;  /* Shared, locked */

struct block_tree_ {
  intmach_t size_this_node;
  MEM_BLOCK *block_list;
  block_tree_t *left, *right, *parent;
  intmach_t max_left, max;
};

typedef block_tree_t BLOCK_TREE;

/* All of them locked */

static BLOCK_TREE *free_blocks_tree = NULL;
static BLOCK_TREE *disposed_tree_nodes = NULL;

static tagged_t *reserve_block(intmach_t requested_chars, MEM_BLOCK  *list);
static void insert_block(MEM_BLOCK *block, 
                         MEM_BLOCK *previous,
                         MEM_BLOCK *next);
static BLOCK_TREE *search_free_block(intmach_t tagged_w);
static void insert_free_block(MEM_BLOCK *block);
static void remove_free_block(MEM_BLOCK *block);
static void change_free_block(MEM_BLOCK *block, intmach_t increment);
static BLOCK_TREE *create_new_node(MEM_BLOCK *block, BLOCK_TREE *parent);
static void remove_block_from_node(BLOCK_TREE *node, MEM_BLOCK  *block);
static void add_block_to_node(BLOCK_TREE *node, MEM_BLOCK  *block);
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
static MEM_BLOCK *create_new_block(intmach_t size_in_chars);
#endif
static MEM_BLOCK *find_mem_block(tagged_t *ptr);
static void dealloc_block(MEM_BLOCK *block);
#if defined(DEBUG)
static void test_tree(void);
static void test(BLOCK_TREE *tree);
#endif

#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
tagged_t *mmap_base = NULL;

void init_own_malloc(void) {
  intmach_t mmap_size = OWNMALLOC_MmapSize; // In bytes
  MEM_BLOCK *new_block;

  mmap_base = (tagged_t *)SMALLPTR_BASE; 

  if (own_fixed_mmap((void *) mmap_base, mmap_size)) {
    fprintf(stderr, "PANIC: cannot mmap() own memory at %p!!!\n", mmap_base);
    exit(-1);
  }

  // What follows is basically a create_new_block which allocates 
  // all memory we can address
  new_block = (MEM_BLOCK *)mmap_base;
  new_block->block_is_free = TRUE;
  new_block->size = CHARS_TO_TW(mmap_size - TW_TO_CHARS(2*HEADER_SIZE));
  insert_block(new_block, NULL, block_list);
  insert_free_block(new_block);
  // print_mem_map();
}
#else
void init_own_malloc(void) {}
#endif

/* Search for a block with memory enough. Requested size comes in tagged_t
   words.  If no block found, return NULL. */

static BLOCK_TREE *search_free_block(intmach_t requested_tagged_w)
{
  BLOCK_TREE *running;

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

static void insert_free_block(MEM_BLOCK *block)
{
  intmach_t size = block->size;
  BLOCK_TREE *parent, *running, *new_node;

  if (!free_blocks_tree)                                    /* Empty tree */
    free_blocks_tree = create_new_node(block, NULL);
  else {
    running = free_blocks_tree;
    parent = NULL;
    while (running){
      parent = running; 
      if (size < running->size_this_node){
        if (size > running->max_left)       /* Update sizes as we go down */
          running->max_left = size;
        running = running->left;
      } else if (size > running->size_this_node) {
        if (size > running->max) 
          running->max = size;
        running = running->right;
      } else break;
    }
    
    if (running == parent)                          /* Do not create node */
      add_block_to_node(running, block);
    else {                                                 /* Change parent */
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

static void remove_free_block(MEM_BLOCK *block)
{
  BLOCK_TREE *node = block->which_node;
  BLOCK_TREE *parent, *running, *successor, *child;

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

void change_free_block(MEM_BLOCK *block,
		       intmach_t increment)
{
  remove_free_block(block);
  block->size += increment;
  insert_free_block(block);
}

#if defined(DEBUG)
static void test_tree(void)
{
  if (free_blocks_tree)
    test(free_blocks_tree);
}

/* 
   Each son points back to its parent.
   Each node has left and right sizes correctly set.
*/

static void test(BLOCK_TREE *t)
{
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

void print_mem_map(void)
{
  MEM_BLOCK *moving = block_list;

  while (moving) {
    fprintf(stderr, "addr: \t %x \t len: \t %8d \t free: %d\n", 
           (unsigned int)moving, 
           moving->size,
           moving->block_is_free);
    moving = moving->fwd;
  }
}
#endif

/* Creates a new tree node and initializes all of its fields. */

BLOCK_TREE *create_new_node(MEM_BLOCK *block,
			    BLOCK_TREE *parent)
{
  BLOCK_TREE *node;

  if (disposed_tree_nodes) {
    node = disposed_tree_nodes;
    disposed_tree_nodes = disposed_tree_nodes->right;
  } else node = (BLOCK_TREE *)malloc(sizeof(BLOCK_TREE));

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

void add_block_to_node(BLOCK_TREE *node,
		       MEM_BLOCK *block)
{
  MEM_BLOCK *first = node->block_list;

  block->prev_free = NULL;
  block->next_free = first;
  first->prev_free = block;
  node->block_list = block;
  block->which_node = node;
}

void remove_block_from_node(BLOCK_TREE *node,
			    MEM_BLOCK *block)
{
  MEM_BLOCK *previous = block->prev_free,
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

static void insert_block(MEM_BLOCK *block,
			 MEM_BLOCK *previous,
			 MEM_BLOCK *next)
{
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL)
    previous->fwd = block;
  else                             /* Then block is the first in the list */
    block_list = block;
  if (next != NULL) next->bck = block;
}

/* Given a block which has enough room to allocate the requested chars,
   return pointer to memory area and split the block into two if needed. */

static tagged_t *reserve_block(intmach_t req_tagged,
			       MEM_BLOCK *block)
{
#if defined(DEBUG)
  if (block->size < req_tagged){
    printf("**** Fatal error: reserve_block received a block smaller than needed\n");
    return NULL;
  }
#endif

  if (block->size >= req_tagged +  HEADER_SIZE) {
    MEM_BLOCK *new_block;                               /*Split the block */
   
    change_free_block(block, -(req_tagged + HEADER_SIZE));
    new_block = (MEM_BLOCK *)((tagged_t *)block + HEADER_SIZE + block->size);
    new_block->block_is_free = FALSE;
    new_block->size = req_tagged;
    insert_block(new_block, block, block->fwd);
    return &(new_block->mem_ptr);
  } else {                                    /* Remaning part too small */
    block->block_is_free = FALSE;
    remove_free_block(block);
    return &(block->mem_ptr);
  }
}

/* Create a new block with a given size, rounded upwards to be a multiple of
   an ALIGNed word.  If the creation was sucessful, insert them into the
   general blocks list and into the free blocks list. */

#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
static MEM_BLOCK *create_new_block(intmach_t size_in_tagged_w) {
  MEM_BLOCK *new_block;

  new_block = (MEM_BLOCK *)malloc(TW_TO_CHARS(size_in_tagged_w + HEADER_SIZE));
  
  if (!new_block) {
#if defined(DEBUG)
    printf("malloc: could not allocate %d words of memory\n", 
           size_in_tagged_w + HEADER_SIZE);
#endif
    return NULL;
  }
  
  new_block->size = size_in_tagged_w;
  new_block->block_is_free = TRUE;
  insert_block(new_block, NULL, block_list);
  insert_free_block(new_block);
  return new_block;
}
#endif

/* Find the control block which points to the memory area ptr. */

static MEM_BLOCK *find_mem_block(tagged_t *ptr)
{
  MEM_BLOCK *control_block;

  control_block = (MEM_BLOCK *)(ptr - HEADER_SIZE);

#if defined(DEBUG)
  if (!control_block || &(control_block->mem_ptr) != ptr)
    printf("find_mem_block: deallocing wrong memory address (%x)!\n",
           (unsigned int)ptr);
#endif

  return control_block;
}


/* Mark a block as unused.  Collapse it with the surronding ones if possible */

static void dealloc_block(MEM_BLOCK *block)
{
  MEM_BLOCK *next, *prev;

  block->block_is_free = TRUE;

  if (((next = block->fwd) != NULL) &&        /* Check if next block free */
      (next->block_is_free == TRUE) &&
      ((tagged_t *)block + block->size + HEADER_SIZE == (tagged_t *)next)){
    block->size += next->size + HEADER_SIZE;
    if ((block->fwd = next->fwd) != NULL)
      block->fwd->bck = block;
    remove_free_block(next);
  }
  
  if (((prev = block->bck) != NULL) &&
      (prev->block_is_free == TRUE) &&
      ((tagged_t *)prev + prev->size + HEADER_SIZE == (tagged_t *)block)){
    change_free_block(prev, block->size + HEADER_SIZE);
    if ((prev->fwd = block->fwd) != NULL)
      prev->fwd->bck = prev;
  } else insert_free_block(block);
}

#if defined(DEBUG)
void dump_blocks(void)
{
  MEM_BLOCK *running = block_list;

  while (running) {
    printf("%d tags (%s) from %x to %x\n",
           (int)running->size, 
           running->block_is_free ? "free" : "used",
           (int)&(running->mem_ptr),
           (int)(&(running->mem_ptr) + running->size));
    running = running->fwd;
  }
  printf("\n");
}
#endif

/* Our three beloved calls: alloc, dealloc, realloc. */

#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
#define ADJUST_BLOCK(SIZE) ((SIZE) < CHARS_TO_TW(OWNMALLOC_BLOCKSIZE) ? CHARS_TO_TW(OWNMALLOC_BLOCKSIZE) : (SIZE))
#endif

tagged_t *own_malloc(intmach_t size)
{
  MEM_BLOCK  *block;
  BLOCK_TREE *node;
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
  if ((node = search_free_block(size_to_reserve)) == NULL) {
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
    if ((block = create_new_block(ADJUST_BLOCK(size_to_reserve))) == NULL){
      /* malloc could not stand it! */
#endif
#if defined(DEBUG)
      printf("own_malloc: could not reserve %d chars\n", size);
#endif
      return NULL;
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
    }
#endif
  } else block = node->block_list;
  pointer_returned = reserve_block(size_to_reserve, block);
#if defined(DEBUG)
  if (debug_mem)
    printf("own_malloc returned %x, %d chars\n", 
           (unsigned int)pointer_returned, size);
#endif

  return pointer_returned;
}

void own_free(tagged_t *ptr)
{
  MEM_BLOCK *block_to_dealloc;

  if ((block_to_dealloc = find_mem_block(ptr))){
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

tagged_t *own_realloc(tagged_t *ptr, intmach_t size)
{
  MEM_BLOCK *old_block;
  intmach_t        size_to_copy;

  if (ptr == NULL) 
    return own_malloc(size);
  else if (size == 0) {
    own_free(ptr);
    return NULL;
  }

  old_block = find_mem_block(ptr);                /* Gives error messages */
  if (old_block) {
    BLOCK_TREE *node;
    MEM_BLOCK  *new_block;
    tagged_t     *new_mem_area;
    intmach_t        size_in_tagged_w = CHARS_TO_TW(size);

    if ((node = search_free_block(size_in_tagged_w)) == NULL) {
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
      if ((new_block= create_new_block(ADJUST_BLOCK(size_in_tagged_w))) == NULL){
#endif
#if defined(DEBUG)
        printf("own_realloc: could not reserve %d chars!\n", size);
#endif
        return NULL;
#if !(defined(USE_MMAP) && OWNMALLOC_MmapAllowed)
      } 
#endif
    } else new_block = node->block_list;
    new_mem_area = reserve_block(size_in_tagged_w, new_block);
    size_to_copy = old_block->size > size_in_tagged_w ?
                   size_in_tagged_w : old_block->size;
    (void)memcpy(new_mem_area, 
                 &(old_block->mem_ptr),
                 TW_TO_CHARS(size_to_copy));
    own_free(ptr);
#if defined(DEBUG)
    if (debug_mem)
      printf("own_realloc returned %x, %d chars\n",
             (unsigned int)new_mem_area, size);
#endif    

    return new_mem_area;
  } else return NULL;
} 

