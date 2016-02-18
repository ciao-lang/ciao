/*
 *  space.h
 */

/* TODO (JFMC): depends on particular 32-bit tagging scheme */

#ifndef _CIAO_DIFFERENCE_CONSTRAINTS_SPACE_H
#define _CIAO_DIFFERENCE_CONSTRAINTS_SPACE_H

#include <limits.h>

#define INI_SIZE_G      10
#define FACTOR_G        2
#define MAX             INT_MAX

struct space
{
  int size;
  int limit;
  int **edges;
  int *pi;
};


struct space *space;

struct space* create_space(void);
void delete_space(struct space *space);
void print_space(struct space *space);
struct space* clone_space(struct space *s);
CFUN__PROTO(proy_space, struct space *, int size, tagged_t* vars, int* attrs, int undo);
void print_variable_space(struct space *s, int id);
int isValue_space(struct space*s, int id);
CVOID__PROTO(delay_space, struct space *s, int v);
CVOID__PROTO(reset_space, struct space *s, int x, int y, int v);
CVOID__PROTO(full_abstraction_space, struct space *s, int v1, int v2);
CVOID__PROTO(normalize_space, struct space *s, int i, int j, int L, int U);
CFUN__PROTO(new_diff_var_space, int, struct space *s);
CBOOL__PROTO(add_diff_const_space, struct space *s, int x, int y, int d);
CVOID__PROTO(dijkstra_space, struct space*s, int v);
CVOID__PROTO(get_shortest_path_space, struct space *s, int size, int *orig_vars);
int is_more_general_space(struct space *s1, int size, 
			  struct space *s2, int *vars);

#endif /* _CIAO_DIFFERENCE_CONSTRAINTS_SPACE_H */
