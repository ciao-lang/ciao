struct space* create_space(void)
{
  struct space* space = (struct space*) checkalloc (sizeof(struct space));
  space->size = 1;
  space->limit = INI_SIZE_G;
  space->edges = (int**) checkalloc (INI_SIZE_G * sizeof(int*));

  int i;
  for (i = 0; i < INI_SIZE_G; i++) {
    space->edges[i] = (int*) checkalloc (INI_SIZE_G * sizeof(int));
  }
  space->edges[0][0] = 0;

  space->pi = (int*) checkalloc (INI_SIZE_G * sizeof(int));
  space->pi[0] = 0;
  return space;
}

void delete_space(struct space *space)  
{
  int i;
  for (i = 0 ; i < space->limit; i++) {
    checkdealloc((tagged_t *)space->edges[i],space->limit * sizeof(int));
  }
  checkdealloc((tagged_t *)space->edges,space->limit * sizeof(int*));
  checkdealloc((tagged_t *)space->pi, space->limit * sizeof(int));
  checkdealloc((tagged_t *)space,sizeof(struct space));
}

void print_space(struct space *space)
{
  printf("\n\tSEPARATION LOGIC SPACE:\n"); fflush(stdout);
  int i,j;
  for (i = 0; i < space->size; i++) {
    printf("\nE %d:",i);
    for (j = 0; j < space->size; j++) {
      printf("\t%d",space->edges[i][j]);
    }
  }
  printf("\nPI :");
  for (j = 0; j < space->size; j++) {
    printf("\t%d",space->pi[j]);
  }
}

struct space* clone_space(struct space *s)
{
//  struct timeval t_ini, t_fin;
//  gettimeofday(&t_ini,NULL);
//  ALLOC_GLOBAL_TABLE(res,struct space*,sizeof(struct space));
  struct space *res = (struct space*) checkalloc (sizeof(struct space));
  res->size = s->size;
  res->limit = s->size * FACTOR_G;

//  ALLOC_GLOBAL_TABLE(res->edges,int**,res->limit*sizeof(int*));
  res->edges = (int**) checkalloc (res->limit*sizeof(int*));
  int i;
  for (i = 0; i < res->limit; i++) {
//      ALLOC_GLOBAL_TABLE(res->edges[i],int*,res->limit*sizeof(int));
    res->edges[i] = (int*) checkalloc (res->limit * sizeof(int));
    int j;
    if (i >= res->size) continue;
    for (j = 0; j < res->size; j++) {
      res->edges[i][j] = s->edges[i][j];
    }
  }

//  ALLOC_GLOBAL_TABLE(res->pi,int*,res->limit*sizeof(int));
  res->pi = (int*) checkalloc (res->limit*sizeof(int));
  for (i = 0; i < res->size; i++) res->pi[i] = s->pi[i];
//  gettimeofday(&t_fin,NULL);
//  trail_time = trail_time + timeval_diff(&t_fin, &t_ini);
  return res;
}

CFUN__PROTO(proy_space, struct space*,
	    int size, tagged_t* vars, int* orig_attrs, int undo) {
//  struct timeval t_ini, t_fin;
//  gettimeofday(&t_ini,NULL);
  //Make an array of attr_index from vars
  int i;
  if (undo) {
    for (i = 0; i < size; i++) {
      //Update attributtes 
      MAKE_UNDO_ATTR(w, vars[i], i+1);
    }
  }

  struct space *res = (struct space*) checkalloc (sizeof(struct space));
  res->size = size + 1;
  res->limit = res->size;

  res->edges = (int**) checkalloc (res->limit * sizeof(int*));
  for (i = 0; i < res->limit; i++) {
    res->edges[i] = (int*) checkalloc (res->limit * sizeof(int));
    int ii;
    if (i == 0) ii = 0;
    else ii = orig_attrs[i-1];
    int j;
    for (j = 0; j < res->limit; j++) {
      int jj;
      if (j == 0) {
	jj = 0;
      } else {
	jj = orig_attrs[j-1];
      }
      res->edges[i][j] = space->edges[ii][jj];
    }
  }

  res->pi = (int*) checkalloc (res->limit*sizeof(int));
  res->pi[0] = space->pi[0];
  for (i = 1; i < res->size; i++) {
    res->pi[i] = space->pi[orig_attrs[i-1]];
  }

//  gettimeofday(&t_fin,NULL);
//  trail_time = trail_time + timeval_diff(&t_fin, &t_ini);
  return res;
}

void print_variable_space(struct space *s, int id) 
  {
    printf("[%d,%d]",-s->edges[0][id],s->edges[id][0]); 
  }

int isValue_space(struct space*s, int id) 
{
  return (s->edges[id][0] == -s->edges[0][id]);
}

CVOID__PROTO(delay_space, struct space *s, int v) {
  //Creating undo/1 predicate
  MAKE_UNDO_DC(Arg,v,0,s->edges[v][0],MAX);
  s->edges[v][0] = MAX;
}

CVOID__PROTO(reset_space, struct space *s, int x, int y, int v) {
  add_diff_const_space(Arg,s,y,v,s->edges[0][v]);
  add_diff_const_space(Arg,s,v,y,s->edges[v][0]);

  //Creating undo/1 predicates
  MAKE_UNDO_DC(Arg,x,v,s->edges[x][v],MAX);
  MAKE_UNDO_DC(Arg,v,x,s->edges[v][x],MAX);

  s->edges[x][v] = MAX;
  s->edges[v][x] = MAX;
}

CVOID__PROTO(full_abstraction_space, struct space *s, int v1, int v2) {
  if (s->edges[v1][v2] == 0) 
    {
      //Creating undo/1 predicate
      MAKE_UNDO_DC(Arg,v1,v2,s->edges[v1][v2],s->edges[v2][v1]);
      s->edges[v1][v2] = s->edges[v2][v1];
    }
  if (s->edges[v2][v1] == 0) 
    {
      //Creating undo/1 predicate
      MAKE_UNDO_DC(Arg,v2,v1,s->edges[v2][v1],s->edges[v1][v2]);
      s->edges[v2][v1] = s->edges[v1][v2];
    }
}

CVOID__PROTO(normalize_space, struct space *s, int i, int j, int L, int U) {
  if (s->edges[i][j] >= L) 
    {
      //Creating undo/1 predicate
      MAKE_UNDO_DC(Arg,i,j,s->edges[i][j],MAX);
      s->edges[i][j] = MAX;
    }
  else if (-s->edges[0][i] >= L) 
    {
      //Creating undo/1 predicate
      MAKE_UNDO_DC(Arg,i,j,s->edges[i][j],MAX);
      s->edges[i][j] = MAX;
    }
  else if ((-s->edges[0][j] >= U) && (i != 0))
    {
      //Creating undo/1 predicate
      MAKE_UNDO_DC(Arg,i,j,s->edges[i][j],MAX);
      s->edges[i][j] = MAX;
    }
  else if ((-s->edges[0][j] >= U) && (i == 0)) 
    {
      //Creating undo/1 predicate
      MAKE_UNDO_DC(Arg,i,j,s->edges[i][j],-U);
      s->edges[i][j] = -U;
    }
}

//Create a new different constraint variable
CFUN__PROTO(new_diff_var_space, int, struct space *s) {
  int id = s->size;
  int i;

  //Creating undo/1 predicate
  MAKE_UNDO_VAR(Arg);
  s->size++;

  if (s->size > s->limit) 
    {
      int old_limit = s->limit;
      s->limit = FACTOR_G * s->limit;
      s->edges = (int**) checkrealloc((tagged_t *)s->edges, 
				      old_limit * sizeof(int*), 
				      s->limit * sizeof(int*));
      for (i = 0; i < id; i++) {
	s->edges[i] = (int*) checkrealloc((tagged_t *)s->edges[i], 
					  old_limit * sizeof(int),
					  s->limit * sizeof(int));
      }
      for (i = id; i < s->limit; i++) {
	s->edges[i] = (int*) checkalloc(s->limit * sizeof(int));
      }
    
      s->pi = (int*) checkrealloc((tagged_t*)s->pi, old_limit * sizeof(int),
				   s->limit * sizeof(int));
    }

  for (i = 0; i < id; i++) s->edges[i][id] = MAX;
  for (i = 0; i < id; i++) s->edges[id][i] = MAX;
  s->edges[id][id] = 0;
  s->pi[id] = 0;
  
  return id;
}

//Adds a new different constraint 
CBOOL__PROTO(add_diff_const_space, struct space *s, int x, int y, int d) {
  if ((x >= s->size) || (x < 0)) 
    {
      printf("\nDiff Const Var out of range\n");
      return FALSE;
    }

  if ((y >= s->size) || (y < 0)) 
    {
      printf("\nDiff Const Var out of range\n");
      return FALSE;
    }

  //It is subsumed.
  if (d >= s->edges[x][y]) return TRUE;
    
  if (d == -MAX) return FALSE;

  //Creating undo/1 predicate
  MAKE_UNDO_DC(Arg,x,y,s->edges[x][y],d);
  s->edges[x][y] = d;
  if ((s->pi[x] + d - s->pi[y]) >= 0) return TRUE;

  //Initialazing structures
  int *gamma = (int*) checkalloc (s->size * (sizeof(int)));
  int *newpi = (int*) checkalloc (s->limit * (sizeof(int)));
  int i;
  for (i = 0; i < s->size; i++) {
    gamma[i] = 0;
    newpi[i] = s->pi[i];
  }
  gamma[y] = s->pi[x] + d - s->pi[y];

  //loop
  int min = gamma[y];
  int current = y;
  while ((min < 0) && (gamma[x] == 0))
    {
      newpi[current] = s->pi[current] + gamma[current];
      gamma[current] = 0;
	
      int e;
      for (e = 0; e < s->size; e++) {
	if ((e != current) && (s->edges[current][e] != MAX) && 
	    (newpi[e] == s->pi[e])) {
	  int tmp = newpi[current] + s->edges[current][e] - newpi[e];
	  if (tmp < gamma[e]) gamma[e] = tmp;
	}
      }
      //argmin
      min = 0;
      for (e = 0; e < s->size; e++) {
	if (gamma[e] < min) {
	  min = gamma[e];
	  current = e;
	}
      }
    }

    int res = gamma[x];

    //Freeing structures
    checkdealloc((tagged_t *)gamma, s->size * (sizeof(int)));
    //Creating undo/1 predicate
    MAKE_UNDO_PI(Arg,s->pi,newpi);

    checkdealloc((tagged_t *)s->pi, s->limit * (sizeof(int)));
    s->pi = newpi;

    return (res >= 0);
  }

//Computes shortest paths
CVOID__PROTO(dijkstra_space, struct space*s, int v) {
  int *dist = (int*) checkalloc (s->size * sizeof(int));
  int *visit = (int*) checkalloc (s->size * sizeof(int));
  int i;
  for (i = 0; i < s->size; i++) {
    visit[i] = 0;
    dist[i] = MAX;
  }
  dist[v] = 0;
  visit[v] = 1;
  
  int end = FALSE;
  int u = v;
  
  while (!end) {
    int e;
    for (e = 0; e < s->size; e++) {
      if (s->edges[u][e] != MAX) {
	int tmp = dist[u] + s->edges[u][e] + s->pi[u] - s->pi[e];
	if (tmp < dist[e]) {
	  dist[e] = tmp;
	  //Better implications in the graph
	  // edges[v][e] = tmp + pi[e] - pi[v]; 
	  add_diff_const_space(Arg, s, v, e, tmp + s->pi[e] - s->pi[v]); 
	}
      }
    }
      
    int min = MAX;
    int i;
    for (i = 0; i < s->size; i++) {
      if (!visit[i] && (dist[i] < min)) {
	u = i;
	min = dist[i];
      }
    }
      
    visit[u] = 1;
    if (min == MAX) end = TRUE;
  }        

  checkdealloc((tagged_t *)dist, s->size * sizeof(int));
  checkdealloc((tagged_t *)visit, s->size * sizeof(int));
}

CVOID__PROTO(get_shortest_path_space, struct space *s, int size, int *orig_vars) {
  //Executing Dijkstra
  dijkstra_space(Arg,s,0);
  int i;
  for (i = 0; i < size; i++) {
    dijkstra_space(Arg,s,orig_vars[i]);
  }
}

int is_more_general_space(struct space *s1, int size, 
			  struct space *s2, int *vars) 
{
  //Checking for entailment
  int i, j;
  for (j = 0; j < size; j++) {
    if (s1->edges[0][j+1] < s2->edges[0][vars[j]]) 
      return FALSE;
  }

  for (i = 0; i < size; i++) {
    if (s1->edges[i+1][0] < s2->edges[vars[i]][0]) 
      return FALSE;
  }
    
  for (i = 0; i < size; i++) {
    for (j = 0; j < size; j++) {
      if (s1->edges[i+1][j+1] < s2->edges[vars[i]][vars[j]]) 
	return FALSE;
    }
  }
  
  return TRUE;
}

