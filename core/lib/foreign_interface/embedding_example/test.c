#include <ciao_prolog.h>
#include <stdio.h>
#include <stdlib.h>

void mydisplay(ciao_term term) {
  if (ciao_is_atom(term)) {
    printf("<atom name=\"%s\"/>", ciao_atom_name(term));
  } else if (ciao_is_structure(term)) {
    int i;
    int a;
    a = ciao_structure_arity(term);
    printf("<structure name=\"%s\" arity=\"%d\">", ciao_structure_name(term), a);
    for (i = 1; i <= a; i++) {
      printf("<argument number=\"%d\">", i);
      mydisplay(ciao_structure_arg(term, i));
      printf("</argument>");
    }
    printf("</structure>");
  } else if (ciao_is_list(term)) {
    printf("<list>");
    printf("<head>");
    mydisplay(ciao_list_head(term));
    printf("</head>");
    printf("<tail>");
    mydisplay(ciao_list_tail(term));
    printf("</tail>");
    printf("</list>");
  } else if (ciao_is_empty_list(term)) {
    printf("<empty_list/>");
  } else if (ciao_is_integer(term)) {
    printf("<integer value=\"%d\"/>", ciao_get_c_int(term));
  } else if (ciao_is_number(term)) {
    printf("<float value=\"%f\"/>", ciao_get_c_float(term));
  } else {
    printf("<unknown/>");
  }
}

int main(void)
{
  ciao_term var, list, complex_term;
  ciao_query *query;

  printf("Initializing Ciao...\n");

  ciao_opts("program_name", 0, NULL, 0, NULL, NULL);
  ciao_init(NULL);

  printf("Creating a Ciao context...\n");

  ciao_implicit_ctx = ciao_ctx_new();

  printf("Loading test.po...\n");

  ciao_load_qfile("test.po");

  printf("Testing...\n");

  ciao_frame_begin();

  complex_term = ciao_var();

  printf("t1:\n");
  ciao_commit_call("test:mydisplay", 1, complex_term);

  printf("t2:\n");
  mydisplay(complex_term);
  printf("\n");
  printf("t3:\n");
  ciao_commit_call("test:complex_term", 1, complex_term);
  printf("t4:\n");
  mydisplay(complex_term);
  printf("\n");

  printf("t5:\n");
  ciao_commit_call("test:mydisplay", 1, complex_term);
  ciao_commit_call("test:mydisplay", 1, complex_term);
  ciao_commit_call("test:mydisplay", 1, complex_term);
  printf("t6:\n");
  mydisplay(complex_term);
  printf("\n");
  
  printf("t7:\n");
  var = ciao_var();

  printf("t8:\n");
  list = ciao_listn(7,
                       ciao_mk_c_int(2),
                       ciao_mk_c_int(3),
                       ciao_mk_c_int(5),
                       ciao_mk_c_int(7),
                       ciao_mk_c_int(11),
                       ciao_mk_c_int(13),
                       ciao_mk_c_int(17)
                       );
  printf("t9:\n");
  printf("Created var %ld\n", list);

  printf("t10:\n");
  ciao_commit_call("test:mydisplay", 1, list);

  printf("Calling member/2\n");
  printf("t11:\n");
  query = ciao_query_begin("test:member", 2, var, list);
  printf("Done\n");
  printf("t12:\n");
  while (ciao_query_ok(query)) {
    printf("There is a solution\n");
    if (ciao_is_integer(var)) {
      printf("Number %d\n", ciao_get_c_int(var));
    } else {
      printf("Not a number!\n");
    }
    printf("Searching next\n");
    ciao_query_next(query);
  }
  printf("End\n");
  printf("t13:\n");
  ciao_query_end(query);

  printf("t14:\n");
  query = ciao_query_begin("test:numbers", 1, var);
  while (ciao_query_ok(query)) {
    if (ciao_is_integer(var)) {
      printf("Number %d\n", ciao_get_c_int(var));
    } else {
      printf("Not a number!\n");
    }
    ciao_commit_call("test:mydisplay", 1, var);
    if (ciao_commit_call("test:member", 2, var, list)) {
      printf("Is a selected number\n");
    } else {
      printf("Is not a selected number\n");
    }
    ciao_query_next(query);
  }
  printf("t15:\n");
  ciao_query_end(query);
  
  printf("t16:\n");
  ciao_frame_end();
  printf("End\n");

  return 0;
}
