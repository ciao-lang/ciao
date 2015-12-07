#include <stdio.h>

struct object {
  char *name;
  char *colour;
};

#define OBJECTS 3

struct object objects[OBJECTS] =
{ {"ring","golden"},
  {"table","brown"},
  {"bottle","green"} };

struct object *object(int n) {
  return &objects[n % OBJECTS];
}

void show_object(struct object *o) {
  printf("I show you a %s %s\n", o->colour, o->name);
}

