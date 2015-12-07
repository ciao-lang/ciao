#include <stack>
using namespace std;

typedef stack<int> ciao_stack;

extern "C" void *
ciao_stack_new() 
{
  return (void*) new ciao_stack;
}

extern "C" void
ciao_stack_delete(void * S) 
{
  delete ((ciao_stack *) S);
}

extern "C" int
ciao_stack_size(void * S) 
{
  return  (((ciao_stack *) S)->size());
}

extern "C" void
ciao_stack_push(void * S, int v) 
{
  ((ciao_stack *) S)->push(v);
}

extern "C" void 
ciao_stack_pop(void * S) 
{
  ((ciao_stack *) S)->pop();
}

extern "C" int
ciao_stack_top(void * S) 
{
  return ((ciao_stack *) S)->top();
}
