/*
  todo: port to Prolog (or Prolog+C)

  Returns the size of each compiled C function

  Fri Mar 11 04:43:05 CET 2005
  --jfran

  Example: 
  objdump -d funcsize | ./funcsize
 */

#include <stdio.h>
#include <string.h>

int main(void) {
  char line[4096];
  char name[1024];
  int addr;
  char c;
  char fun_name[1024];
  int fun_addr0;
  int fun_addr1;
  fun_addr1 = -1;
  do {
    if (!fgets(line, 4096, stdin)) break;
    //    printf("%s\n", line);
    if (sscanf(line, "%x <%[^>]>:\n", &addr, name) == 2) {
      // Print old function
      if (fun_addr1 != -1) {
        printf("funcsize(%s, %d).\n", fun_name, fun_addr1-fun_addr0);
      }
      // New function
      strcpy(fun_name, name);
      fun_addr0 = addr;
      fun_addr1 = addr;
    }
    if (sscanf(line, " %x%c", &addr, &c) == 2 && c == ':') {
      fun_addr1 = addr;
    }
  } while(1);
  // Print old function
  if (fun_addr1 != -1) {
    printf("funcsize(%s, %d).\n", fun_name, fun_addr1-fun_addr0);
  }
}
