#include <stdio.h>
#include <string.h>

#include <ciao_prolog.h>
#include "mathlink.h"

MLINK lp=NULL;
//int pkt=0;
MLEnvironment env=NULL;

char * MLOpen_argv[5]  = {"ciao", "-linkname", "", "-linkmode", "launch"};

int mathematica_init(char * Kernel) {
  
  if (env == NULL) {
    if (lp == NULL) {
      env = MLInitialize(NULL);
      if (env == NULL) {
	return 1;
      }
      asprintf(MLOpen_argv + 2, "\"%s\" -mathlink", Kernel);
      lp = MLOpen(sizeof(MLOpen_argv)/sizeof(char*), MLOpen_argv);
      if(lp == NULL) {
	return 2;
      }
    }
  } else if(lp == NULL) {
    return 3;
  }
  return 0;
}

char* evaluateInMathematica(const char* query)
{
  int l, pkt;
  const char* response;
  char* buffer;
  MLPutFunction(lp, "EvaluatePacket", 1);
  MLPutFunction(lp, "ToString", 1);
  MLPutFunction(lp, "ToExpression", 1);
  MLPutString(lp, query);
  MLEndPacket(lp);
  while( (pkt = MLNextPacket( lp), pkt) && pkt != RETURNPKT){
    MLNewPacket( lp);
  }
  MLGetString( lp, &response);
  l = strlen(response); 
  buffer = ciao_malloc(l * sizeof(char));
  strcpy(buffer, response);
  MLReleaseString(lp, response);
  return buffer;
}

void closelink( void)
{
	if( lp) MLClose( lp);
	lp = NULL;
	env = NULL;
}


/* int  main(int argc, char ** argv) { */
/*   if(mathematica_init() != 0) { */
/*     printf("Problems\n"); */
/*     return 1; */
/*   } */
/*   else { */
/*     printf("sum = %s\n", evaluateInMathematica("4+4")); */
/*   } */
/* } */
