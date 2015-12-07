/* Utility for fixing the size constant of the Ciao engine */
/* UPM CLIP laboratory - by Oscar Portela Arjona */

#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
  int j, i;
  struct stat data;
  char size[8];
  char string[] = "This emulator executable has a size of N";
  FILE *fd;

  if (stat(argv[1],&data) == -1) {
    printf("Usage: %s <file>\n",argv[0]);
    return -1;
  }

  if (data.st_size >= 10000000) {
    printf("Error: Size exceeding 7 digits\n");
    return -1;
  }

  if ((fd = fopen(argv[1],"r+b")) == NULL) {
    printf("Error: Unable to open %s for reading and writing",argv[0]);
    return -1;
  }

  sprintf(size,"%7li",(long)data.st_size);
  i = 0;
  for (j = getc(fd); j != EOF; j = getc(fd)) {
    if (j == string[i]) i++; else i = 0;
    if (string[i] == 'N') {
      /* Required for correct execution on SunOS */
      fseek(fd,0,SEEK_CUR);
      fwrite(size,1,7,fd);
      /* Required for correct execution on SunOS */
      fflush(fd);      
    } 
  }
  fclose(fd);
  return 0;
}
