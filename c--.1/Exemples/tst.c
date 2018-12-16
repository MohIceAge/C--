#ifndef MCC
#include <stdio.h>
#include <stdlib.h>
#endif

int main (int argc, char **argv)
{
  int i;
  i=1;
  if(1){
    int i;
    i=2;
  }
  printf("i=%d\n", i);
  return 0;
}
