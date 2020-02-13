#include <stdio.h>
#include <stdlib.h>

extern int our_code_starts_here() asm("our_code_starts_here");

int print(int val) {
  if (val == 0xFFFFFFFF)
    printf("true\n");
  else if (val == 0x7FFFFFFF)
    printf("false\n");
  else
    printf("%d\n", val >> 1);
  return val;
}

int error(int val){
  if(val == 0)
    fprintf(stderr, "Error: expected a number");
  else if(val == 1)
    fprintf(stderr, "Error: expected a boolean");
  else if(val == 2)
    fprintf(stderr, "Error: arithmetic overflow");
  return 0;
}

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}
