#include <stdio.h>
#include <stdlib.h>

extern int our_code_starts_here() asm("our_code_starts_here");

int print(int val) {
  if (val == 0xFFFFFFFF)
    printf("True");
  else if (val == 0x7FFFFFFF)
    printf("false");
  else
    printf("%d", val >> 1);
  return val;
}

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}
