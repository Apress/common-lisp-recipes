#include <stdio.h>

int main () {
  extern long toLispTime(int, int, int);
  printf("Date 1940-12-21 is Lisp time %ld.\n",
         toLispTime(1940, 12, 21));
  return 0;
}
