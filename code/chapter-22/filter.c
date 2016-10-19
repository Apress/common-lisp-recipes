#include <stdio.h>

int main (void) {
  int c;
  while ((c = getchar()) != EOF) {
    putchar(c);
    if (c == '\n')
      fflush(stdout);
    else
      putchar(c);
  }
  return 0;
}
