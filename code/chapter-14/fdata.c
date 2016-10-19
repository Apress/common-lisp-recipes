#include <stdio.h>

int main (int argc, char* argv[]) {
  double x = 42.5E9;
  double y = -.89E-12;

  FILE* out = fopen("/tmp/data", "w");

  fwrite(&x, sizeof(x), 1, out);
  fwrite(&y, sizeof(y), 1, out);

  fclose(out);
}
