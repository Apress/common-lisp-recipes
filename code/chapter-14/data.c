#include <stdio.h>

struct foo {
  char tag;
  unsigned short x0;
  unsigned short y0;
  long size;
};

int main (int argc, char* argv[]) {
  static struct foo foo1 = {'x', 12, 24, -666};
  static struct foo foo2 = {'A', 42, 1, 1234567810};

  FILE* out = fopen("/tmp/data", "w");

  fwrite(&foo1, sizeof(struct foo), 1, out);
  fwrite(&foo2, sizeof(struct foo), 1, out);

  fclose(out);
}
