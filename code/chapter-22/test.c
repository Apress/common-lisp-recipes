#include <ecl/ecl.h>

int main (int argc, char **argv) {
  extern void init_mylib(cl_object);
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_mylib);
  cl_eval(c_string_to_object("(hello)"));
  cl_shutdown();
  return 0;
}
