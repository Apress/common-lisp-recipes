void test (int n, void (*func)(int)) {
  (*func)(n * n);
}
