union result_union {
  double rval;
  unsigned long ival;
};

struct result_struct {
  char exact;
  union result_union val;
};

void factorial (int n, struct result_struct *r) {
  int i;
  // assumes long has 64 bits
  if (n < 21) {
    unsigned long result = 1;
    for (i = 1; i <= n; i++)
      result *= i;
    r->exact = 1;
    r->val.ival = result;
  } else {
    double result = 1.0;
    for (i = 1; i <= n; i++)
      result *= i;
    r->exact = 0;
    r->val.rval = result;
  }
}
