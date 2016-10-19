/*
 *  compile with:
 *  gcc -fpic -shared test.c -o test.so
 */

double power (double base, int exponent) {
  int i;
  double result = 1.0;

  for (i = 1; i <= exponent; i++)
    result *= base;
  return result;
}
