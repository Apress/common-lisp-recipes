/*
 *  compile with:
 *  gcc -fpic -shared test2.c -o test2.so
 */

double power(int exponent) {
  int i;
  double result = 1.0;
  for (i = 1; i <= exponent; i++)
    result *= 2.0;
  return result;
}
