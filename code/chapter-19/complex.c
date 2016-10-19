struct complex {
  double real;
  double imag;
};

double magnitude_squared (struct complex *c) {
  return c->real * c->real + c->imag * c->imag;
}
