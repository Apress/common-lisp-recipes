double sum (double *arr, int size) {
  int i;
  double result = 0.0;
  for (i = 0; i < size; i++) {
    result += arr[i];
  }
  return result;
}

double set_arr (double *arr, int index, double new_value) {
  arr[index] = new_value;
  return new_value;
}
