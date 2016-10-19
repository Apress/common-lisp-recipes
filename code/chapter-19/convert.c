void convert (unsigned char in[], unsigned int out[]) {
  int i = 0;
  while (in[i]) {
    out[i] = in[i];
    i++;
  }
  out[i] = 0;
}
