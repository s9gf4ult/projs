#include "matrix.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>

string doubleshow(T *argnum)
{
  char *prev = (char *)calloc(1,256);
  snprintf(prev, 256, "%f  ", *argnum);
  string returned(prev);
  free(prev);
  return returned;
}


int main(int argc, char **argv) {
  T doubnull = 1;
  Matrix themat(2,2,&doubnull);
  cout << themat.show(&doubleshow) << endl;
  return 0;
}
