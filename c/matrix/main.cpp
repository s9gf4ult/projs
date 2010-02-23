#include "matrix.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>

string doubleshow(T *argnum)
{
  char *prev = (char *)calloc(1,256);
  snprintf(prev, 256, "%2.5f  ", *argnum);
  string returned(prev);
  free(prev);
  return returned;
}


int main(int argc, char **argv) {
  T doubnull = 0;
  T doub1 = 1;
  T mula = 12;
  Matrix themat(2,2,&doubnull);
  Matrix the2mat(2,2,&doub1);
  Matrix ones(2);
  cout << themat.mulate(the2mat).show(&doubleshow) << endl;
  cout << the2mat.mulate(&mula).show(&doubleshow) << endl;
  cout << the2mat.mulate(ones).show(&doubleshow) << endl;
  return 0;
}
