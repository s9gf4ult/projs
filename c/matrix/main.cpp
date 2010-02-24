#include "matrix.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

string doubleshow(T argnum)
{
  char *prev = (char *)calloc(1,256);
  snprintf(prev, 256, "%2.5f  ", argnum);
  string returned(prev);
  free(prev);
  return returned;
}

T mula(T param)
{
  return param * sin(param);
}



int main(int argc, char **argv) {
  Matrix themat(2,2, 0);
  Matrix the2mat(2,2, 1);
  Matrix ones(2);
  cout << themat.mulate(the2mat).show(&doubleshow) << endl;
  cout << the2mat.mulate(19).show(&doubleshow) << endl;
  cout << ones.show(&doubleshow) << endl;
  cout << the2mat.mulate(ones).show(&doubleshow) << endl;
  cout << the2mat.map(&mula).show(&doubleshow) << endl;
  return 0;
}
