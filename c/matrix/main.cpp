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
  // Matrix themat((unsigned int)2,(unsigned int)2,(T) 0);
  // Matrix the2mat(2,2, 1);
  // Matrix ones(2);
  // cout << themat.mulate(the2mat).show(&doubleshow) << endl;
  // cout << the2mat.mulate(19).show(&doubleshow) << endl;
  // cout << ones.show(&doubleshow) << endl;
  // cout << the2mat.mulate(ones).show(&doubleshow) << endl;
  // cout << the2mat.map(&mula).show(&doubleshow) << endl;
  T arr[] = {1, 2, 33,
             23, 44, 55,
             55, 32, 99};
  T arr2[] = {2, 2, 2,
              3, 3, 3,
              4, 4, 4};
  Matrix newone(3,3,arr);
  Matrix newtwo(3,3,arr2);
  cout << newone.mulate(newtwo).show(&doubleshow) << endl;
                
  return 0;
}
