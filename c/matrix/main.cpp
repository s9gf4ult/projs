#include "matrix.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

T mula(T param)
{
  return param * sin(param);
}

string doubleshow(T argnum)
{
  char *prev = (char *)calloc(1,256);
  snprintf(prev, 256, "%2.5f  ", argnum);
  string returned(prev);
  free(prev);
  return returned;
}


int main(int argc, char **argv) {
  T arr[] = {1, 2, 33,
             23, 44, 55,
             55, 32, 99};
  T arr2[] = {2, 2, 2,
              3, 3, 3,
              4, 4, 4};
  Matrix newone(3,3,arr);
  Matrix newtwo(3,3,arr2);
  T ara[] = {2, 3,
             3, 2};
  T rar[] = {-2, 14,
             1,  -1};
  cout << Matrix::Matrix(2,2,ara).mulate(Matrix::Matrix(2,2,rar)).show(&doubleshow) << endl;
  cout << newone.mulate(newtwo).show(&doubleshow) << endl;
                
  return 0;
}
