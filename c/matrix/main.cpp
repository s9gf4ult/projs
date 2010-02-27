#include "matrix.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define TT double

TT mula(TT param)
{
  return param * sin(param);
}

string TTshow(TT argnum)
{
  char *prev = (char *)calloc(1,256);
  snprintf(prev, 256, "%4.5f  ", argnum);
  string returned(prev);
  free(prev);
  return returned;
}


int main(int argc, char **argv) {
  TT ara[] = {2, 3,
             3, 2};
  TT rar[] = {-2, 14,
              1,  -1};
  TT rjrj [] = {2, 33,
                0, -10};
  typedef Matrix<TT> DMat;
  //  cout << DMat::Matrix(2,2,ara).mulate(DMat::Matrix(2,2,rar)).mulate(DMat::Matrix(2,2,rjrj)).show(&TTshow) << endl;
  cout <<  ((DMat::Matrix(2,2,ara) * 2 * DMat::Matrix(2,2,rjrj) * 0.1) + 10).show(&TTshow) <<endl;
                
  return 0;
}
