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
   snprintf(prev, 256, "%2.5f  ", argnum);
   string returned(prev);
   free(prev);
   return returned;
 }



int main(int argc, char **argv) {
  typedef Matrix<TT> DMat;
  DMat rs1(1000,1000, (TT) 0);
  DMat rs2(1000,1000, (TT) 0);
  rs1.randomize();
  rs2.randomize();
  rs1 * rs2;
                
  return 0;
}
