#include "matrix.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define TT double
typedef Matrix<TT> DMat;

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

void passit(DMat *const arg){
  cout << arg->show(&TTshow) << endl;
  arg->set(0, 0, 1);
  cout << arg->show(&TTshow) << endl;
  return;
}



int main(int argc, char **argv) {
  TT arr[] = {3,3,3,3};
  DMat arra(2,2,arr);
  cout << arra.show(&TTshow) << endl;
  passit(&arra);
  cout << arra.show(&TTshow) << endl;
                
  return 0;
}
