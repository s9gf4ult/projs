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
  char *prev = new char[256];
  snprintf(prev, 256, "%2.5f  ", argnum);
  string returned(prev);
  delete prev;
  return returned;
}

void passit(DMat *const arg) {
  cout << arg->show(&TTshow) << endl;
  arg->set(0, 0, 1);
  cout << arg->show(&TTshow) << endl;
  return;
}



int main(int argc, char **argv) {
  TT arr[] = {3,3,3,3};
  DMat *aa = new DMat(100, 100,(TT) 0);
  DMat *bb = new DMat(100, 100, (TT) 0);
  bb->randomize();
  aa->randomize();
  DMat *cc = aa->mulate(bb,1);
  DMat *ccc = aa->mulate(bb,2);
  if ((*cc) == (*ccc)) {
    cout << "Yes they a equal !!!!" << endl;
  } else {
    cout << "fuck" << endl;
  }
  delete aa, bb;
                
  return 0;
}
