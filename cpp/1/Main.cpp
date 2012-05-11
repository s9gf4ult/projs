#include <iostream>
#include "Main.hpp"

Sum *Sum::mappend(const Sum *a, const Sum *b) {
  Sum *ret = new Sum(a->elt + b->elt);
  return ret;
}


int main(int argc, char **argv) {
  Monoid *s = new Sum(10);
  Monoid *b = new Sum(20);
  
  
  std::cout << "hello";
  
}
