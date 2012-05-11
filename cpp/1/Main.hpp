#ifndef MAIN_HPP
#define MAIN_HPP

class Monoid {
public:
  virtual Monoid *mappend(const Monoid *a, const Monoid *b) = 0;
};

class Sum : public Monoid {
  float elt;
public:
  Sum(float elt): elt(elt) {
  };

  virtual Sum *mappend(const Sum *a, const Sum *b);
};  

  

#endif
