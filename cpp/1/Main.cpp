#include <iostream>
#include "Main.hpp"

void ConcreteVisitor1::visit(ConcreteElement1 *elt) {
  std::cout << "visitor 1 visit element 1" << std::endl;
}

void ConcreteVisitor1::visit(ConcreteElement2 *elt) {
  std::cout << "vistor 1 visit element 2" << std::endl;
}

void ConcreteVisitor2::visit(ConcreteElement1 *elt) {
  std::cout << "visior 2 visit element 1" << std::endl;
}

void ConcreteVisitor2::visit(ConcreteElement2 *elt) {
  std::cout << "visitor 2 visit element 2" << std::endl;
};

void ConcreteElement1::accept(Visitor *vst) {
  vst->visit(this);
}

void ConcreteElement2::accept(Visitor *vst) {
  vst->visit(this);
}

void ConcreteVisitor3::visit(ConcreteElement3 *elt) {
  std::cout << "visitor 3 visit element 3" << std::endl;
}

void ConcreteElement3::accept(Visitor *vst) {
  vst->visit(this);
}

void ConcreteElement3::accept(ConcreteVisitor3 *vst) {
  vst->visit(this);
}


int main(int argc, char **argv) {

  Visitor *vs1 = new ConcreteVisitor1();
  Visitor *vs2 = new ConcreteVisitor2();
  ConcreteVisitor3 *vs3 = new ConcreteVisitor3();

  ConcreteElement1 *elt1 = new ConcreteElement1();
  ConcreteElement2 *elt2 = new ConcreteElement2();
  ConcreteElement3 *elt3 = new ConcreteElement3();

  elt1->accept(vs1);
  elt1->accept(vs2);
  elt2->accept(vs1);
  elt2->accept(vs2);
  elt3->accept(vs3);
  elt1->accept(vs3);

  delete vs1, vs2, vs3, elt1, elt2, elt3;
}
