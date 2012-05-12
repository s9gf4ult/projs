#ifndef MAIN_HPP
#define MAIN_HPP

class Visitor;
class ConcreteVisitor1;
class ConcreteVisitor2;
class ConcreteVisitor3;
class Element;
class ConcreteElement1;
class ConcreteElement2;
class ConcreteElement3;

class Visitor {
public:
  virtual void visit(ConcreteElement1 *elt) = 0;
  virtual void visit(ConcreteElement2 *elt) = 0;
};

class ConcreteVisitor1 : public Visitor {
public:
  virtual void visit(ConcreteElement1 *elt);
  virtual void visit(ConcreteElement2 *elt);
};

class ConcreteVisitor2 : public Visitor {
public:
  virtual void visit(ConcreteElement1 *elt);
  virtual void visit(ConcreteElement2 *elt);
};

class ConcreteVisitor3 : public ConcreteVisitor2{
public:
  virtual void visit(ConcreteElement2 *elt);
  virtual void visit(ConcreteElement3 *elt);
};

class Element {
public:
  virtual void accept(Visitor *vst) = 0;
};
  
class ConcreteElement1 : public Element {
public:
  virtual void accept(Visitor *vst);
};

class ConcreteElement2 : public Element {
public:
  virtual void accept(Visitor *vst);
};

class ConcreteElement3 : public Element {
public:
  virtual void accept(Visitor *vst);
  virtual void accept(ConcreteVisitor3 *vst);
};
  

#endif
