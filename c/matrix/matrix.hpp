#include <vector>
#include <string>
using namespace std;

#define T double
class Matrix {
private:
  vector<T> data;
  unsigned int x_size,y_size;

public:
  typedef string ((*stringizer)(T));
  typedef T ((*mapper)(T));
  Matrix(unsigned int x,unsigned int y, T empty);
  Matrix(unsigned int);
  Matrix(unsigned int, unsigned int, vector<T>);
  void set(unsigned int x,unsigned int y,T param);
  T get(unsigned int x,unsigned int y);
  unsigned int getxsize();
  unsigned int getysize();
  void resize(unsigned int, unsigned int, T *);
  string show(stringizer functrans);
  Matrix mulate(Matrix);
  Matrix mulate(T);
  Matrix map(mapper);
};
  
  
