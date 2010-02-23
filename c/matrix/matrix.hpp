#include <vector>
#include <string>
using namespace std;

#define T double
typedef string ((*stringizer)(T*));
class Matrix {
private:
  vector<T> data;
  unsigned int x_size,y_size;

public:
  Matrix(unsigned int x,unsigned int y, T* empty);
  Matrix(unsigned int);
  bool set(unsigned int x,unsigned int y,T* param);
  bool get(unsigned int x,unsigned int y, T* param);
  unsigned int getxsize();
  unsigned int getysize();
  void resize(unsigned int, unsigned int, T *);
  string show(stringizer functrans);
  Matrix mulate(Matrix);
  Matrix mulate(T *);
};
  
  
