#include <vector>
#include <string>
using namespace std;

#define T double
typedef string* ((*stringizer)(T*));
class Matrix {
private:
  vector<T> data;
  unsigned int x_size,y_size;

public:
  Matrix(unsigned int x,unsigned int y, T* empty);
  bool set(unsigned int x,unsigned int y,T* param);
  bool get(unsigned int x,unsigned int y, T* param);
  string *show(stringizer functrans);
};
  
  
