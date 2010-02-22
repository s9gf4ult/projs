#include "matrix.hpp"

using namespace std;

Matrix::Matrix(unsigned int x,unsigned int y, T* empty)
{
  x_size=x;
  y_size=y;
  if ((x == 0) || (y == 0)) return;
  for (int ii=0; ii < (x * y); ii++)
    data.push_back(*empty);
}

bool Matrix::set(unsigned int x,unsigned int y,T* param)
{
  // try
  //   {
		data[x + (y * x_size)] = *param;
		return true;
  //   }
  // catch
  //   return false;
}

bool Matrix::get(unsigned int x,unsigned int y, T* param)
{
  // try
  // {
  *param = data[x + (y * x_size)];
  return true;
  // }
  // catch
  //  return false;
}

string Matrix::show(stringizer functrans)
{
  if ((x_size * y_size) != 0) {
    string resultString;
    for (unsigned int yy=0; yy < y_size; yy++)
      for (unsigned int xx=0; xx < x_size; xx++) {
        string localres = functrans(&data[xx + (yy * x_size)]);
        resultString.append(localres);
        if ((x_size - 1) == xx)
          resultString += "\n";
      }
    return resultString;
  }
  return NULL;
}



					


          





