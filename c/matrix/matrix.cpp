#include "matrix.hpp"
#include <iostream>

using namespace std;

Matrix::Matrix(unsigned int x,unsigned int y, T empty)
{
  x_size=x;
  y_size=y;
  if (x * y) 
    data.resize((x * y), empty);
}

Matrix::Matrix(unsigned int size)
{
#ifdef DEBUG
  cout << "construcor ones" << endl;
#endif
  x_size = size;
  y_size = size;
  if (size) {
    T nullelem = 0;
    data.resize((size * size), nullelem);
    for (unsigned int passer = 0; passer < size; passer++)
      //(passer + (passer * size))
      data[passer * (size + 1)] = 1;
  }
}

Matrix::Matrix(unsigned int x, unsigned int y, T *array)
{
  x_size=x;
  y_size=y;
  if (x * y) {
    data.resize((x * y), 0);
    for (unsigned int ii = 0; ii < (x * y); ii++)
      data[ii] = array[ii];
  }
}

void Matrix::set(unsigned int x,unsigned int y,T param)
{
  if ((x < x_size) && (y < y_size)) {
    data[x + (y * x_size)] = param;
    return;
  } else {
#ifdef DEBUG
    cerr <<"Matrix::set received bad params: x=" << x << "  y=" << y << endl;
#endif
    return;
  }
}

T Matrix::get(unsigned int x,unsigned int y)
{
  if ((x < x_size) && (y < y_size)) {
    return data[x + (y * x_size)];
  } else {
#ifdef DEBUG
    cerr << "Matrix::get bad params:x=" << x << "  y=" << y << endl;
#endif
    return NULL;
  }
}

string Matrix::show(stringizer functrans)
{
  if ((x_size * y_size) != 0) {
    string resultString;
    for (unsigned int yy=0; yy < y_size; yy++)
      for (unsigned int xx=0; xx < x_size; xx++) {
        string localres = functrans(data[xx + (yy * x_size)]);
        resultString.append(localres);
        if ((x_size - 1) == xx)
          resultString += "\n";
      }
    return resultString;
  }
  return "Empty";
}


Matrix Matrix::mulate(Matrix mulator)
{
#ifdef DEBUG
  cout << "mulate by Matrix" << endl;
#endif
  if ((x_size * y_size) != 0 && (mulator.x_size * mulator.y_size) != 0)
    if (x_size == mulator.getxsize()) {
      Matrix resultant(mulator.getxsize(), y_size,(T) 0);
      for (unsigned int y_passer = 0; y_passer < y_size; y_passer++)
        for (unsigned int x_passer = 0; x_passer < mulator.getxsize(); x_passer++) {
          T summator = 0;
          for (unsigned int middle_passer = 0; middle_passer < x_size; middle_passer++) {
            T getter = mulator.get(x_passer, middle_passer);
            summator += (getter * data[middle_passer + (y_passer * x_size)]);
          }
          resultant.set(x_passer, y_passer, summator);
        }
      return resultant;
    }
  Matrix nullmat(0);
  return nullmat;
}

Matrix Matrix::mulate(T mulator)
{
#ifdef DEBUG
  cout << "mulate by T" << endl;
#endif
  if (x_size * y_size) {
    Matrix result(x_size, y_size,(T) 0);
    for (unsigned int ypas = 0; ypas < y_size; ypas++)
      for (unsigned int xpas = 0; xpas < x_size; xpas++) {
        T setter = data[xpas * (ypas * x_size)] * (mulator);
        result.set(xpas, ypas, setter);
      }
    return result;
  } else {
    Matrix nulmat(0);
    return nulmat;
  }
}


unsigned int Matrix::getxsize()
{
  return x_size;
}

unsigned int Matrix::getysize()
{
  return y_size;
}

void Matrix::resize(unsigned int newx, unsigned int newy , T *nullelem)
{
  data.resize((newx * newy), *nullelem);
}
    

Matrix Matrix::map(mapper mapperfunc)
{
  if (x_size * y_size) {
    Matrix result(x_size, y_size,(T) 0);
    for (unsigned int xx = 0; xx < x_size; xx++)
      for (unsigned int yy = 0; yy < y_size; yy++) {
        T rc = data[xx + (yy * x_size)];
        result.set(xx,yy,mapperfunc(rc));
      }
    return result;
  } else {
#ifdef DEBUG
    cerr << "Matrix::map mapping matrix with illegal size x_size=" << x_size << "  y_size=" << y_size << endl;
#endif
    return NULL;
  }
}

    
