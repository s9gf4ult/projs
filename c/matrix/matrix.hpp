#ifndef MATRIX_H
#define MATRIX_H

#include <vector>
#include <string>
#include <cstdlib>
#include <limits>
#include <typeinfo>
#include <iostream>
#include <sys/types.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
using namespace std;

template<class T> class Matrix {
private:
  vector<T> data;
  unsigned int x_size,y_size;
  
  struct ThreadMulate {
    unsigned int from,to;
    Matrix *saveto;
    Matrix *getfrom;
    ThreadMulate(Matrix *s, Matrix *g)
    {
      saveto = s;
      getfrom = g;
    }
    ThreadMulate(unsigned int f, unsigned int t, Matrix *s, Matrix *g)
    {
      from = f;
      to = t;
      saveto = s;
      getfrom = g;
    }
  };

  void *threadMulate(void *arg)
  {
    ThreadMulate *marg = (ThreadMulate *) arg;
    unsigned int tox = marg->getfrom->getxsize();
    for (unsigned int ypos = marg->from; ypos < marg->to; ypos++) {
      for (unsigned int xpos = 0; xpos < tox; xpos++) {
        T summator = 0;
        for (unsigned int mpos = 0; mpos < x_size; mpos++) {
          summator += get(mpos, ypos) * marg->getfrom->get(xpos, mpos);
        }
        marg->saveto->set(xpos, ypos, summator);
      }
    }
    return NULL;
  }
  
    

public:
  
  typedef string ((*stringizer)(T));
  typedef T ((*mapper)(T));
  
  ~Matrix()
  {
    data.clear();
  }

  Matrix(unsigned int x,unsigned int y, T empty)
  {
    x_size=x;
    y_size=y;
    if (x * y) 
      data.resize((x * y), empty);
  }

  Matrix(unsigned int size)
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

  Matrix(unsigned int x, unsigned int y, T *array)
  {
    x_size=x;
    y_size=y;
    if (x * y) {
      data.resize((x * y), 0);
      for (unsigned int ii = 0; ii < (x * y); ii++)
        data[ii] = array[ii];
    }
  }

  void set(unsigned int x,unsigned int y,T param)
  {
    if ((x < x_size) && (y < y_size)) {
      data[x + (y * x_size)] = param;
      return;
    } else {
#ifdef DEBUG
      cerr <<"set received bad params: x=" << x << "  y=" << y << endl;
#endif
      return;
    }
  }

  T get(unsigned int x,unsigned int y)
  {
    if ((x < x_size) && (y < y_size)) {
      return data[x + (y * x_size)];
    } else {
#ifdef DEBUG
      cerr << "get bad params:x=" << x << "  y=" << y << endl;
#endif
      return NULL;
    }
  }

  string show(stringizer functrans)
  {
    if ((x_size * y_size) != 0) {
      string resultString;
      for (unsigned int yy=0; yy < y_size; yy++)
        for (unsigned int xx=0; xx < x_size; xx++) {
          string localres = functrans(get(xx,yy));
          resultString.append(localres);
          if ((x_size - 1) == xx)
            resultString += "\n";
        }
      return resultString;
    }
    return "Empty";
  }
  
  Matrix *mulate(Matrix *mulator, unsigned int threads) 
  {
#ifdef DEBUG
    cout << "mulate by Matrix" << endl;
#endif
    if ((x_size * y_size) != 0 && (mulator->getysize() * mulator->getxsize()) != 0 && x_size == mulator->getysize()) {
      unsigned int maxthreads = max((unsigned int) 1, min(threads, y_size));
      unsigned int step = y_size / maxthreads;
      Matrix *result = new Matrix(mulator->getxsize(), y_size, (T)0);
      std::vector<ThreadMulate> thargs(maxthreads, ThreadMulate(result, mulator));
      for (unsigned int cnt = 0; cnt < maxthreads; cnt++) {
        thargs[cnt].from = cnt * step;
        thargs[cnt].to = (cnt + 1) * step;
      }
      thargs[maxthreads - 1].to = y_size; // это в том случае если step не будет кратен количеству потоков
      // теперь создаем сами потоки
      std::vector<pthread_t> pthreads(maxthreads, 0);
      bool thats_ok = true;
      for (unsigned int cnt = 0; cnt < maxthreads; cnt++) {
        pthread_t curth = pthreads[cnt];
        ThreadMulate curarg = thargs[cnt];
        if (pthread_create(&curth, NULL,(void * (*)(void *)) &threadMulate, (void *) &curarg)) { // TODO вывести threadMulate из класса
            thats_ok = false;
            break;
        }
        pthreads[cnt] = curth;
      }
      if (!thats_ok) {          // если чтото в создании потоков пошло не так надо убить все созданные потоки
        for (unsigned int cnt = 0; cnt < maxthreads; cnt++) {
          if (pthreads[cnt]) {
            pthread_kill(pthreads[cnt], 9);
          } else {
            break;
          }
        }
        thargs.clear();
        pthreads.clear();
        delete result;
        return NULL;
      }
      // все потоки сдесь должны быть запущены и работать надо их дождаться
      void * pthRet;
      for (unsigned int cnt = 0; cnt < maxthreads; cnt++) {
        pthread_join(pthreads[cnt], &pthRet);
      }
      // после отработки этого цикла все потоки должны завершить работу

      
      thargs.clear();
      pthreads.clear();
      return result;
      
    } else {
#ifdef DEBUG
      cerr << "Matrix::mulate(Matrix) incorrect operands or size" << endl
           << "   x_size = " << x_size << endl
           << "   y_size = " << y_size << endl
           << "   mulator.getxsize() = " << mulator->getxsize() << endl
           << "   mulator.getysize() = " << mulator->getysize() << endl;
#endif
      return NULL;
    }
  }

  bool operator== (Matrix comparator)
  {
    if ((y_size == comparator.getysize()) && (x_size == comparator.getxsize())) {
      for (unsigned int ypas = 0; ypas < y_size; ypas++) {
        for (unsigned int xpas = 0; xpas < x_size; xpas++) {
          if (get(xpas,ypas) != comparator.get(xpas,ypas)) {
            return false;
          }
        }
      }
      return true;
    }
    return false;
  }

  Matrix mulate(T mulator)
  {
#ifdef DEBUG
    cout << "mulate by T" << endl;
#endif
    if (x_size * y_size) {
      Matrix result(x_size, y_size,(T) 0);
      for (unsigned int ypas = 0; ypas < y_size; ypas++)
        for (unsigned int xpas = 0; xpas < x_size; xpas++) {
          result.set(xpas, ypas, get(xpas, ypas) * mulator);
        }
      return result;
    } else {
      Matrix nulmat(0);
      return nulmat;
    }
  }


  unsigned int getxsize()
  {
    return x_size;
  }

  unsigned int getysize()
  {
    return y_size;
  }
                                   
  void resize(unsigned int newx, unsigned int newy , T *nullelem)
  {
    data.resize((newx * newy), *nullelem);
  }
    

  Matrix map(mapper mapperfunc)
  {
    if (x_size * y_size) {
      Matrix result(x_size, y_size,(T) 0);
      for (unsigned int xx = 0; xx < x_size; xx++)
        for (unsigned int yy = 0; yy < y_size; yy++) {
          result.set(xx,yy,mapperfunc(get(xx,yy)));
        }
      return result;
    } else {
#ifdef DEBUG
      cerr << "map mapping matrix with illegal size x_size=" << x_size << "  y_size=" << y_size << endl;
#endif
      return NULL;
    }
  }

  Matrix operator* (Matrix &mulator)
  {
    return *(mulate(&mulator,1));
  }

  Matrix operator* (T &mulator)
  {
    return mulate(mulator);
  }

  Matrix operator+ (T &adder)
  {
    if (x_size*y_size) {
      Matrix result(x_size, y_size,(T) 0);
      for (unsigned int ypas = 0; ypas < y_size; ypas++) {
        for (unsigned int xpas = 0; xpas < x_size; xpas++) {
          T setter = get(xpas, ypas) + adder;
          result.set(xpas, ypas, setter);
        }
      }
      return result;
    } else {
#ifdef DEBUG
      cerr << "Matrix::operator+ (T) incorrect size of matrix" << endl;
#endif
      return NULL;
    }
  }

  Matrix operator+ (Matrix  &adder)
  {
    if ((x_size == adder.getxsize()) && (y_size == adder.getysize()) && (x_size * y_size)) {
      Matrix result(x_size, y_size, (T) 0);
      for (unsigned int ypas = 0; ypas < y_size; ypas++) {
        for (unsigned int xpas = 0; xpas < x_size; xpas++) {
          result.set(xpas, ypas, get(xpas, ypas) + adder.get(xpas, ypas));
        }
      }
      return result;
    } else {
#ifdef DEBUG
      cerr << "Matrix::operator+ (Matrix) incorrect matrix's size or size of operator" << endl
           << "     x_size = " << x_size << endl
           << "     y_size = " << y_size << endl;
#endif
      return NULL;
    }
  }

  void randomize()
  {
    if (x_size * y_size) {
      for (unsigned int ypas = 0; ypas < y_size; ypas++) {
        for (unsigned int xpas = 0; xpas < x_size; xpas++) {
          set(xpas,ypas, ((T) rand()) );
        }
      }
    }
    return;
  }
  
};
  
  
#endif
