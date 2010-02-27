all:main


main.o:main.cpp matrix.hpp
	g++ main.cpp -O0  -ggdb -c -o main.o

main:main.o
	g++ main.o matrix.o -O0 -ggdb -o main

clean:
	rm *.o
	rm main