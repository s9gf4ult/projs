all:main

matrix.o:matrix.cpp
	g++ -DDEBUG matrix.cpp -c -o matrix.o

main.o:main.cpp
	g++ main.cpp -c -o main.o

main:main.o matrix.o
	g++ main.o matrix.o -o main