all:main

matrix.o:matrix.cpp
	g++ -DDEBUG -O0 -ggdb matrix.cpp -c -o matrix.o

main.o:main.cpp
	g++ main.cpp -O0  -ggdb -c -o main.o

main:main.o matrix.o
	g++ main.o matrix.o -O0 -ggdb -o main

clean:
	rm *.o
	rm main