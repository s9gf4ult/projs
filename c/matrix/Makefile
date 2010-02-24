all:main

matrix.o:matrix.cpp
	g++ -DDEBUG -ggdb matrix.cpp -c -o matrix.o

main.o:main.cpp
	g++ main.cpp -ggdb -c -o main.o

main:main.o matrix.o
	g++ main.o matrix.o -ggdb -o main

clean:
	rm *.o
	rm main