#CPP:=llvm-g++
CPP:=g++

release:main.cpp matrix.hpp
	$(CPP) -O3 -lm -lrt -lpthread -DPRINT_CLOCK main.cpp -o release

debug:main.cpp matrix.hpp
	$(CPP) -O0 -DDEBUG -lm -lpthread main.cpp -o debug

distr:Makefile main.cpp matrix.hpp README
	mkdir testmat
	cp Makefile main.cpp matrix.hpp README testmat
	tar -cjf testmat.tar.bz2 testmat
	rm -rf testmat
