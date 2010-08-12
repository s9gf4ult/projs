CPP=g++

VERSION=2.3

CFLAGS= -Wall -O -I. -DVERSION=\"$(VERSION)\"


DESTDIR=/usr/local/bin
MANDIR=/usr/local/share/man/man1

HEADERS=dbf.h
TARGETS=dbfsak
DUMPOBJ=dbf.o dbfsak.o
SQLOBJ=dbf.o

FILES=dbfsak.cpp \
	dbf.cpp \
	dbf.h \
	Makefile \
	dbfsak.1 \
	LICENSE \
	README \
	CREDITS \
	MANIFEST \
        CHANGES

.SUFFIXES:.cpp

.cpp.o	:
	$(CPP) -c $(CFLAGS) $<

all: $(HEADERS) $(TARGETS)

clean:
	rm -f *.o core $(TARGETS)

install:
	cp dbfsak $(DESTDIR)
	cp dbfsak.1 $(MANDIR)

cleanpkg:
	rm -rf ../dbfsak-$(VERSION)
	rm dbfsak-$(VERSION).tar.gz

package:
	mkdir ../dbfsak-$(VERSION)
	cp `cat MANIFEST` ../dbfsak-$(VERSION)
	cd .. ; tar czvf dbfsak/dbfsak-$(VERSION).tar.gz dbfsak-$(VERSION) ; cd dbfsak 

sourceforge:
	export CVS_RSH=ssh ; export CVS_ROOT=paulmfoster@cvs.dbfdump.sourceforge.net:/cvsroot/dbfdump ; cd .. ; cd dbfdump-$(VERSION) ; cvs import dbfdump qmsi start

# Main targets

dbfsak : $(DUMPOBJ)
	$(CPP) -o $@ $(DUMPOBJ)

# Dependencies

dbfsak : dbfsak.cpp

dbf : dbf.cpp

dbf.o : dbf.cpp

dbfsak.o : dbfsak.cpp

