unt.py:
	pyuic4 -o unt.py untitled.ui

windist:unt.py
	rm -rf win
	mkdir win
	unix2dos -n unt.py win/unt.py
	unix2dos -n aa.py win/aa.py
	zip -r win.zip win

unixdist:unt.py
	rm -rf svod
	mkdir svod
	cp aa.py unt.py svod
	tar -cjf svod.tzr.bz2 svod

clean:
	rm unt.py
	rm -rf svod
	rm -rf svod.tar.bz2
	rm -rf win
	rm -rf win.zip