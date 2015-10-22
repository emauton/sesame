pfkeyport: pfkeyport.c
	gcc -Wall -pedantic -o pfkeyport pfkeyport.c -lev

clean:
	rm -f pfkeyport
