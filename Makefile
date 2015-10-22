all: pfkeyport

capability: pfkeyport
	sudo setcap cap_net_admin=ep pfkeyport

pfkeyport: pfkeyport.c
	gcc -Wall -pedantic -o pfkeyport pfkeyport.c -lev -lcap

clean:
	rm -f pfkeyport
