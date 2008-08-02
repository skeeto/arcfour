CFLAGS = -g -W -Wall -O2
LDFLAGS =

all : arcfour cs2

arcfour : main.o arcfour.o
	gcc $(LDFLAGS) $(CFLAGS) $^ -o $@

cs2 : cs2.o ciphersaber.o arcfour.o

arc4.o        : arcfour.c arcfour.h
main.o        : main.c arcfour.h
sc2.o         : sc2.c
ciphersaber.o : ciphersaber.c ciphersaber.h arcfour.h

.PHONY : clean

clean : 
	$(RM) arcfour cs2 cs2.o ciphersaber.o main.o arcfour.o
