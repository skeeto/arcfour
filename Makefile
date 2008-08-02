CFLAGS = -g -W -Wall -O2
LDFLAGS =

all : arcfour

arcfour : main.o arcfour.o
	gcc $(LDFLAGS) $(CFLAGS) $^ -o $@

arc4.o : arcfour.c arcfour.h
main.o : main.c arcfour.h

.PHONY : clean

clean : 
	$(RM) arcfour main.o plot.o arcfour.o
