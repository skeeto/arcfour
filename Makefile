.POSIX:
CC     = c99
CFLAGS = -Wall -Wextra -g3 -Og -march=native

all: rc4dump ciphersaber

rc4dump: rc4dump.c rc4.h
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ rc4dump.c $(LDLIBS)

ciphersaber: ciphersaber.c rc4.h
	$(CC) $(LDFLAGS) $(CFLAGS) ciphersaber.c -o $@ $(LDLIBS)

clean:
	rm -f rc4dump ciphersaber
