/* ARC4 (Alleged RC4) implementation
 * 
 * Due to flaws discovered in the RC4 keystream, it is advised to drop
 * the first 768 bytes (3072 for the paranoid) from the keystream,
 * which is what dump_bytes() is for.
 */
#ifndef ARC4_H
#define ARC4_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char byte;
typedef struct keystream
{
  byte  key[256];
  short keylen;
  byte  S[256];
  byte  i, j;
} keystream;

/* keystream functions */
void init_keystream (keystream *k);
void get_bytes (keystream *k, void *buffer, size_t bytes);
void dump_bytes (keystream *k, size_t bytes);
void arc4_crypt (keystream *k, void *buffer, size_t bytes);
void clear_key (keystream *k);
void clear_state (keystream *k);
void clear_keystream (keystream *k);

/* Key setting functions */
void set_key (keystream *k, void *key, size_t size);
void set_strkey (keystream *k, char *str);
void set_hexkey (keystream *k, char *str);

#endif /* ARC4_H */
