#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "arcfour.h"

#define SWAP(a, b) if (a ^ b) {a ^= b; b ^= a; a ^= b;}

/* Initialize a keystream.  */
void init_keystream (keystream *k, int n)
{
  k->i = 0;
  k->j = 0;

  int i;
  for (i = 0; i < 256; i++)
    k->S[i] = i;

  int s;
  byte j = 0;
  for (s = 0; s < n; s++)
    for (i = 0; i < 256; i++)
      {
	j += k->S[i] + k->key[i % k->keylen];
	SWAP(k->S[i], k->S[j]);
      }
}

/* Retrieve some bytes from the keystream. */
void get_bytes (keystream *k, void *buffer, size_t bytes)
{
  size_t b;
  for (b = 0; b < bytes; b++)
    {
      k->j += k->S[++k->i];
      SWAP (k->S[k->i], k->S[k->j]);
      ((byte *)buffer)[b] = k->S[(k->S[k->i] + k->S[k->j]) & 0xFF];
    }
}

/* Same as get_bytes(), but doesn't output the bytes. Used for dumping
   insecure, initial, low entropy bytes. */
void dump_bytes (keystream *k, size_t bytes)
{
  size_t b;
  for (b = 0; b < bytes; b++)
    {
      k->j += k->S[++k->i];
      SWAP (k->S[k->i], k->S[k->j]);
    }
}

/* Encrypt a buffer using the given keystream. */
void arc4_crypt (keystream *k, void *buffer, size_t bytes)
{
  size_t chunk_max = 1024;
  byte chunk[chunk_max];

  size_t i, j;
  for (i = 0; i < bytes; i += chunk_max)
    {
      size_t csize = (bytes - i < chunk_max) ? (bytes - i) : chunk_max;
      get_bytes (k, (void *)(&chunk[0]), csize);
      for (j = 0; j < csize; j++)
	((byte *)buffer)[i+j] ^= chunk[j];
    }
}

/* Destroy the key stored in the keystream struct. */
void clear_key (keystream *k)
{
  memset (&k->key[0], 0, 256);
  k->keylen = 0;
}

/* Destroy the key stored in the keystream struct. */
void clear_state (keystream *k)
{
  memset (&k->S[0], 0, 256);
  k->i = 0;
  k->j = 0;
}

/* Destroy the key stored in the keystream struct. */
void clear_keystream (keystream *k)
{
  clear_key (k);
  clear_state (k);
}

/* Set the key. */
void set_key (keystream *k, void *key, size_t size)
{
  k->keylen = size;
  if (k->keylen > 256)
    k->keylen = 256;
  memcpy (&k->key[0], (byte *)key, k->keylen);
  init_keystream (k, 1);
}

/* Set the key as a given string. */
void set_strkey (keystream *k, char *str)
{
  k->keylen = strlen(str);
  if (k->keylen > 256)
    k->keylen = 256;
  memcpy (&k->key[0], str, k->keylen);
  init_keystream (k, 1);
}

/* Set the key from a hexadecimal value from a string. */
void set_hexkey (keystream *k, char *str)
{
  k->keylen = strlen(str) / 2;
  if (k->keylen > 256)
    k->keylen = 256;

  short i;
  for (i = 0; i < k->keylen; i++)
    {
      unsigned int r;
      scanf(str + i*2, "%2x", &r);
      k->key[i] = r;
    }

  init_keystream (k, 1);
}
