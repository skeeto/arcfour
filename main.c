/*
 * ARC4 (Alleged RC4) Command Line Filter
 *
 * TODO: getopt() et al
 */
#include <stdio.h>
#include <stdlib.h>
#include "arcfour.h"

/* Options */
size_t bsize = 4096;

int main (int argc, char **argv)
{
  (void) argc;
  (void) argv;
  
  keystream k;
  
  char *key   = "Wiki";
  set_strkey (&k, key);  
  
  byte *buffer, *ks;
  buffer = malloc (bsize);
  ks     = malloc (bsize);
  
  while (!feof(stdin))
    {
      size_t i, in = fread (buffer, 1, bsize, stdin);
      get_bytes (&k, (void *)ks, in);
      for (i = 0; i < in; i++)
	printf("%c", (buffer[i] ^ ks[i]));
    }
  
  return EXIT_SUCCESS;
}
