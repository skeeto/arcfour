#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "arcfour.h"

size_t bsize = 4096;
size_t ivlen = 10;
int    mix_n = 20;

void print_err (char *s)
{
  fprintf (stderr, "%s Quitting.\n", s);
  exit (EXIT_FAILURE);
}

void print_warn (char *s)
{
  fprintf (stderr, "%s\n", s);
}

int main (int argc, char **argv)
{
  (void) argc;
  (void) argv;
  (void) mix_n;
  
  keystream k;
  
  /* Get passphrase */
  char *passphrase = getpass ("Passphrase: ");
  size_t passlen = strlen (passphrase);
  passlen = (passlen < 256 - ivlen ? passlen : 256 - ivlen);

  /* Generate an IV */
  char *iv = (char *) malloc (ivlen);
  FILE *rand = fopen ("/dev/random", "r");
  if (rand == NULL)
    {
      print_warn ("Failed to open /dev/random.");
      /* Generate by some other method */
      /* XXX */
    }
  else
    {
      fread ((void *)iv, 10, 1, rand);
      fclose (rand);
    }
  
  /* Build the key */
  char *key = (char *) malloc (passlen + 10);
  memcpy (key + passlen, iv, ivlen);
  set_strkey (&k, key);

  /* Process input */
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
