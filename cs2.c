#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "arcfour.h"

size_t bsize   = 4096;		/* buffer size */
size_t ivlen   = 10;		/* Initialization Vector length */
int    mix_n   = 20;		/* no. of key schedules (CipherSaber-2) */
char   mode    = 'e';		/* de/encrpytion */
int    verbose = 1;		/* verbosity */

/* Print error message and quit. */
void print_err (char *s)
{
  fprintf (stderr, "%s Quitting.\n", s);
  exit (EXIT_FAILURE);
}

/* Print warning message, but don't quit. */
void print_warn (char *s)
{
  if (verbose)
    fprintf (stderr, "%s\n", s);
}

int main (int argc, char **argv)
{
  keystream k;
  (void) mix_n;

  if (argc > 1 && strcmp(argv[1], "-d") == 0)
    mode = 'd';

  /* Handle IV */
  char *iv = (char *) malloc (ivlen);
  if (mode == 'e')
    {
      /* Generate an IV */
      FILE *rand = fopen ("/dev/urandom", "r");
      if (rand == NULL)
	{
	  print_warn ("Failed to open /dev/urandom.");
	  /* Generate by some other method */
	  /* XXX */
	}
      else
	{
	  fread ((void *) iv, 1, 10, rand);
	  fclose (rand);
	}
      fwrite (iv, 1, ivlen, stdout);
    }
  else
    {
      /* Read the IV */
      fread (iv, 1, ivlen, stdin);
    }

  /* Get passphrase */
  char *passphrase = getpass ("Passphrase: ");
  size_t passlen = strlen (passphrase);
  passlen = (passlen < 256 - ivlen ? passlen : 256 - ivlen);
  fprintf(stderr, "%s  %d\n", passphrase, passlen);

  /* Build the key */
  char *key = (char *) malloc (passlen + 10);
  memcpy (key, passphrase, passlen);
  memcpy (key + passlen, iv, ivlen);
  set_key (&k, key, passlen + ivlen);

  /* Destroy the passphrase */
  memset (passphrase, 0, strlen (passphrase));
  memset (key, 0, passlen + 10);

  /* Process input */
  byte *buffer, *ks;
  buffer = malloc (bsize);
  ks     = malloc (bsize);
  while (!feof (stdin))
    {
      size_t i, in = fread (buffer, 1, bsize, stdin);
      get_bytes (&k, (void *) ks, in);
      for (i = 0; i < in; i++)
	buffer[i] ^= ks[i];
      fwrite (buffer, in, 1, stdout);
    }

  return EXIT_SUCCESS;
}