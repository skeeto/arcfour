#include <stdio.h>
#include <unistd.h>
#include "arcfour.h"
#include "ciphersaber.h"

size_t bsize   = 4096;		/* buffer size */
size_t ivlen   = 10;		/* Initialization Vector length */

char *cs_getpass (int confirm);
int cs_handle_stream (char *passphrase, char *iv, int num_mix,
		      FILE *instream, FILE *outstream);

/* Encrypt stream with ciphersaber. */
int cs_encrypt (char *passphrase, int num_mix,
		FILE *instream, FILE *outstream)
{
  if (passphrase == NULL)
    {
      passphrase = cs_getpass (1);
      if (passphrase == NULL)
	return CS_FAILURE;
    }

  /* Generate an IV */
  char *iv = (char *) malloc (ivlen);
  FILE *rand = fopen ("/dev/urandom", "r");
  if (rand == NULL)
    {
      /* Generate by some other method */
      /* XXX TODO */
    }
  else
    {
      fread ((void *) iv, 1, 10, rand);
      fclose (rand);
    }
  fwrite (iv, 1, ivlen, outstream);

  int ret = cs_handle_stream (passphrase, iv, num_mix,
			      instream, outstream);
  free (iv);
  return ret;
}

/* Decrypt stream with ciphersaber.  */
int cs_decrypt (char *passphrase, int num_mix,
		FILE *instream, FILE *outstream)
{
  if (passphrase == NULL)
    {
      passphrase = cs_getpass (0);
      if (passphrase == NULL)
	return CS_FAILURE;
    }

  /* Get IV */
  char *iv = (char *) malloc (ivlen);
  fread (iv, 1, ivlen, instream);

  int ret = cs_handle_stream (passphrase, iv, num_mix, instream, outstream);
  free (iv);
  return ret;
}

/* Get passphrase interactively from the user. Return NULL for
   failure. */
char *cs_getpass (int confirm)
{
  char *pass2 = getpass ("Passphrase: ");
  if (strlen(pass2) == 0)
    return NULL;
  while (confirm)
    {
      /* Ask again */
      char *pass1 = strdup(pass2);
      pass2 = getpass ("Again: ");
      if (strlen(pass2) == 0)
	{
	  free (pass1);
	  return NULL;
	}
      if (strcmp(pass1, pass2) != 0)
	{
	  free (pass1);
	  break;
	}

      /* Start over */
      fprintf (stderr, "Passphrases did not match. Try again.\n");
      free (pass1);
      pass2 = getpass ("Passphrase: ");
      if (strlen(pass2) == 0)
	return NULL;
    }

  return pass2;
}

/* En/decrypt a stream given the parameters. */
int cs_handle_stream (char *passphrase, char *iv, int num_mix,
		      FILE *instream, FILE *outstream)
{
  keystream k;

  size_t passlen = strlen (passphrase);
  passlen = (passlen < 256 - ivlen ? passlen : 256 - ivlen);

  /* Piece together the key */
  char *key = (char *) malloc (passlen + ivlen);
  memcpy (key, passphrase, passlen);
  memcpy (key + passlen, iv, ivlen);
  set_key (&k, key, passlen + ivlen);
  init_keystream (&k, num_mix);

  /* Destroy the passphrase (don't need it anymore) */
  memset (passphrase, 0, strlen (passphrase));
  memset (key, 0, passlen + ivlen);
  clear_key (&k);

  /* Process instream */
  byte *buffer, *ks;
  buffer = malloc (bsize);
  ks     = malloc (bsize);
  while (!feof (instream))
    {
      size_t i, in = fread (buffer, 1, bsize, instream);
      get_bytes (&k, (void *) ks, in);
      for (i = 0; i < in; i++)
	buffer[i] ^= ks[i];
      fwrite (buffer, in, 1, outstream);
    }

  clear_keystream (&k);
  return CS_SUCCESS;
}
