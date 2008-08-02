#ifndef CIPHERSABER_H
#define CIPHERSABER_H

#include <stdio.h>
#include "arcfour.h"

#define CS_FAILURE 1
#define CS_SUCCESS 0

/* Passing NULL to passphrase will cause it to ask the user
   interactively for a passphrase. */
int cs_encrypt (char *passphrase, int num_mix,
		FILE *instream, FILE *outstream);
int cs_decrypt (char *passphrase, int num_mix,
		FILE *instream, FILE *outstream);

#endif /* CIPHERSABER_H */
