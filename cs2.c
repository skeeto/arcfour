#include <stdio.h>
#include <stdlib.h>
#include "ciphersaber.h"

int    num_mix = 1;		/* no. of key schedules (CipherSaber-2) */
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
  if (argc > 1 && strcmp(argv[1], "-d") == 0)
    mode = 'd';

  /* Run CipherSaber */
  int ret;
  if (mode == 'e')
    ret = cs_encrypt (NULL, num_mix, stdin, stdout);
  else
    ret = cs_decrypt (NULL, num_mix, stdin, stdout);
  if (ret == CS_FAILURE)
    print_err ("Failed to de/encrypt.");

  return EXIT_SUCCESS;
}
