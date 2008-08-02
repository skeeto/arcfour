#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include "ciphersaber.h"

int    num_mix = 1;		/* no. of key schedules (CipherSaber-2) */
char   mode    = ' ';		/* de/encrpytion */
char  *progname;		/* Program name. */
char  *version = "1.0.0";	/* Version info */

/* Print error message and quit. */
void print_err (char *s)
{
  fprintf (stderr, "%s Quitting.\n", s);
  exit (EXIT_FAILURE);
}

/* Print version information. */
void print_version ()
{
  printf ("cs2, version %s.\n", version);
  printf ("Copyright (C) 2008 Chris Wellons\n");
  printf ("This is free software; see the source code ");
  printf ("for copying conditions.\n");
  printf ("There is ABSOLUTELY NO WARRANTY; not even for ");
  printf ("MERCHANTIBILITY or\nFITNESS FOR A PARTICULAR PURPOSE.\n\n");
}

/* Print usage information. */
void print_usage(int ret)
{
  printf ("Usage: %s [options]\n\nOptions:\n\n", progname);
  printf ("\t-d           Decrypt\n");
  printf ("\t-e           Encrypt\n");
  printf ("\t-n           CipherSaber-2 mix number\n");
  printf ("\t-v           Print version info\n");
  printf ("\t-h           Print this usage text\n");
  exit (ret);
}

int main (int argc, char **argv)
{
  progname = argv[0];

  int c;
  while ((c = getopt (argc, argv, "den:hv")) != -1)
    switch (c)
      {
      case 'e':		/* encrypt */
	mode = 'e';
	break;
      case 'd':		/* decrypt */
	mode = 'd';
	break;
      case 'n':		/* number of mixes */
	num_mix = atoi (optarg);
	if (num_mix < 1)
	  print_err ("Number of mixes must be > 1.");
	break;
      case 'h':		/* print help */
	print_usage (EXIT_SUCCESS);
	break;
      case 'v':		/* version */
	print_version (EXIT_SUCCESS);
	break;
      case '?':		/* bad argument */
	print_usage (EXIT_FAILURE);
      }

  if (mode == ' ')
    print_err ("Must select en/decrypt mode using -d or -e.");

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
