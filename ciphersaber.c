#define _POSIX_SOURCE 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <getopt.h>

#include "rc4.h"

#define IVLEN 10

static void
usage(void)
{
    fputs("ciphersaber <-D|-E> [-k file]\n", stderr);
    exit(EXIT_FAILURE);
}

/* Pump data from stdin to stdout via XOR with keystream */
static void
pump(struct rc4 *rc4)
{
    size_t z;
    static char input[4096];
    while (!feof(stdin) && (z = fread(input, 1, sizeof(input), stdin))) {
        static char output[sizeof(input)];
        rc4_rand(rc4, output, z);
        for (size_t i = 0; i < z; i++)
            output[i] ^= input[i];
        if (fwrite(output, 1, z, stdout) != z) {
            perror(0);
            exit(EXIT_FAILURE);
        }
    }
    if (ferror(stdin)) {
        perror(0);
        exit(EXIT_FAILURE);
    }
}

static void
encrypt(char *key, int len)
{
    /* Generate an IV */
    FILE *urandom = fopen("/dev/urandom", "rb");
    if (!urandom) {
        perror("/dev/urandom");
        exit(EXIT_FAILURE);
    }
    if (fread(key + len, 1, IVLEN, urandom) != IVLEN) {
        perror("/dev/urandom");
        exit(EXIT_FAILURE);
    }
    fclose(urandom);

    /* Write IV header */
    if (fwrite(key + len, 1, IVLEN, stdout) != IVLEN) {
        perror(0);
        exit(EXIT_FAILURE);
    }

    struct rc4 rc4[1];
    rc4_init(rc4, key, len + IVLEN);
    pump(rc4);
}

static void
decrypt(char *key, int len)
{
    /* Read IV header */
    if (!fread(key + len, 1, IVLEN, stdin)) {
        perror("reading input");
        exit(EXIT_FAILURE);
    }

    struct rc4 rc4[1];
    rc4_init(rc4, key, len + IVLEN);
    pump(rc4);
}

int
main(int argc, char **argv)
{
    char *keyfile = 0;
    enum {M_ENCRYPT = 1, M_DECRYPT} mode = 0;

    int option;
    while ((option = getopt (argc, argv, "DEk:")) != -1) {
        switch (option) {
            case 'E':
                mode = M_ENCRYPT;
                break;
            case 'D':
                mode = M_DECRYPT;
                break;
            case 'k':
                keyfile = optarg;
                break;
            default:
                usage();
        }
    }

    if (!mode)
        usage();

    int keylen;
    char key[256];
    if (!keyfile) {
        /* Prompt user for a passphrase */
        char check[sizeof(key)];
        FILE *tty = fopen("/dev/tty", "r+");
        if (!tty) {
            perror(keyfile);
            exit(EXIT_FAILURE);
        }

        fputs("Passphrase: ", tty);
        fflush(tty);
        if (!fgets(key, sizeof(key) - IVLEN, tty)) {
            fputs("failed to read phassphrase\n", stderr);
            exit(EXIT_FAILURE);
        }

        fputs("Passphrase (repeat): ", tty);
        fflush(tty);
        if (!fgets(check, sizeof(check) - IVLEN, tty)) {
            fputs("failed to read phassphrase\n", stderr);
            exit(EXIT_FAILURE);
        }

        if (strcmp(check, key)) {
            fputs("fatal: passphrases don't match\n", stderr);
            exit(EXIT_FAILURE);
        }

        keylen = strlen(key);
        key[keylen] = 0;
        fclose(tty);
    } else {
        /* Read key from a file */
        FILE *f = fopen(keyfile, "rb");
        if (!f) {
            perror(keyfile);
            exit(EXIT_FAILURE);
        }
        keylen = fread(key, 1, sizeof(key), f);
        if (!keylen && ferror(f)) {
            perror(keyfile);
            exit(EXIT_FAILURE);
        }
        fclose(f);
    }

    switch (mode) {
        case M_ENCRYPT:
            encrypt(key, keylen);
            break;
        case M_DECRYPT:
            decrypt(key, keylen);
            break;
    }
}
