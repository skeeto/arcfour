#include <stdio.h>
#include <string.h>
#include "rc4.h"

int
main(int argc, char **argv)
{
    struct rc4 rc4[1];
    static char buf[16UL * 1024];

    rc4_init(rc4, argv[argc - 1], strlen(argv[argc - 1]) + 1);
    do 
        rc4_rand(rc4, buf, sizeof(buf));
    while (fwrite(buf, 1, sizeof(buf), stdout));
}
