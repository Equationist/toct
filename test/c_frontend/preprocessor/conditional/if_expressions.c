/* Test #if with expressions */
#define VERSION_MAJOR 2
#define VERSION_MINOR 1
#define DEBUG_LEVEL 3

#include <stdio.h>

int main() {
#if VERSION_MAJOR > 1
    printf("Version is 2.x or higher\n");
#else
    printf("Version is 1.x\n");
#endif

#if VERSION_MAJOR == 2 && VERSION_MINOR >= 1
    printf("Version 2.1 or higher detected\n");
#endif

#if DEBUG_LEVEL >= 3
    printf("Debug mode: VERBOSE\n");
#elif DEBUG_LEVEL >= 2
    printf("Debug mode: NORMAL\n");
#elif DEBUG_LEVEL >= 1
    printf("Debug mode: MINIMAL\n");
#else
    printf("Debug mode: OFF\n");
#endif

#if defined(VERSION_MAJOR) && !defined(VERSION_PATCH)
    printf("Major version defined, patch version not defined\n");
#endif

    return 0;
}