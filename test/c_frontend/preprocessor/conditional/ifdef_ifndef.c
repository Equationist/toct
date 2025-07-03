/* Test #ifdef and #ifndef directives */
#define FEATURE_A
/* FEATURE_B is not defined */

#include <stdio.h>

int main() {
#ifdef FEATURE_A
    printf("Feature A is enabled\n");
#else
    printf("Feature A is disabled\n");
#endif

#ifdef FEATURE_B
    printf("Feature B is enabled\n");
#else
    printf("Feature B is disabled\n");
#endif

#ifndef FEATURE_C
    printf("Feature C is not defined\n");
#else
    printf("Feature C is defined\n");
#endif

#ifdef FEATURE_A
    #ifdef FEATURE_B
        printf("Both A and B enabled\n");
    #else
        printf("A enabled, B disabled\n");
    #endif
#endif

    return 0;
}