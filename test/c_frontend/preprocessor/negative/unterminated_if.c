/* Test unterminated #if directive */
#define FEATURE_A

#include <stdio.h>

int main() {
#if defined(FEATURE_A)
    printf("Feature A enabled\n");
    /* Missing #endif */
    
    return 0;
}