/* Test complex nested conditional compilation */
#define FEATURE_SET_A
#define SUBSYSTEM_1
/* #define SUBSYSTEM_2 */
#define OPTIMIZATION_LEVEL 2

#include <stdio.h>

int main() {
    printf("Configuration:\n");

#ifdef FEATURE_SET_A
    printf("Feature Set A enabled\n");
    
    #ifdef SUBSYSTEM_1
        printf("  Subsystem 1 active\n");
        
        #if OPTIMIZATION_LEVEL >= 2
            printf("    High optimization for Subsystem 1\n");
        #else
            printf("    Standard optimization for Subsystem 1\n");
        #endif
    #endif
    
    #ifdef SUBSYSTEM_2
        printf("  Subsystem 2 active\n");
    #else
        printf("  Subsystem 2 inactive\n");
    #endif
    
#else
    printf("Feature Set A disabled\n");
    
    #ifdef SUBSYSTEM_1
        printf("  Basic Subsystem 1\n");
    #endif
#endif

#if defined(FEATURE_SET_A) && defined(SUBSYSTEM_1) && (OPTIMIZATION_LEVEL > 1)
    printf("Full feature configuration detected\n");
#endif

    return 0;
}