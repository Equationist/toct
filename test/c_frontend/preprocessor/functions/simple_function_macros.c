/* Test simple function-like macros */
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define SQUARE(x) ((x) * (x))
#define ABS(x) ((x) < 0 ? -(x) : (x))

#include <stdio.h>

int main() {
    int x = 5, y = 10;
    
    printf("MAX(%d, %d) = %d\n", x, y, MAX(x, y));
    printf("MIN(%d, %d) = %d\n", x, y, MIN(x, y));
    printf("SQUARE(%d) = %d\n", x, SQUARE(x));
    printf("ABS(-7) = %d\n", ABS(-7));
    printf("ABS(3) = %d\n", ABS(3));
    
    /* Test with expressions */
    printf("MAX(x+1, y-2) = %d\n", MAX(x+1, y-2));
    printf("SQUARE(x+1) = %d\n", SQUARE(x+1));
    
    return 0;
}