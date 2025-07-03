/* Test nested macro expansion */
#define DOUBLE(x) ((x) * 2)
#define TRIPLE(x) ((x) * 3)
#define ADD(a, b) ((a) + (b))
#define MULTIPLY(a, b) ((a) * (b))

/* Nested macro definitions */
#define DOUBLE_TRIPLE(x) DOUBLE(TRIPLE(x))
#define ADD_DOUBLE(a, b) ADD(DOUBLE(a), DOUBLE(b))
#define QUAD(x) DOUBLE(DOUBLE(x))

#include <stdio.h>

int main() {
    int x = 5;
    
    printf("DOUBLE(%d) = %d\n", x, DOUBLE(x));
    printf("TRIPLE(%d) = %d\n", x, TRIPLE(x));
    printf("DOUBLE_TRIPLE(%d) = %d\n", x, DOUBLE_TRIPLE(x));
    printf("ADD_DOUBLE(%d, %d) = %d\n", x, 3, ADD_DOUBLE(x, 3));
    printf("QUAD(%d) = %d\n", x, QUAD(x));
    
    /* Complex nesting */
    printf("MULTIPLY(DOUBLE(%d), TRIPLE(%d)) = %d\n", x, x, MULTIPLY(DOUBLE(x), TRIPLE(x)));
    
    return 0;
}