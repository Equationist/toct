/* Test macro side effects and multiple evaluation */
#define UNSAFE_MAX(a, b) ((a) > (b) ? (a) : (b))
#define UNSAFE_SQUARE(x) ((x) * (x))

#include <stdio.h>

int increment_counter() {
    static int counter = 0;
    return ++counter;
}

int main() {
    int x = 5, y = 3;
    
    printf("Initial x = %d, y = %d\n", x, y);
    
    /* This will increment x twice due to macro expansion */
    printf("UNSAFE_MAX(++x, y) = %d\n", UNSAFE_MAX(++x, y));
    printf("x after UNSAFE_MAX = %d\n", x);
    
    /* Reset and test with function call */
    x = 5;
    printf("Reset x = %d\n", x);
    printf("UNSAFE_SQUARE(increment_counter()) = %d\n", UNSAFE_SQUARE(increment_counter()));
    printf("Counter called multiple times due to macro expansion\n");
    
    return 0;
}