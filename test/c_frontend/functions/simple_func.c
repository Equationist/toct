/* Test simple function definition and calling */
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int multiply(int x, int y) {
    int result = x * y;
    return result;
}

int main() {
    int x = 5;
    int y = 3;
    
    printf("add(%d, %d) = %d\n", x, y, add(x, y));
    printf("multiply(%d, %d) = %d\n", x, y, multiply(x, y));
    
    return 0;
}