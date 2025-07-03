/* Test function pointers */
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int subtract(int a, int b) {
    return a - b;
}

int apply_operation(int (*operation)(int, int), int x, int y) {
    return operation(x, y);
}

int main() {
    int x = 10;
    int y = 5;
    
    printf("Direct call: add(%d, %d) = %d\n", x, y, add(x, y));
    printf("Function pointer: add(%d, %d) = %d\n", x, y, apply_operation(add, x, y));
    printf("Function pointer: subtract(%d, %d) = %d\n", x, y, apply_operation(subtract, x, y));
    
    return 0;
}