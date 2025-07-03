/* Test variadic functions */
#include <stdio.h>
#include <stdarg.h>

int sum_ints(int count, ...) {
    va_list args;
    va_start(args, count);
    
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }
    
    va_end(args);
    return total;
}

void print_ints(int count, ...) {
    va_list args;
    va_start(args, count);
    
    printf("Values: ");
    for (int i = 0; i < count; i++) {
        if (i > 0) printf(", ");
        printf("%d", va_arg(args, int));
    }
    printf("\n");
    
    va_end(args);
}

int main() {
    int result = sum_ints(4, 10, 20, 30, 40);
    printf("Sum: %d\n", result);
    
    print_ints(3, 1, 2, 3);
    
    return 0;
}