/* Test recursive functions */
#include <stdio.h>

int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

int main() {
    printf("factorial(5) = %d\n", factorial(5));
    printf("fibonacci(7) = %d\n", fibonacci(7));
    
    return 0;
}