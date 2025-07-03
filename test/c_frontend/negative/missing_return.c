/* Test missing return statement */
#include <stdio.h>

int compute(int x) {
    int y = x * 2;
    /* Missing return statement */
}

/* Also test void function with return value */
void print_value() {
    return 42;  /* Error: void function cannot return value */
}

int main() {
    int result = compute(5);
    printf("Result: %d\n", result);
    
    return 0;
}