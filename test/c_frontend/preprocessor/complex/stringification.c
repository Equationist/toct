/* Test stringification operator (#) */
#define STRINGIFY(x) #x
#define PRINT_VAR(var) printf(#var " = %d\n", var)
#define ASSERT_EQ(a, b) printf("Assert: " #a " == " #b " -> %s\n", (a) == (b) ? "PASS" : "FAIL")

#include <stdio.h>

int main() {
    int value = 42;
    int result = 100;
    
    printf("Stringification tests:\n");
    printf("STRINGIFY(hello) = %s\n", STRINGIFY(hello));
    printf("STRINGIFY(123) = %s\n", STRINGIFY(123));
    printf("STRINGIFY(value + 10) = %s\n", STRINGIFY(value + 10));
    
    printf("\nVariable printing:\n");
    PRINT_VAR(value);
    PRINT_VAR(result);
    
    printf("\nAssertion tests:\n");
    ASSERT_EQ(value, 42);
    ASSERT_EQ(result, 100);
    ASSERT_EQ(value * 2, 84);
    
    return 0;
}