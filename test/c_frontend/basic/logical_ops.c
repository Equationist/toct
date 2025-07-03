/* Test logical and bitwise operations */
#include <stdio.h>

int main() {
    int a = 1;
    int b = 0;
    int x = 6;  // 110 in binary
    int y = 3;  // 011 in binary
    
    printf("Logical operations:\n");
    printf("a && b = %d\n", a && b);
    printf("a || b = %d\n", a || b);
    printf("!a = %d\n", !a);
    printf("!b = %d\n", !b);
    
    printf("Bitwise operations:\n");
    printf("x & y = %d\n", x & y);
    printf("x | y = %d\n", x | y);
    printf("x ^ y = %d\n", x ^ y);
    printf("~x = %d\n", ~x);
    printf("x << 1 = %d\n", x << 1);
    printf("x >> 1 = %d\n", x >> 1);
    
    return 0;
}