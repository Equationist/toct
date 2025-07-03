/* Test variable declarations and assignments */
#include <stdio.h>

int main() {
    int x = 42;
    int y;
    y = x * 2;
    
    printf("x = %d\n", x);
    printf("y = %d\n", y);
    
    x = y + 10;
    printf("x after assignment = %d\n", x);
    
    return 0;
}