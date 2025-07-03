/* Test basic pointer operations */
#include <stdio.h>

int main() {
    int x = 42;
    int *ptr = &x;
    
    printf("x = %d\n", x);
    printf("&x = %p\n", (void*)&x);
    printf("ptr = %p\n", (void*)ptr);
    printf("*ptr = %d\n", *ptr);
    
    *ptr = 100;
    printf("After *ptr = 100:\n");
    printf("x = %d\n", x);
    printf("*ptr = %d\n", *ptr);
    
    return 0;
}