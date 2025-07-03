/* Test pointer arithmetic */
#include <stdio.h>

int main() {
    int arr[5] = {1, 2, 3, 4, 5};
    int *ptr = arr;
    
    printf("Original pointer: *ptr = %d\n", *ptr);
    
    ptr++;
    printf("After ptr++: *ptr = %d\n", *ptr);
    
    ptr += 2;
    printf("After ptr += 2: *ptr = %d\n", *ptr);
    
    ptr--;
    printf("After ptr--: *ptr = %d\n", *ptr);
    
    printf("Pointer difference: %ld\n", (long)(ptr - arr));
    
    return 0;
}