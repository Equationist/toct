/* Test array operations */
#include <stdio.h>

int main() {
    int arr[5] = {10, 20, 30, 40, 50};
    int i;
    
    printf("Array elements:\n");
    for (i = 0; i < 5; i++) {
        printf("arr[%d] = %d\n", i, arr[i]);
    }
    
    printf("Array via pointer arithmetic:\n");
    for (i = 0; i < 5; i++) {
        printf("*(arr + %d) = %d\n", i, *(arr + i));
    }
    
    arr[2] = 99;
    printf("After arr[2] = 99:\n");
    printf("arr[2] = %d\n", arr[2]);
    
    return 0;
}