/* Test sizeof operator */
#include <stdio.h>

struct TestStruct {
    int x;
    char y;
    double z;
};

int main() {
    int arr[10];
    struct TestStruct s;
    
    printf("sizeof(int): %zu\n", sizeof(int));
    printf("sizeof(char): %zu\n", sizeof(char));
    printf("sizeof(double): %zu\n", sizeof(double));
    printf("sizeof(int*): %zu\n", sizeof(int*));
    printf("sizeof(arr): %zu\n", sizeof(arr));
    printf("sizeof(struct TestStruct): %zu\n", sizeof(struct TestStruct));
    printf("sizeof(s): %zu\n", sizeof(s));
    
    return 0;
}