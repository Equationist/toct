/* Test #include directive */
#include <stdio.h>
#include "include_test.h"

int header_function(int x) {
    return x * 2;
}

int main() {
    printf("HEADER_VALUE = %d\n", HEADER_VALUE);
    printf("HEADER_STRING = %s\n", HEADER_STRING);
    printf("header_function(5) = %d\n", header_function(5));
    
    return 0;
}