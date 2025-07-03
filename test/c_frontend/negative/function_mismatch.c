/* Test function argument mismatch */
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    /* Wrong number of arguments */
    int result = add(5);
    
    return 0;
}