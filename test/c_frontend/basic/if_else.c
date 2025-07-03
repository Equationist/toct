/* Test if-else control flow */
#include <stdio.h>

int main() {
    int x = 10;
    int y = 5;
    
    if (x > y) {
        printf("x is greater than y\n");
    } else {
        printf("x is not greater than y\n");
    }
    
    if (x == y) {
        printf("x equals y\n");
    } else if (x < y) {
        printf("x is less than y\n");
    } else {
        printf("x is greater than y (else if)\n");
    }
    
    return 0;
}