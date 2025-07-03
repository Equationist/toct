/* Test void functions and global variables */
#include <stdio.h>

int global_counter = 0;

void increment_counter() {
    global_counter++;
}

void print_counter() {
    printf("Counter: %d\n", global_counter);
}

int main() {
    print_counter();
    increment_counter();
    print_counter();
    increment_counter();
    increment_counter();
    print_counter();
    
    return 0;
}