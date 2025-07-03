/* Test type error cases - these should fail compilation */
#include <stdio.h>

int main() {
    int x = 5;
    char* str = "hello";
    
    /* Type mismatch - cannot assign string to int */
    x = str;
    
    return 0;
}