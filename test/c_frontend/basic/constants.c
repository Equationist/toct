/* Test various constant types */
#include <stdio.h>

int main() {
    // Integer constants
    int dec = 42;
    int hex = 0x2A;
    int oct = 052;
    
    // Character constants  
    char c = 'A';
    char newline = '\n';
    char tab = '\t';
    
    // String constants
    char* str = "Hello World";
    
    // Float constants
    float f = 3.14f;
    double d = 2.718;
    
    printf("Decimal: %d\n", dec);
    printf("Hex: %d\n", hex);
    printf("Octal: %d\n", oct);
    printf("Character: %c\n", c);
    printf("String: %s\n", str);
    printf("Float: %.2f\n", f);
    printf("Double: %.3f\n", d);
    
    return 0;
}