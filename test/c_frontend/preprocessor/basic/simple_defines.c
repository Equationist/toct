/* Test basic #define directives */
#define MAX_SIZE 100
#define PI 3.14159
#define TRUE 1
#define FALSE 0
#define GREETING "Hello, World!"

#include <stdio.h>

int main() {
    int size = MAX_SIZE;
    float pi_val = PI;
    int flag = TRUE;
    
    printf("MAX_SIZE = %d\n", size);
    printf("PI = %.5f\n", pi_val);
    printf("TRUE = %d, FALSE = %d\n", flag, FALSE);
    printf("Greeting: %s\n", GREETING);
    
    return 0;
}