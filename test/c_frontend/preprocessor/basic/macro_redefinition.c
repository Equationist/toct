/* Test macro redefinition and #undef */
#define VALUE 10
#include <stdio.h>

int main() {
    printf("Initial VALUE = %d\n", VALUE);
    
#undef VALUE
#define VALUE 20
    
    printf("Redefined VALUE = %d\n", VALUE);
    
#undef VALUE
#define VALUE "STRING"
    
    printf("VALUE as string = %s\n", VALUE);
    
    return 0;
}