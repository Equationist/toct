/* Test undefined macro usage */
#include <stdio.h>

int main() {
    /* Using undefined macro should cause error */
    int x = UNDEFINED_MACRO;
    printf("x = %d\n", x);
    
    return 0;
}