/* Test macro redefinition without #undef */
#define VALUE 10
#define VALUE 20  /* This should cause a warning/error */

#include <stdio.h>

int main() {
    printf("VALUE = %d\n", VALUE);
    return 0;
}