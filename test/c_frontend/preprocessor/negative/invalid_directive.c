/* Test invalid preprocessor directive */
#invalid_directive
#unknown_command value

#include <stdio.h>

int main() {
    printf("This should not compile\n");
    return 0;
}