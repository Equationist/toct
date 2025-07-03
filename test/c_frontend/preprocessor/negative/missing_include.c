/* Test missing include file */
#include "nonexistent_file.h"
#include <stdio.h>

int main() {
    printf("This should not compile\n");
    return 0;
}