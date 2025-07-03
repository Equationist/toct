/* Test loop constructs */
#include <stdio.h>

int main() {
    int i;
    
    printf("For loop:\n");
    for (i = 0; i < 5; i++) {
        printf("i = %d\n", i);
    }
    
    printf("While loop:\n");
    i = 0;
    while (i < 3) {
        printf("while i = %d\n", i);
        i++;
    }
    
    printf("Do-while loop:\n");
    i = 0;
    do {
        printf("do-while i = %d\n", i);
        i++;
    } while (i < 2);
    
    return 0;
}