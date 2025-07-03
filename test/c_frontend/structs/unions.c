/* Test union types */
#include <stdio.h>

union Data {
    int i;
    float f;
    char c;
};

int main() {
    union Data data;
    
    data.i = 42;
    printf("data.i = %d\n", data.i);
    
    data.f = 3.14f;
    printf("data.f = %.2f\n", data.f);
    printf("data.i after setting f = %d\n", data.i);
    
    data.c = 'A';
    printf("data.c = %c\n", data.c);
    printf("data.c as int = %d\n", (int)data.c);
    
    printf("sizeof(union Data) = %zu\n", sizeof(union Data));
    
    return 0;
}