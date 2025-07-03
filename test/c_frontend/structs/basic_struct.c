/* Test basic struct operations */
#include <stdio.h>

struct Point {
    int x;
    int y;
};

int main() {
    struct Point p1 = {10, 20};
    struct Point p2;
    
    printf("p1.x = %d, p1.y = %d\n", p1.x, p1.y);
    
    p2.x = 30;
    p2.y = 40;
    printf("p2.x = %d, p2.y = %d\n", p2.x, p2.y);
    
    p1 = p2;
    printf("After p1 = p2: p1.x = %d, p1.y = %d\n", p1.x, p1.y);
    
    return 0;
}