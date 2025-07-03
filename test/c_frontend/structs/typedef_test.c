/* Test typedef declarations */
#include <stdio.h>

typedef int Integer;
typedef char* String;

typedef struct {
    int x;
    int y;
} Point;

typedef struct Node {
    int data;
    struct Node* next;
} Node;

int main() {
    Integer num = 42;
    String greeting = "Hello";
    Point p = {10, 20};
    Node node1 = {100, NULL};
    Node node2 = {200, &node1};
    
    printf("Integer: %d\n", num);
    printf("String: %s\n", greeting);
    printf("Point: (%d, %d)\n", p.x, p.y);
    printf("Node1 data: %d\n", node1.data);
    printf("Node2 data: %d, next data: %d\n", node2.data, node2.next->data);
    
    return 0;
}