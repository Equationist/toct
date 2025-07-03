/* Test multi-dimensional arrays */
#include <stdio.h>

int main() {
    int matrix[3][3] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    
    int i, j;
    
    printf("Matrix:\n");
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
    
    printf("Element at [1][2] = %d\n", matrix[1][2]);
    matrix[1][2] = 99;
    printf("After matrix[1][2] = 99, element = %d\n", matrix[1][2]);
    
    return 0;
}