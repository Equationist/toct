/* Test function-like macro expansion */
#define ADD(a, b) ((a) + (b))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define SQUARE(n) ((n) * (n))

int main() {
    int result1 = ADD(5, 3);
    int result2 = MAX(10, 20);
    int result3 = SQUARE(4);
    int complex = ADD(SQUARE(2), MAX(1, 3));
    
    return 0;
}