// Test while loop
int main() {
    int sum = 0;
    int i = 1;
    
    while (i <= 5) {
        sum = sum + i;
        i = i + 1;
    }
    
    return sum;  // Should return 15 (1+2+3+4+5)
}