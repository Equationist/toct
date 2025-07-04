// Test control flow
int main() {
    int x = 10;
    int y = 5;
    int result;
    
    if (x > y) {
        result = x - y;
    } else {
        result = y - x;
    }
    
    return result;
}