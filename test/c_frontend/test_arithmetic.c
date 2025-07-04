// Test arithmetic operations
int main() {
    int a = 10;
    int b = 3;
    
    // Test subtraction
    int sub = a - b;  // 7
    
    // Test multiplication
    int mul = a * b;  // 30
    
    // Test division
    int div = a / b;  // 3
    
    // Test modulo
    int mod = a % b;  // 1
    
    // Combined operations
    int result = mul - div + mod;  // 30 - 3 + 1 = 28
    
    return result;
}