// Test bitwise operations
int main() {
    int a = 0x0F;  // 15 (0000 1111)
    int b = 0x06;  // 6  (0000 0110)
    
    // Test AND
    int and_result = a & b;  // 6 (0000 0110)
    
    // Test OR
    int or_result = a | b;   // 15 (0000 1111)
    
    // Test XOR
    int xor_result = a ^ b;  // 9 (0000 1001)
    
    // Test shift left
    int shl_result = b << 2; // 24 (0001 1000)
    
    // Test shift right
    int shr_result = a >> 2; // 3 (0000 0011)
    
    // Combined operations
    int result = (and_result | shl_result) ^ shr_result;  // (6 | 24) ^ 3 = 30 ^ 3 = 29
    
    return result;
}