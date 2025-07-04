// Test loops with block parameters
int test_while_loop() {
    int i = 0;
    int sum = 0;
    
    while (i < 10) {
        sum = sum + i;
        i = i + 1;
    }
    
    return sum;
}

int test_for_loop() {
    int sum = 0;
    int i;
    
    for (i = 0; i < 10; i = i + 1) {
        sum = sum + i;
    }
    
    return sum;
}

int main() {
    int while_result = test_while_loop();
    int for_result = test_for_loop();
    
    if (while_result == 45 && for_result == 45) {
        return 0;  // Success
    } else {
        return 1;  // Failure
    }
}