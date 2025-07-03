/* Test variadic macros (C99 feature, but commonly supported) */
#define DEBUG_PRINT(fmt, ...) printf("DEBUG: " fmt "\n", ##__VA_ARGS__)
#define LOG(level, ...) printf("[%s] ", level); printf(__VA_ARGS__); printf("\n")

#include <stdio.h>

int main() {
    DEBUG_PRINT("Simple message");
    DEBUG_PRINT("Value is %d", 42);
    DEBUG_PRINT("Two values: %d and %s", 10, "hello");
    
    LOG("INFO", "System started");
    LOG("ERROR", "Failed with code %d", 404);
    LOG("WARN", "Memory usage: %d%%", 85);
    
    return 0;
}