/* Test token pasting operator (##) */
#define CONCAT(a, b) a##b
#define MAKE_FUNC(name) int func_##name() { return name##_value; }
#define DECLARE_VAR(type, name) type var_##name
#define PRINT_FIELD(struct_name, field) printf(#struct_name "." #field " = %d\n", struct_name.field_##field)

#include <stdio.h>

/* Use token pasting to create variables and functions */
int one_value = 10;
int two_value = 20;
int test_value = 99;

MAKE_FUNC(one)
MAKE_FUNC(two)
MAKE_FUNC(test)

struct MyStruct {
    int field_x;
    int field_y;
    int field_count;
};

int main() {
    DECLARE_VAR(int, counter) = 5;
    DECLARE_VAR(float, ratio) = 3.14f;
    
    printf("Token pasting tests:\n");
    printf("CONCAT(hel, lo) -> hello\n");
    printf("var_counter = %d\n", var_counter);
    printf("var_ratio = %.2f\n", var_ratio);
    
    printf("\nFunction calls via token pasting:\n");
    printf("func_one() = %d\n", func_one());
    printf("func_two() = %d\n", func_two());
    printf("func_test() = %d\n", func_test());
    
    struct MyStruct s = {100, 200, 42};
    printf("\nStruct field access:\n");
    PRINT_FIELD(s, x);
    PRINT_FIELD(s, y);
    PRINT_FIELD(s, count);
    
    return 0;
}