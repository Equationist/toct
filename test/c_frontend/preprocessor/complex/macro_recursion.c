/* Test macro recursion and self-reference */
#define EMPTY()
#define DEFER(id) id EMPTY()
#define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()

/* Self-referential macro (should not expand infinitely) */
#define FOO FOO
#define BAR(x) BAR(x)

/* Recursive-like macro using deferred expansion */
#define REPEAT_1(m, x) m(x)
#define REPEAT_2(m, x) m(x) REPEAT_1(m, x)
#define REPEAT_3(m, x) m(x) REPEAT_2(m, x)

#define PRINT_NUM(x) printf("%d ", x);

#include <stdio.h>

int main() {
    printf("Macro recursion tests:\n");
    
    /* These should just expand to themselves, not infinitely */
    printf("Self-referential macros (should not cause infinite expansion):\n");
    printf("FOO expands to: FOO (literal)\n");
    
    printf("\nRepeat patterns:\n");
    printf("REPEAT_1: ");
    REPEAT_1(PRINT_NUM, 1);
    printf("\n");
    
    printf("REPEAT_2: ");
    REPEAT_2(PRINT_NUM, 2);
    printf("\n");
    
    printf("REPEAT_3: ");
    REPEAT_3(PRINT_NUM, 3);
    printf("\n");
    
    return 0;
}