/* Test advanced macro techniques */
#define EVAL(x) x
#define CAT(a, b) a##b
#define XCAT(a, b) CAT(a, b)

/* X-Macro pattern for code generation */
#define COLORS \
    X(RED, 1) \
    X(GREEN, 2) \
    X(BLUE, 3) \
    X(YELLOW, 4)

/* Generate enum */
typedef enum {
#define X(name, value) COLOR_##name = value,
    COLORS
#undef X
} color_t;

/* Generate string array */
const char* color_names[] = {
#define X(name, value) [COLOR_##name] = #name,
    COLORS
#undef X
};

/* Multi-level macro expansion */
#define INDIRECT(m, x) m(x)
#define MAKE_NAME(prefix) XCAT(prefix, _function)
#define DECLARE_FUNCTION(name) int name() { return 42; }

DECLARE_FUNCTION(MAKE_NAME(test))
DECLARE_FUNCTION(MAKE_NAME(demo))

#include <stdio.h>

int main() {
    printf("Advanced macro tests:\n");
    
    printf("Colors via X-macro:\n");
    printf("RED = %d (%s)\n", COLOR_RED, color_names[COLOR_RED]);
    printf("GREEN = %d (%s)\n", COLOR_GREEN, color_names[COLOR_GREEN]);
    printf("BLUE = %d (%s)\n", COLOR_BLUE, color_names[COLOR_BLUE]);
    printf("YELLOW = %d (%s)\n", COLOR_YELLOW, color_names[COLOR_YELLOW]);
    
    printf("\nGenerated functions:\n");
    printf("test_function() = %d\n", test_function());
    printf("demo_function() = %d\n", demo_function());
    
    return 0;
}