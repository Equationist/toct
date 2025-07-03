/* Test conditional compilation */
#define FEATURE_A
#define VERSION 2

int main() {
#ifdef FEATURE_A
    int feature_a_code = 1;
#else
    int feature_a_code = 0;
#endif

#ifdef FEATURE_B
    int feature_b_code = 1;
#else
    int feature_b_code = 0;
#endif

#if VERSION >= 2
    int version_check = 1;
#else
    int version_check = 0;
#endif

    return 0;
}