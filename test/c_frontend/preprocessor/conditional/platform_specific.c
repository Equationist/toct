/* Test platform-specific conditional compilation */
#include <stdio.h>

int main() {
    printf("Platform detection:\n");

#ifdef __linux__
    printf("Running on Linux\n");
#elif defined(__APPLE__) && defined(__MACH__)
    printf("Running on macOS\n");
#elif defined(_WIN32) || defined(_WIN64)
    printf("Running on Windows\n");
#else
    printf("Unknown platform\n");
#endif

#ifdef __GNUC__
    printf("Compiled with GCC version %d.%d\n", __GNUC__, __GNUC_MINOR__);
#elif defined(__clang__)
    printf("Compiled with Clang\n");
#elif defined(_MSC_VER)
    printf("Compiled with MSVC\n");
#else
    printf("Unknown compiler\n");
#endif

#if defined(__x86_64__) || defined(_M_X64)
    printf("64-bit architecture\n");
#elif defined(__i386) || defined(_M_IX86)
    printf("32-bit architecture\n");
#else
    printf("Unknown architecture\n");
#endif

    return 0;
}