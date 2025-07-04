/* stdio_simple.h - Simplified Standard Input/Output Library for CompilerKit C Compiler */

#ifndef _STDIO_SIMPLE_H
#define _STDIO_SIMPLE_H

/* Simple printf declarations without variadic arguments for testing */
int printf(const char* format);
int printf_int(const char* format, int value);
int printf_str(const char* format, const char* str);
int printf_two_int(const char* format, int a, int b);
int printf_three_int(const char* format, int a, int b, int c);

/* Basic I/O */
int putchar(int c);
int puts(const char* s);
int getchar(void);

/* Null pointer */
#ifndef NULL
#define NULL ((void*)0)
#endif

/* End of file constant */
#define EOF (-1)

#endif /* _STDIO_SIMPLE_H */