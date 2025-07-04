/* string.h - String Library for CompilerKit C Compiler */

#ifndef _STRING_H
#define _STRING_H

/* Standard types */
typedef unsigned long size_t;

/* String length */
size_t strlen(const char* s);

/* String copy */
char* strcpy(char* dest, const char* src);
char* strncpy(char* dest, const char* src, size_t n);

/* String concatenation */
char* strcat(char* dest, const char* src);
char* strncat(char* dest, const char* src, size_t n);

/* String comparison */
int strcmp(const char* s1, const char* s2);
int strncmp(const char* s1, const char* s2, size_t n);
int strcasecmp(const char* s1, const char* s2);
int strncasecmp(const char* s1, const char* s2, size_t n);

/* String search */
char* strchr(const char* s, int c);
char* strrchr(const char* s, int c);
char* strstr(const char* haystack, const char* needle);
size_t strcspn(const char* s1, const char* s2);
size_t strspn(const char* s1, const char* s2);
char* strpbrk(const char* s1, const char* s2);
char* strtok(char* str, const char* delim);

/* Memory functions */
void* memset(void* s, int c, size_t n);
void* memcpy(void* dest, const void* src, size_t n);
void* memmove(void* dest, const void* src, size_t n);
int memcmp(const void* s1, const void* s2, size_t n);
void* memchr(const void* s, int c, size_t n);

/* Error strings */
char* strerror(int errnum);

/* Null pointer */
#ifndef NULL
#define NULL ((void*)0)
#endif

#endif /* _STRING_H */