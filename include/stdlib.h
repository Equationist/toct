/* stdlib.h - Standard Library for CompilerKit C Compiler */

#ifndef _STDLIB_H
#define _STDLIB_H

/* Standard types */
typedef unsigned long size_t;

/* Memory management */
void* malloc(size_t size);
void* calloc(size_t num, size_t size);
void* realloc(void* ptr, size_t size);
void free(void* ptr);

/* String conversion */
int atoi(const char* str);
long atol(const char* str);
double atof(const char* str);
long strtol(const char* str, char** endptr, int base);
unsigned long strtoul(const char* str, char** endptr, int base);
double strtod(const char* str, char** endptr);

/* Process control */
void exit(int status);
void abort(void);
int atexit(void (*function)(void));

/* Environment */
char* getenv(const char* name);
int system(const char* command);

/* Searching and sorting */
void* bsearch(const void* key, const void* base, size_t num, size_t size, 
              int (*compare)(const void*, const void*));
void qsort(void* base, size_t num, size_t size, 
           int (*compare)(const void*, const void*));

/* Math */
int abs(int x);
long labs(long x);
int rand(void);
void srand(unsigned int seed);

/* Maximum values */
#define RAND_MAX 32767

/* Exit status codes */
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

/* Null pointer */
#ifndef NULL
#define NULL ((void*)0)
#endif

#endif /* _STDLIB_H */