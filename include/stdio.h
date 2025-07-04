/* stdio.h - Standard Input/Output Library for CompilerKit C Compiler */

#ifndef _STDIO_H
#define _STDIO_H

/* Standard I/O types */
typedef struct {
    int _fd;
    int _flags;
    char* _buffer;
} FILE;

/* Standard streams */
extern FILE* stdin;
extern FILE* stdout; 
extern FILE* stderr;

/* Standard I/O functions */
int printf(const char* format, ...);
int fprintf(FILE* stream, const char* format, ...);
int sprintf(char* str, const char* format, ...);
int snprintf(char* str, int size, const char* format, ...);

int scanf(const char* format, ...);
int fscanf(FILE* stream, const char* format, ...);
int sscanf(const char* str, const char* format, ...);

int putchar(int c);
int puts(const char* s);
int fputs(const char* s, FILE* stream);
int fputc(int c, FILE* stream);

int getchar(void);
char* gets(char* s);
char* fgets(char* s, int size, FILE* stream);
int fgetc(FILE* stream);
int ungetc(int c, FILE* stream);

FILE* fopen(const char* filename, const char* mode);
FILE* freopen(const char* filename, const char* mode, FILE* stream);
int fclose(FILE* stream);
int fflush(FILE* stream);

long ftell(FILE* stream);
int fseek(FILE* stream, long offset, int whence);
void rewind(FILE* stream);
int feof(FILE* stream);
int ferror(FILE* stream);
void clearerr(FILE* stream);

/* File position constants */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* End of file constant */
#define EOF (-1)

/* Buffer size */
#define BUFSIZ 8192

/* Null pointer */
#ifndef NULL
#define NULL ((void*)0)
#endif

#endif /* _STDIO_H */