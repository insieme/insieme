#ifndef __GEM_STDIO_H
#define __GEM_STDIO_H

typedef struct FILE {
} FILE;

typedef unsigned long size_t;

FILE *stderr, *stdout, *stdin;

#define NULL ((void *)0)
#define EOF -1

#define SEEK_SET    0   /* Seek from beginning of file.  */
#define SEEK_CUR    1   /* Seek from current position.  */
#define SEEK_END    2   /* Seek from end of file.  */

const int L_tmpnam = 12;

int printf(const char *, ...);
int fprintf(FILE *, const char *, ...);
int sprintf(char *, const char *, ...);
int snprintf(char *, unsigned long, const char *, ...);
int vsprintf();
int vfprintf();
int fscanf(FILE *stream, const char *format, ...);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);

char *tmpnam(char *s);

#endif // __GEM_STDIO_H
