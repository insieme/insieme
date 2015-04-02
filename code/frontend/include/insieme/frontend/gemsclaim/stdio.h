#ifndef __GEM_STDIO_H
#define __GEM_STDIO_H

#include <string.h>
#include <stdarg.h>
#include <assert.h>

/* The following 3 fields should be provided to the backend compiler
 * test/gemsclaim/tools/enc.cpp produces the header file with proper
 * definitions  */
extern unsigned long input_file_pos;
extern const char input_file[];
extern const unsigned long INPUT_FILE_LEN;

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

char *tmpnam(char *s);

/* We don't care about the following functions but we want to avoid implicit declaration warnings */
FILE *fopen(const char *path, const char *mode);
FILE *fdopen(int fd, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *stream);
int fclose(FILE *fp);
int printf(const char *format, ...);
int sprintf(char *str, const char *format, ...);
int snprintf(char *str, size_t size, const char *format, ...);
int scanf(const char *format, ...);
int sscanf(const char *str, const char *format, ...);

int gemfeof(FILE *stream) {
    if(stream == stdin) {
        if(input_file_pos >= INPUT_FILE_LEN)
            return 1;
        else
            return 0;
    }
    else {
        assert(0 && "feof is not supported.");
        return 1;
    }
}

int ferror(FILE *stream);
int gemferror(FILE *stream) {
    if(stream == stdout || stream == stderr)
        return ferror(stream);
    else if(stream == stdin) 
        return 0;
    else {
        assert(0 && "ferror is not supported.");
        return 1;
    }
}

int fflush(FILE *stream);
int gemfflush(FILE *stream) {
    if(stream == stdout || stream == stderr) 
        return fflush(stream);
    else {
        assert(0 && "fflush is not supported.");
        return EOF;
    }
}

/* READING FUNCTIONS */

int gemfscanf(FILE *stream, const char *format, ...) {
        assert(0 && "fscanf is not supported.");
        return 0;
}

int gemvfscanf(FILE *stream, const char *format, va_list ap) {
        assert(0 && "vfscanf is not supported.");
        return 0;
}

size_t gemfread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
        if(stream == stdin) {
            long bytes;
            
            if(input_file_pos >= INPUT_FILE_LEN)
                return 0;

            if(input_file_pos + size*nmemb > INPUT_FILE_LEN) 
                bytes = INPUT_FILE_LEN - input_file_pos;
            else
                bytes = size * nmemb;

            /* the following trick is needed because Insieme will change
             * the data layout of input_file */
            const char* tmp = input_file;
            tmp += input_file_pos;

            memcpy(ptr, tmp, bytes);
            input_file_pos += bytes;

            return bytes;
        }
        else {
            assert(0 && "fread is not supported.");
            return 0; 
        }
}

int gemfgetc(FILE *stream) {
    int c, ret;
    ret = gemfread(&c, 1, 1, stream);

    return (ret == 1) ? c : EOF;
}

char *gemfgets(char *s, int size, FILE *stream) {
    assert(0 && "fgets is not supported.");
    return NULL; 
}

#define getc(stream) fgetc(stream)

int getchar(void) {
    int c, ret;
    ret = gemfread(&c, 1, 1, stdin);

    return (ret == 1) ? c : EOF;
}

char *gets(char *s) {
    assert(0 && "gets is not supported.");
    return NULL; 
}

int ungetc(int c, FILE *stream) {
    if(stream == stdin) {
        input_file_pos -= 1;
        return c;
    }
    else {
        assert(0 && "ungetc is not supported.");
        return EOF; 
    }
}

/* WRITING FUNCTIONS */

int vfprintf(FILE *stream, const char *format, va_list ap);
int gemfprintf(FILE *stream, char *format, ...) {
        if(stream == stdout || stream == stderr) {
            int ret;
            va_list args;
            va_start(args, format);
            ret = vfprintf(stream, format, args);
            va_end(args);
            return ret;
        }
        else
            return 0;
}

int gemvfprintf(FILE *stream, const char *format, va_list ap) {
        if(stream == stdout || stream == stderr)
            return vfprintf(stream, format, ap);
        else
            return 0; 
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t gemfwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
        if(stream == stdout || stream == stderr)
            return fwrite(ptr, size, nmemb, stream);
        else
            return 0; 
}

#define fprintf     gemfprintf
#define vfprintf    gemvfprintf
#define fwrite      gemfwrite

#define fscanf      gemfscanf
#define vfscanf     gemvfscanf
#define fread       gemfread
#define fgetc       gemfgetc
#define fgets       gemfgets
#define getchar     gemgetchar
#define gets        gemgets
#define ungetc      ungetc

#define feof        gemfeof
#define ferror      gemferror
#define fflush      gemfflush

#endif // __GEM_STDIO_H
