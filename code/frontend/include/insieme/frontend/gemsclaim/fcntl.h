#ifndef _GEM_FCNTL_H_
#define _GEM_FCNTL_H_

#define O_RDWR      0
#define O_RDONLY    1
#define O_WRONLY    2
#define O_APPEND    3
#define O_CREAT     4
#define O_TRUNC     5
#define O_EXCL      6

int  open(const char * path, int a, ...) {
        return -1;
}

#endif  //_GEM_FCNTL_H_
