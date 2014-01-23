#ifndef _GEM_UNISTD_H_
#define _GEM_UNISTD_H_

long read(int fd, void *buf, size_t count) {
    if(fd == 0)
        return (long)fread(buf, (size_t)1, count, stdin);

    fprintf(stderr, "UNHANDLED call to function %s\n", __func__);
    return (long)-1;
}

int unlink(const char *pathname) {
    fprintf(stderr, "UNHANDLED call to function %s\n", __func__);
    return -1;
}

long write(int fd, const void *buf, size_t  count) {
    if(fd == 1) 
        return (long)fwrite(buf, (size_t)1, count, stdout);
    else if(fd == 2)
        return (long)fwrite(buf, (size_t)1, count, stderr);

    fprintf(stderr, "UNHANDLED call to function %s\n", __func__);
    return (long)-1;
}

unsigned long lseek(int fd, unsigned long offset, int whence) {
    fprintf(stderr, "UNHANDLED call to function %s\n", __func__);
    return -1;
}

int close(int fd) {
    if(fd == 0)
        return (!fclose(stdin)) ? 0 : -1;
    else if(fd == 1)
        return (!fclose(stdout)) ? 0 : -1;
    else if(fd == 2)
        return (!fclose(stderr)) ? 0 : -1;

    fprintf(stderr, "UNHANDLED call to function %s\n", __func__);
    return -1;
}

#endif // _GEM_UNISTD_H_
