#include <HsFFI.h>
#include <stdlib.h>

/*static void my_enter(void) __attribute__((constructor));
static */void my_enter(void) {
    hs_init(NULL, NULL);
}

/*static void my_exit(void) __attribute__((destructor));
static */void my_exit(void) {
    hs_exit();
}
