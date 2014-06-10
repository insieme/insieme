#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>

int main() {
    Display *display = NULL;

    if (XShmQueryExtension(display))
        return 1;

    return 0;
}
