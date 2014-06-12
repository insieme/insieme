#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>

int main() {
    XEvent xev;
    Display *display = XOpenDisplay("");

    XCheckMaskEvent(display, 0, &xev);

    if (XShmQueryExtension(display))
        return 0;
    else
        return 1;
}
