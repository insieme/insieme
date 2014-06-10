#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>

int main() {
    Display *display = NULL;

    display = XOpenDisplay(NULL);
    if(!display) 
    	return 1;

    if (XShmQueryExtension(display))
        return 0;

    return 1;
}
