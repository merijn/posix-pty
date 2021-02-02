#include <sys/ioctl.h>

#include <string.h>

#include <HsFFI.h>

#include "pty_size.h"

int
set_pty_size(int fd, HsInt x, HsInt y, HsInt xpixel, HsInt ypixel)
{
    struct winsize ws;

    /* Set the terminal size and settings. */
    memset(&ws, 0, sizeof ws);
    ws.ws_col = x;
    ws.ws_row = y;
    ws.ws_xpixel = xpixel;
    ws.ws_ypixel = ypixel;

    return ioctl(fd, TIOCSWINSZ, &ws);
}

int
get_pty_size(int fd, HsInt *x, HsInt *y, HsInt *xpixel, HsInt *ypixel)
{
    int result;
    struct winsize ws;

    /* Set the terminal size and settings. */
    memset(&ws, 0, sizeof ws);
    result = ioctl(fd, TIOCGWINSZ, &ws);

    *x = ws.ws_col;
    *y = ws.ws_row;
    *xpixel = ws.ws_xpixel;
    *ypixel = ws.ws_ypixel;

    return result;
}

