#ifndef __SET_SIZE_H__
#define __SET_SIZE_H__

#include <HsFFI.h>

int
set_pty_size(int fd, HsInt x, HsInt y);

int
get_pty_size(int fd, HsInt *x, HsInt *y);

#endif
