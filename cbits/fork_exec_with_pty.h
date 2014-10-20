#ifndef __FORK_EXEC_WITH_PTY_H__
#define __FORK_EXEC_WITH_PTY_H__

#include <HsFFI.h>

int
fork_exec_with_pty(HsInt sx, HsInt sy, int search,
                   const char *file,
                   char *const argv[],
                   char *const env[],
                   HsInt *outpid);
#endif
