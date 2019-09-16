#define _BSD_SOURCE
#include <sys/ioctl.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define TTYDEFCHARS
#include <termios.h>
#include <sys/ttydefaults.h>
#undef TTYDEFCHARS

#if defined(__APPLE__)
#include <util.h>
#elif defined(__GLIBC__)
#include <pty.h>
#else /* bsd without glibc */
#include <libutil.h>
#endif

#include <HsFFI.h>

#include "HsBase.h"
#include "Rts.h"

// Rts internal API, not exposed in a public header file
extern void blockUserSignals(void);
extern void unblockUserSignals(void);

#include "fork_exec_with_pty.h"

/* Should be exported by unistd.h, but isn't on OSX. */
extern char **environ;

/* Fork and exec with a pty, returning the fd of the master pty. */
int
fork_exec_with_pty
    ( HsInt sx
    , HsInt sy
    , int search
    , const char *file
    , char *const argv[]
    , char *const env[]
    , HsInt *child_pid
    )
{
    int pty;
    int packet_mode = 1;
    struct winsize ws;

    /* Set the terminal size and settings. */
    memset(&ws, 0, sizeof ws);
    ws.ws_col = sx;
    ws.ws_row = sy;

    /* Fork and exec, returning the master pty. */
    blockUserSignals();
    stopTimer();

    *child_pid = forkpty(&pty, NULL, NULL, &ws);

    int ret = pty;

    switch (*child_pid) {
    case -1:
        unblockUserSignals();
        startTimer();

        return -1;
    case 0:
        /* Child process */
        unblockUserSignals();

        /* If an environment is specified, override the old one. */
        if (env) environ = (char**) env;

        /* Search user's path or not. */
        if (search) execvp(file, argv);
        else        execv(file, argv);

        perror("exec failed");
        exit(EXIT_FAILURE);
    default:
        /* Parent process */

        /* Switch the pty to packet mode, we'll deal with packeting on the
           haskell side of things. */
        if (ioctl(pty, TIOCPKT, &packet_mode) == -1) ret = 1;

        unblockUserSignals();
        startTimer();

        return ret;
    }
}
