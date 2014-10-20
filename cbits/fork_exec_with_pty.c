#define _BSD_SOURCE

#include <sys/types.h>
#include <sys/ioctl.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define TTYDEFCHARS
#include <termios.h>
#ifdef __linux__
#include <pty.h>
#else /* bsd/apple */
#include <util.h>
#endif

#include <HsFFI.h>

#include "fork_exec_with_pty.h"

/* Should be exported by unistd.h, but isn't on OSX. */
extern char **environ;

/* Fork and exec with a pty, returning the fd of the master pty. */
int
fork_exec_with_pty(HsInt sx, HsInt sy, int search,
                   const char *file,
                   char *const argv[],
                   char *const env[],
		   HsInt *outpid)
{
    int pty;
    int packet_mode = 1;
    struct winsize ws;
    struct termios tio;

    /* Set the terminal size and settings. */
    memset(&ws, 0, sizeof ws);
    ws.ws_col = sx;
    ws.ws_row = sy;

    memset(&tio, 0, sizeof tio);
    tio.c_iflag = TTYDEF_IFLAG;
    tio.c_oflag = TTYDEF_OFLAG;
    tio.c_lflag = TTYDEF_LFLAG;
    tio.c_cflag = TTYDEF_CFLAG;
    memcpy(&tio.c_cc, ttydefchars, sizeof tio.c_cc);
    cfsetspeed(&tio, TTYDEF_SPEED);

    /* Fork and exec, returning the master pty. */
    switch ((*outpid = forkpty(&pty, NULL, &tio, &ws))) {
    case -1:
        return -1;
    case 0:
        /* If an environment is specified, override the old one. */
        if (env) environ = (char**) env;

        /* Search user's path or not. */
        if (search) execvp(file, argv);
        else        execv(file, argv);

        perror("exec failed");
        exit(EXIT_FAILURE);
    default:
        /* Switch the pty to packet mode, we'll deal with packeting on the
           haskell side of things. */
        if (ioctl(pty, TIOCPKT, &packet_mode) == -1) return 1;

        return pty;
    }
}
