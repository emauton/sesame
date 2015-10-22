/* A simple port program to allow Erlang to read and write a PF_KEY socket.
 *
 * We expect data from Erlang on stdin with a 2B length prefix, and pass it on
 * to the PF_KEY socket.
 *
 * We add a 2B length prefix to data from the PF_KEY socket and pass it back to
 * Erlang on stdout.
 *
 * See http://www.erlang.org/doc/tutorial/c_port.html */

#include <errno.h>
#include <ev.h>
#include <fcntl.h>
#include <linux/pfkeyv2.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#define SIZE 32768

int pf_key_sock;  /* We maintain one PF_KEY socket, opened in main() and
                     written in stdin_cb(). */

/** Supporting functions. */

/* Return a PF_KEY socket or crash. */
int open_pf_key(void);

/* Set an FD nonblocking or crash. */
void set_nonblocking(int fd);

/* Read some data successfully or crash.
 * For nonblocking FDs we expect to read "forever", so crash on EOF.
 * Returns either the number of bytes read or -1 on EAGAIN.
 * https://en.wikipedia.org/wiki/Read_or_Die :o) */
ssize_t read_or_die(int fd, char *buf, size_t len);

/* Write all data passed or crash.
 * Returns either the number of bytes read or -1 on EAGAIN. */
void write_or_die(int fd, char *buf, size_t len);

/* Syslog a formatted message and crash. */
void log_exit(char *fmt, ...);

/** Event callbacks. */

/* Called when stdin is ready to read.
 * Read data from stdin, strip out length, and pass to PF_KEY. */
void stdin_cb(EV_P_ ev_io *w, int revents) {
    char buf[SIZE];
    ssize_t nr, len;

    nr = read_or_die(w->fd, buf, SIZE);
    if (nr == -1)
        return;

    /* 2B length header. MSB first.
     * See http://www.erlang.org/doc/man/erlang.html#open_port-2 */
    len = (buf[0] << 8) | buf[1];

    /* Shift the write over to strip length header. */
    write_or_die(pf_key_sock, buf + 2, len);
}

/* Called when PF_KEY is ready to read.
 * Read data from PF_KEY, add length, and pass to stdout. */
void pf_key_cb(EV_P_ ev_io *w, int revents) {
    char buf[SIZE];
    ssize_t nr;

    /* Shift the read over to leave room for length header. */
    nr = read_or_die(w->fd, buf + 2, SIZE - 2);
    if (nr == -1)
        return;

    /* 2B length header. MSB first.
     * See http://www.erlang.org/doc/man/erlang.html#open_port-2 */
    buf[0] = (nr >> 8) & 0xff;
    buf[1] = nr & 0xff;

    write_or_die(STDOUT_FILENO, buf, nr);
}

/* Initialise syslog, nonblocking FDs and PF_KEY socket, then loop forever in
 * libev, blitting data back and forth between standard I/O and PF_KEY. */
int main(void) {
    struct ev_loop *loop = EV_DEFAULT;
    ev_io stdin_watcher;
    ev_io pf_key_watcher;

    openlog("pfkeyport", LOG_PID | LOG_NDELAY, LOG_AUTH);
    set_nonblocking(STDIN_FILENO);
    set_nonblocking(STDOUT_FILENO);
    pf_key_sock = open_pf_key();

    ev_io_init(&stdin_watcher, stdin_cb, STDIN_FILENO, EV_READ);
    ev_io_start(loop, &stdin_watcher);
    ev_io_init(&pf_key_watcher, pf_key_cb, pf_key_sock, EV_READ);
    ev_io_start(loop, &pf_key_watcher);
    ev_run(loop, 0);

    closelog();
    return 0;
}

/** Supporting function definitions. See commentary above.
 *  Read & write are via Love's "Linux System Programming". */

int open_pf_key(void) {
    int fd = socket(PF_KEY, SOCK_RAW | SOCK_NONBLOCK, PF_KEY_V2);
    if (fd == -1)
        log_exit("opening pf_key socket: %s", strerror(errno));
    return fd;
}

void set_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL);
    if (flags == -1)
        log_exit("getting fd %d flags: %s", fd, strerror(errno));

    flags = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    if (flags == -1)
        log_exit("setting fd %d nonblocking: %s", fd, strerror(errno));
}

ssize_t read_or_die(int fd, char *buf, size_t len) {
    ssize_t nr;

start:
    nr = read(fd, buf, len);

    if (nr == -1) {
        if (errno == EINTR)
            goto start;  /* rlove says it's OK! */
        if (errno == EAGAIN)
            return -1;  /* We can read when ev calls back again. */
        else
            log_exit("reading from fd %d: %s", fd, strerror(errno));
    }

    if (nr == 0)
        log_exit("reading from fd %d: EOF", fd);

    return nr;
}

void write_or_die(int fd, char *buf, size_t len) {
    ssize_t nw;

    while (len != 0 && (nw = write(fd, buf, len)) != 0) {
        if (nw == -1) {
            if (errno == EINTR)
                continue;
            else
                log_exit("writing to fd %d: %s", fd, strerror(errno));
        }
        len -= nw;
        buf += nw;
    }
}

void log_exit(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsyslog(LOG_WARNING, fmt, args);
    va_end(args);
    exit(EXIT_FAILURE);
}

/* Copyright 2015 Cian Synnott
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License. */
