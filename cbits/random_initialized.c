#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef HAVE_GETENTROPY
#ifndef DO_NOT_USE_GET_ENTROPY
static int ensure_pool_initialized_getentropy()
{
    char tmp;
    return getentropy(&tmp, sizeof(tmp));
}
#endif
#endif

// Poll /dev/random to wait for randomness. This is a proxy for the /dev/urandom
// pool being initialized.
static int ensure_pool_initialized_poll()
{
    struct pollfd pfd;
    int dev_random = open("/dev/random", O_RDONLY);
    if (dev_random == -1)
        return -1;

    pfd.fd = dev_random;
    pfd.events = POLLIN;
    pfd.revents = 0;

    while (1) {
        int ret = poll(&pfd, 1, -1);
        if (ret < 0 && (errno == EAGAIN || errno == EINTR))
            continue;
        if (ret != 1) {
            close(dev_random);
            errno = EIO;
            return -1;
        }

        break;
    }

    return close(dev_random);
}

// Returns 0 on success, non-zero on failure.
int ensure_pool_initialized()
{
#ifdef HAVE_GETENTROPY
#ifndef DO_NOT_USE_GET_ENTROPY
    if (ensure_pool_initialized_getentropy() == 0)
        return 0;
#endif
#endif

    return ensure_pool_initialized_poll();
}
