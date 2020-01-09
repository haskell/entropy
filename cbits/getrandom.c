#ifdef HAVE_GETRANDOM

#define _GNU_SOURCE
#include <errno.h>

#ifdef HAVE_LIBC_GETRANDOM
#include <sys/random.h>
#else

#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <linux/random.h>

#ifndef SYS_getrandom
#define SYS_getrandom __NR_getrandom
#endif

static ssize_t getrandom(void* buf, size_t buflen, unsigned int flags)
{
    return syscall(SYS_getrandom, buf, buflen, flags);
}

#endif

int system_has_getrandom()
{
    char tmp;
    return getrandom(&tmp, sizeof(tmp), GRND_NONBLOCK) != -1 || errno != ENOSYS;
}

// Returns 0 on success, non-zero on failure.
int entropy_getrandom(unsigned char* buf, size_t len)
{
    while (len) {
        ssize_t bytes_read = getrandom(buf, len, 0);

        if (bytes_read == -1) {
            if (errno != EINTR)
                return -1;
            else
                continue;
        }

        len -= bytes_read;
        buf += bytes_read;
    }

    return 0;
}

#endif
