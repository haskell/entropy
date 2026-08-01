#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.73])
AC_INIT([entropy], [0.4.1.11], [https://github.com/TomMD/entropy/issues], [https://hackage.haskell.org/package/entropy-0.4.1.11/entropy-0.4.1.11.tar.gz], [https://hackage.haskell.org/package/entropy])
AC_CONFIG_SRCDIR([cbits/rdrand.h])
AC_CONFIG_HEADERS([cbits/config.h])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stdint.h unistd.h errno.h sys/random.h sys/syscall.h sys/types.h linux/types.h])
AC_CHECK_HEADER([linux/random.h], [AC_DEFINE([HAVE_LINUX_RANDOM_H],[1],[Define to 1 if you have the <linux/random.h> header file.])], [],
[AC_INCLUDES_DEFAULT[
#ifdef HAVE_LINUX_TYPES_H
# include <linux/types.h>
#endif
]])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_LANG(C)

AC_MSG_CHECKING([for rdrand assembly instruction])
rdrandexists=no
AC_RUN_IFELSE(
  [AC_LANG_PROGRAM([
#include <stdint.h>
],
[  uint64_t therand;
   unsigned char err;
   asm volatile("rdrand %0 ; setc %1"
     : "=r" (therand), "=qm" (err));
   return (!err);
  ])],
  [AC_DEFINE([HAVE_RDRAND], [1], [Define if the assembly instruction rdrand exists.])
   rdrandexists=yes
  ])
AC_MSG_RESULT([$rdrandexists])

AC_CHECK_FUNC([getrandom],
  [AC_DEFINE([HAVE_LIBC_GETRANDOM],[1], [Define if getrandom from sys/random.h exists.])
   AC_DEFINE([HAVE_GETRANDOM],[1], [Define if getrandom either from libc / via linux syscalls is available.])
  ],
  [ AC_MSG_CHECKING([for SYS_getrandom syscall])
    AC_RUN_IFELSE(
      [AC_LANG_PROGRAM([
#define _GNU_SOURCE

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_SYSCALL_H
# include <sys/syscall.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef HAVE_LINUX_RANDOM_H
# include <linux/random.h>
#endif

static ssize_t getrandom(void* buf, size_t buflen, unsigned int flags);

static ssize_t getrandom(void* buf, size_t buflen, unsigned int flags)
{
    return syscall(SYS_getrandom, buf, buflen, flags);
}
    ],
    [
errno = 0;
ssize_t result;
char tmp;
result = getrandom(&tmp, sizeof(tmp), GRND_NONBLOCK);
return result == -1 && errno == ENOSYS;
    ]
    )],
    [AC_DEFINE([HAVE_GETRANDOM],[1], [Define if getrandom either from libc / via linux syscalls is available.])
    ]
    )
    ])

AC_CHECK_FUNC([getentropy],
  [AC_DEFINE([HAVE_GETENTROPY],[1], [Define if the function getentropy exists.])
  ])

AC_OUTPUT
