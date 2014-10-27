AC_INIT(entropy,0.3.4,thomas.dubuisson@gmail.com)
AC_PROG_CC
AC_TRY_RUN([
#include <stdint.h>

int main()
{
  uint64_t therand;
  unsigned char err;
  asm volatile( "rdrand %0 ; setc %1"
              : "=r"(therand), "=qm"(err));
  return (!err);
}
], exists="yes", exists="no")
echo "checking if we can use RDRAND... ${exists}"

if test "${exists}" == "yes"; then
  AC_SUBST(RDRAND_CFLAG, "-DHAVE_RDRAND")
  AC_SUBST(RDRAND_GHCFLAG, "-optc-DHAVE_RDRAND")
fi

AC_CONFIG_FILES([entropy.buildinfo])
AC_OUTPUT
