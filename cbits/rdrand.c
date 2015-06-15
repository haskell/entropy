#ifdef HAVE_RDRAND

#include <stdint.h>
#include <stdlib.h>

int cpu_has_rdrand()
{
    uint32_t ax,bx,cx,dx,func=1;
    __asm__ volatile ("cpuid":\
            "=a" (ax), "=b" (bx), "=c" (cx), "=d" (dx) : "a" (func));
    return (cx & 0x40000000);
}

#ifdef arch_x86_64
// Returns 1 on success
static inline int _rdrand64_step(uint64_t *therand)
{
     unsigned char err;
     asm volatile("rdrand %0 ; setc %1"
                 : "=r" (*therand), "=qm" (err));
     return (int) err;
}

// Returns 0 on success, non-zero on failure.
int get_rand_bytes(uint8_t *therand, size_t len)
{
    int cnt;
    int fail=0;
    uint8_t *p = therand;
    uint8_t *end = therand + len;
    if((uint64_t)p%8 != 0) {
        uint64_t tmp;
        fail |= !_rdrand64_step(&tmp);
        while((uint64_t)p%8 != 0 && p != end) {
            *p = (uint8_t)(tmp & 0xFF);
            tmp = tmp >> 8;
            p++;
        }
    }
    for(; p <= end - sizeof(uint64_t); p+=sizeof(uint64_t)) {
        fail |= !_rdrand64_step((uint64_t *)p);
    }
    if(p != end) {
        uint64_t tmp;
        int cnt;
        fail |= !_rdrand64_step(&tmp);
        while(p != end) {
            *p = (uint8_t)(tmp & 0xFF);
            tmp = tmp >> 8;
            p++;
        }
    }
    return fail;
}
#endif /* x86-64 */

#ifdef arch_i386
// Returns 1 on success
static inline int _rdrand32_step(uint32_t *therand)
{
     unsigned char err;
     asm volatile("rdrand %0 ; setc %1"
                 : "=r" (*therand), "=qm" (err));
     return (int) err;
}

int get_rand_bytes(uint8_t *therand, size_t len)
{
    int cnt;
    int fail=0;
    uint8_t *p = therand;
    uint8_t *end = therand + len;
    if((uint32_t)p % sizeof(uint32_t) != 0) {
        uint32_t tmp;
        fail |= !_rdrand32_step(&tmp);
        while((uint32_t)p % sizeof(uint32_t) != 0 && p != end) {
            *p = (uint8_t)(tmp & 0xFF);
            tmp = tmp >> 8;
            p++;
        }
    }
    for(; p <= end - sizeof(uint32_t); p+=sizeof(uint32_t)) {
        fail |= !_rdrand32_step((uint32_t *)p);
    }
    if(p != end) {
        uint32_t tmp;
        int cnt;
        fail |= !_rdrand32_step(&tmp);
        while(p != end) {
            *p = (uint8_t)(tmp & 0xFF);
            tmp = tmp >> 8;
            p++;
        }
    }
    return fail;
}
#endif /* i386 */

#endif // RDRAND
