#ifndef rdrand_h
#include <stdint.h>

#ifdef arch_x86_64
int cpu_has_rdrand()

// Returns 0 on success, non-zero on failure.
int get_rand_bytes(uint8_t *therand, size_t len)
#endif /* arch_x86_64 */
#endif // rdrand_h
