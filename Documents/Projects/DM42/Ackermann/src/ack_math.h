#ifndef ACK_MATH_H
#define ACK_MATH_H

#include <stdint.h>

// Closed forms. Return 0 and set *out when A(m,n) fits in 32 bits, else 1.
int ack_formula(uint32_t m, uint32_t n, uint32_t *out);

// Number of calls plain recursion (equivalently, steps of the brute-force
// engine) needs for A(m,n). Return 0 and set *out, or 1 if A(m,n) does not fit
// in 32 bits (or the count does not fit in 64).
int ack_bf_steps(uint32_t m, uint32_t n, uint64_t *out);

// Deepest the brute-force engine's stack gets for A(m,n). Return 0 and set *out
// when A(m,n) fits in 32 bits, else 1.
int ack_peak_depth(uint32_t m, uint32_t n, uint32_t *out);

// Memoized evaluation: for each m level only the farthest n reached is
// remembered (with its value and brute-force call count), and a later call with
// a larger n resumes the climb from there. Needs no libgcc: 64-bit add/compare only.
enum { MEMO_OK = 0, MEMO_OVERFLOW = 1, MEMO_ABORTED = 2 };

typedef struct {
  uint32_t value;
  uint64_t bf_steps;      // what the brute-force engine would have taken
  uint64_t real_steps;    // calls actually made here
} memo_result_t;

// poll() is called every 65536 real steps; return nonzero to abort.
int ack_memo(uint32_t m, uint32_t n, memo_result_t *r, int (*poll)(void));

// Estimated real steps of ack_memo (fits the measured growth; 0 = unknown/huge).
uint64_t ack_memo_estimate(uint32_t m, uint32_t n);

#endif
