#ifndef ACK_ENGINE_H
#define ACK_ENGINE_H

#include <stdint.h>

// State of the brute-force Ackermann engine (ack.s). The engine keeps the
// stack of pending m values run-length encoded: runs 0..nruns-2 live in the
// vals/cnts arrays and the topmost run is cached in topval/topcnt. For A(4,1)
// the stack is 65,531 entries deep but never more than 4 runs.
//
// The field order is fixed: ack.s reads and writes these by offset.
typedef struct {
  uint32_t  m, n;               //  0,  4  current arguments; n holds the result when done
  uint8_t  *vals;               //  8      run values   (maxruns - 1 entries)
  uint32_t *cnts;               // 12      run lengths  (maxruns - 1 entries)
  uint32_t  nruns;              // 16      runs on the stack, including the cached top
  uint32_t  topval, topcnt;     // 20, 24
  uint32_t  depth, peak;        // 28, 32  entries on the stack now / most ever
  uint32_t  maxruns;            // 36
  uint32_t  steps_lo, steps_hi; // 40, 44  64-bit step counter
} ack_state_t;

enum { ACK_MORE = 0, ACK_DONE = 1, ACK_OVERFLOW = 2, ACK_RUNS = 3 };

// Run up to `budget` steps. Returns ACK_MORE if the budget ran out (call again
// to continue), ACK_DONE when finished (result in s->n), ACK_OVERFLOW if n
// would pass 2^32 - 1, ACK_RUNS if the stack needs more than maxruns runs.
int ack_run(ack_state_t *s, uint32_t budget);

#endif
