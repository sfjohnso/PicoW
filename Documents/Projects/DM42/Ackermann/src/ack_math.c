#include "ack_math.h"

typedef uint64_t u64;

int ack_formula(uint32_t m, uint32_t n, uint32_t *out) {
  switch (m) {
  case 0: if (n == 0xFFFFFFFFu) return 1; *out = n + 1; return 0;
  case 1: if (n > 0xFFFFFFFDu) return 1; *out = n + 2; return 0;
  case 2: if (n > 2147483646u) return 1; *out = 2 * n + 3; return 0;
  case 3: if (n > 29) return 1; *out = (uint32_t)(((u64)1 << (n + 3)) - 3); return 0;
  case 4: if (n == 0) { *out = 13; return 0; }
          if (n == 1) { *out = 65533; return 0; }
          return 1;
  case 5: if (n == 0) { *out = 65533; return 0; }
          return 1;
  default: return 1;
  }
}

// Calls made by A(2,x): 2x^2 + 7x + 5 (x < 2^32, so this fits in 64 bits).
static u64 calls2(u64 x) { return 2 * x * x + 7 * x + 5; }

// Calls made by A(3,n), by C(3,n) = 1 + C(3,n-1) + C(2, A(3,n-1)).
static int calls3(uint32_t n, u64 *out) {
  u64 c = 15;                                  // C(3,0) = 1 + C(2,1)
  for (uint32_t k = 1; k <= n; k++) {
    u64 prev = c;
    c = 1 + c + calls2(((u64)1 << (k + 2)) - 3);   // A(3,k-1) = 2^(k+2) - 3
    if (c < prev) return 1;
  }
  *out = c;
  return 0;
}

int ack_bf_steps(uint32_t m, uint32_t n, uint64_t *out) {
  uint32_t v;
  if (ack_formula(m, n, &v)) return 1;
  switch (m) {
  case 0: *out = 1; return 0;
  case 1: *out = 2 * (u64)n + 2; return 0;
  case 2: *out = calls2(n); return 0;
  case 3: return calls3(n, out);
  case 4: {
    u64 c3;
    if (n == 0) { if (calls3(1, &c3)) return 1; *out = 1 + c3; return 0; }   // 1 + C(3,1)
    // n == 1: C(4,1) = 1 + C(4,0) + C(3, A(4,0) = 13)
    u64 c40 = 0; if (calls3(1, &c3)) return 1; c40 = 1 + c3;
    if (calls3(13, &c3)) return 1;
    *out = 1 + c40 + c3;
    return 0;
  }
  case 5: {                                       // C(5,0) = 1 + C(4,1)
    u64 c41; if (ack_bf_steps(4, 1, &c41)) return 1;
    *out = 1 + c41; return 0;
  }
  default: return 1;
  }
}

int ack_peak_depth(uint32_t m, uint32_t n, uint32_t *out) {
  uint32_t v;
  if (ack_formula(m, n, &v)) return 1;
  switch (m) {
  case 0: *out = 0; return 0;
  case 1: *out = n; return 0;
  case 2: *out = 2 * n + 1; return 0;
  case 3: *out = (uint32_t)(((u64)1 << (n + 3)) - 5); return 0;
  case 4: *out = n == 0 ? 11 : 65531; return 0;
  case 5: *out = 65531; return 0;
  default: return 1;
  }
}

// ---- memoized chain --------------------------------------------------------

typedef struct { u64 v, c; } vc_t;

#define MAXLEVEL 8
static u64      hw_n[MAXLEVEL];
static vc_t     hw[MAXLEVEL];
static uint8_t  hw_ok[MAXLEVEL];
static u64      real_steps;
static int      abort_code;
static int    (*poll_fn)(void);

static vc_t chain(uint32_t m, u64 n) {
  vc_t zero = {0, 0};
  if (abort_code) return zero;
  real_steps++;
  if ((real_steps & 0xFFFF) == 0 && poll_fn && poll_fn()) { abort_code = MEMO_ABORTED; return zero; }

  if (m == 0) { vc_t r = {n + 1, 1}; return r; }
  if (m >= MAXLEVEL) { abort_code = MEMO_OVERFLOW; return zero; }
  if (hw_ok[m] && n == hw_n[m]) return hw[m];

  u64 j;
  vc_t cur;
  if (hw_ok[m] && n > hw_n[m]) {
    j = hw_n[m]; cur = hw[m];
  } else {
    vc_t low = chain(m - 1, 1);
    if (abort_code) return zero;
    cur.v = low.v; cur.c = 1 + low.c; j = 0;
    if (!hw_ok[m]) { hw_ok[m] = 1; hw_n[m] = 0; hw[m] = cur; }
  }
  while (j < n) {
    vc_t low = chain(m - 1, cur.v);
    if (abort_code) return zero;
    cur.c = 1 + cur.c + low.c;                   // C(m,j+1) = 1 + C(m,j) + C(m-1, A(m,j))
    cur.v = low.v;
    j++;
    if (cur.v > 0xFFFFFFFFull) { abort_code = MEMO_OVERFLOW; return zero; }
    if (j > hw_n[m]) { hw_n[m] = j; hw[m] = cur; }
  }
  return cur;
}

int ack_memo(uint32_t m, uint32_t n, memo_result_t *r, int (*poll)(void)) {
  for (int i = 0; i < MAXLEVEL; i++) hw_ok[i] = 0;
  real_steps = 0; abort_code = 0; poll_fn = poll;
  vc_t res = chain(m, n);
  r->real_steps = real_steps;
  if (abort_code) return abort_code;
  r->value = (uint32_t)res.v;
  r->bf_steps = res.c;
  return MEMO_OK;
}

uint64_t ack_memo_estimate(uint32_t m, uint32_t n) {
  switch (m) {
  case 0: return 1;
  case 1: return n + 3;
  case 2: return 4 * (u64)n + 4;
  case 3: return n > 40 ? 0 : (12 * ((u64)1 << n));
  case 4: return n == 0 ? 22 : (n == 1 ? 98315 : 0);
  case 5: return n == 0 ? 98316 : 0;
  default: return 0;
  }
}
