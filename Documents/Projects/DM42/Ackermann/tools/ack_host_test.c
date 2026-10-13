/* Host-side checks for the Ackermann program (build: see README).
 * ack_run_c() is a line-for-line C twin of src/ack.s, so the algorithm,
 * the run-length stack and the slice/resume logic are tested here; the
 * assembly itself is checked on the calculator by its start-up self-test. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../src/ack_engine.h"
#include "../src/ack_math.h"

static int ack_run_c(ack_state_t *s, uint32_t budget) {
  uint32_t m = s->m, n = s->n, left = budget, nruns = s->nruns, topval = s->topval,
           topcnt = s->topcnt, depth = s->depth, peak = s->peak;
  int code;
  for (;;) {
    if (left == 0) { code = ACK_MORE; break; }
    left--;
    if (m == 0) {
      if (++n == 0) { code = ACK_OVERFLOW; break; }
      if (depth == 0) { code = ACK_DONE; break; }
      m = topval; depth--;
      if (--topcnt == 0) {
        if (--nruns != 0) { topval = s->vals[nruns - 1]; topcnt = s->cnts[nruns - 1]; }
      }
    } else if (n == 0) { m--; n = 1; }
    else {
      uint32_t v = m - 1;
      if (nruns == 0) { topval = v; topcnt = 1; nruns = 1; }
      else if (topval == v) topcnt++;
      else {
        s->vals[nruns - 1] = (uint8_t)topval; s->cnts[nruns - 1] = topcnt;
        if (++nruns > s->maxruns) { code = ACK_RUNS; break; }
        topval = v; topcnt = 1;
      }
      depth++; if (depth > peak) peak = depth;
      n--;
    }
  }
  s->m = m; s->n = n; s->nruns = nruns; s->topval = topval; s->topcnt = topcnt;
  s->depth = depth; s->peak = peak;
  uint64_t steps = ((uint64_t)s->steps_hi << 32 | s->steps_lo) + (budget - left);
  s->steps_lo = (uint32_t)steps; s->steps_hi = (uint32_t)(steps >> 32);
  return code;
}

static uint64_t plain_steps;
static uint64_t plain(uint64_t m, uint64_t n) {
  plain_steps++;
  if (!m) return n + 1;
  if (!n) return plain(m - 1, 1);
  return plain(m - 1, plain(m, n - 1));
}

static int fails;
#define CHECK(cond, ...) do { if (!(cond)) { fails++; printf("FAIL: "); printf(__VA_ARGS__); printf("\n"); } } while (0)

static uint8_t vals[64]; static uint32_t cnts[64];

static void run_engine(uint32_t m, uint32_t n, uint32_t slice, ack_state_t *s, int *rc) {
  memset(s, 0, sizeof *s);
  s->m = m; s->n = n; s->vals = vals; s->cnts = cnts; s->maxruns = 64;
  do { *rc = ack_run_c(s, slice); } while (*rc == ACK_MORE);
}

int main(int argc, char **argv) {
  int cases = 0, full = argc > 1 && !strcmp(argv[1], "full");
  /* 1. small cases against plain recursion, with several slice sizes */
  for (uint32_t m = 0; m <= 3; m++)
    for (uint32_t n = 0; n <= 9; n++) {
      if (m == 3 && n > 8) continue;
      plain_steps = 0; uint64_t pv = plain(m, n);
      uint32_t fv; int fo = ack_formula(m, n, &fv);
      uint64_t bs; int bo = ack_bf_steps(m, n, &bs);
      CHECK(!fo && fv == pv, "formula A(%u,%u)=%u want %llu", m, n, fv, (unsigned long long)pv);
      CHECK(!bo && bs == plain_steps, "bf_steps A(%u,%u)=%llu want %llu", m, n, (unsigned long long)bs, (unsigned long long)plain_steps);
      memo_result_t mr; int mo = ack_memo(m, n, &mr, NULL);
      CHECK(mo == MEMO_OK && mr.value == pv && mr.bf_steps == plain_steps, "memo A(%u,%u)", m, n);
      static const uint32_t slices[] = {1, 3, 7, 1000, 0xFFFFFFFFu};
      for (unsigned i = 0; i < 5; i++) {
        ack_state_t s; int rc; run_engine(m, n, slices[i], &s, &rc);
        uint64_t st = (uint64_t)s.steps_hi << 32 | s.steps_lo;
        CHECK(rc == ACK_DONE && s.n == pv && st == plain_steps, "engine A(%u,%u) slice %u: rc=%d n=%u steps=%llu", m, n, slices[i], rc, s.n, (unsigned long long)st);
        uint32_t pk; int po = ack_peak_depth(m, n, &pk);
        CHECK(!po && pk == s.peak, "peak depth A(%u,%u): predicted %u, engine %u", m, n, pk, s.peak);
      }
      cases++;
    }
  /* 2. the big ones: engine (steps, depth, runs), formula, steps and memo */
  struct { uint32_t m, n; } big[] = {{3, 10}, {3, 11}, {3, 12}, {3, 13}, {4, 0}, {4, 1}, {5, 0}};
  for (unsigned i = 0; i < sizeof big / sizeof big[0]; i++) {
    uint32_t m = big[i].m, n = big[i].n, fv;
    int fo = ack_formula(m, n, &fv); uint64_t bs; int bo = ack_bf_steps(m, n, &bs);
    memo_result_t mr; int mo = ack_memo(m, n, &mr, NULL);
    CHECK(!fo && !bo && mo == MEMO_OK, "big A(%u,%u) status", m, n);
    CHECK(mr.value == fv && mr.bf_steps == bs, "big A(%u,%u) memo value/steps (%u,%llu) vs (%u,%llu)", m, n, mr.value, (unsigned long long)mr.bf_steps, fv, (unsigned long long)bs);
    printf("A(%u,%u) = %u   brute-force steps %llu   memo real steps %llu\n", m, n, fv, (unsigned long long)bs, (unsigned long long)mr.real_steps);
    if (bs < 100000000ULL || (full && m == 4 && n == 1)) {   /* "full" also runs A(4,1): 2.9e9 steps */
      ack_state_t s; int rc; run_engine(m, n, 5000000, &s, &rc);
      uint64_t st = (uint64_t)s.steps_hi << 32 | s.steps_lo;
      CHECK(rc == ACK_DONE && s.n == fv && st == bs, "engine big A(%u,%u): rc=%d n=%u steps=%llu", m, n, rc, s.n, (unsigned long long)st);
      printf("   engine: %llu steps, peak depth %u\n", (unsigned long long)st, s.peak);
    }
  }
  /* 3. overflow classification */
  uint32_t fv;
  CHECK(ack_formula(4, 2, &fv) == 1 && ack_formula(5, 1, &fv) == 1 && ack_formula(6, 0, &fv) == 1 && ack_formula(3, 30, &fv) == 1, "overflow classification");
  CHECK(ack_formula(3, 29, &fv) == 0 && fv == 4294967293u, "A(3,29) = 2^32-3");
  { ack_state_t s; int rc; memset(&s, 0, sizeof s); s.m = 0; s.n = 0xFFFFFFFFu; s.vals = vals; s.cnts = cnts; s.maxruns = 64;
    rc = ack_run_c(&s, 10);
    CHECK(rc == ACK_OVERFLOW, "engine reports overflow for A(0,2^32-1), got %d", rc); }
  { ack_state_t s; int rc; run_engine(3, 6, 1000, &s, &rc); (void)rc;
    memset(&s, 0, sizeof s); s.m = 3; s.n = 6; s.vals = vals; s.cnts = cnts; s.maxruns = 2;
    do { rc = ack_run_c(&s, 1000); } while (rc == ACK_MORE);
    CHECK(rc == ACK_RUNS, "run-limit error with maxruns=2, got %d", rc); }
  printf("%d small cases checked, %s\n", cases, fails ? "FAILURES" : "all passed");
  return fails != 0;
}
