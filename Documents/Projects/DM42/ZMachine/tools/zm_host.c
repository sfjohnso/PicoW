/* PC test harness for the Z-machine core: plays a story file on the console.
 *
 *   zm_host story.z3 [-v] [-s seed] [-autosave N] [-load] < commands.txt
 *
 * Input lines are read from stdin and echoed, so a transcript of a scripted run
 * reads like a terminal session. -v prints the status line to stderr.
 * Save files go to zm_save.dat in the current directory.
 *
 * -autosave N saves the game (at the input prompt) when the Nth line is asked
 * for, to zm_auto.dat, and quits; -load starts from zm_auto.dat. Together they
 * test the "resume at the prompt" state: a run split in two this way must read
 * exactly like an uninterrupted one. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../src/zmachine.h"

static FILE *story, *savef;
static int verbose;
static const char *save_name = "zm_save.dat";
static int autosave_at, reads_done;
static zm_t zstate;

static int h_read(void *ctx, uint32_t off, uint8_t *buf, uint32_t len) {
  (void)ctx;
  memset(buf, 0, len);
  if (fseek(story, (long)off, SEEK_SET)) return 0;       /* past the end: zeros */
  size_t n = fread(buf, 1, len, story);
  (void)n;
  return 0;
}
static void h_print(void *ctx, int c) { (void)ctx; putchar(c == 13 ? '\n' : c); }
static void h_status(void *ctx, const char *where, int a, int b, int is_time) {
  (void)ctx;
  if (verbose) fprintf(stderr, "[status] %s | %s %d %s %d\n", where, is_time ? "time" : "score", a, is_time ? ":" : "moves", b);
}
static int h_read_line(void *ctx, char *buf, int max) {
  (void)ctx;
  char line[512];
  fflush(stdout);
  if (autosave_at && ++reads_done == autosave_at) {
    save_name = "zm_auto.dat";
    int ok = zm_save_state(&zstate, 1);
    fprintf(stderr, "\n[autosave %s]\n", ok ? "ok" : "FAILED");
    return -1;
  }
  if (!fgets(line, sizeof line, stdin)) return -1;
  size_t n = strlen(line);
  while (n && (line[n - 1] == '\n' || line[n - 1] == '\r')) line[--n] = 0;
  printf("%s\n", line);
  if ((int)n > max) n = (size_t)max;
  memcpy(buf, line, n);
  return (int)n;
}
static int h_save_begin(void *ctx) { (void)ctx; savef = fopen(save_name, "wb"); return savef ? 0 : 1; }
static int h_save_write(void *ctx, const uint8_t *d, uint32_t n) { (void)ctx; return fwrite(d, 1, n, savef) != n; }
static void h_save_end(void *ctx, int ok) { (void)ctx; (void)ok; fclose(savef); savef = NULL; }
static int h_restore_begin(void *ctx) { (void)ctx; savef = fopen(save_name, "rb"); return savef ? 0 : 1; }
static int h_restore_read(void *ctx, uint8_t *d, uint32_t n) { (void)ctx; return fread(d, 1, n, savef) != n; }
static void h_restore_end(void *ctx) { (void)ctx; if (savef) fclose(savef); savef = NULL; }
static void h_message(void *ctx, const char *m) { (void)ctx; fflush(stdout); fprintf(stderr, "\n[interpreter: %s]\n", m); }

int main(int argc, char **argv) {
  const char *path = NULL;
  uint32_t seed = 12345;
  int load_auto = 0;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-v")) verbose = 1;
    else if (!strcmp(argv[i], "-s") && i + 1 < argc) seed = (uint32_t)atol(argv[++i]);
    else if (!strcmp(argv[i], "-autosave") && i + 1 < argc) autosave_at = atoi(argv[++i]);
    else if (!strcmp(argv[i], "-load")) load_auto = 1;
    else path = argv[i];
  }
  if (!path) { fprintf(stderr, "usage: zm_host story.z3 [-v] [-s seed] [-autosave N] [-load]\n"); return 2; }
  story = fopen(path, "rb");
  if (!story) { perror(path); return 2; }

  static uint8_t dyn[65536], cache[64 * ZM_PAGE];
  static uint16_t stack[8192];
  static zm_frame_t frames[512];
  static uint32_t ctag[64], cuse[64];
  zm_mem_t mem = { dyn, sizeof dyn, stack, 8192, frames, 512, cache, 64, ctag, cuse };
  zm_io_t io = { h_read, h_print, h_status, h_read_line, h_save_begin, h_save_write, h_save_end,
                 h_restore_begin, h_restore_read, h_restore_end, h_message };
  int e = zm_init(&zstate, &io, NULL, &mem, seed);
  if (e) { fprintf(stderr, "cannot start story: error %d (1 read, 2 not version 3, 3 memory)\n", e); return 1; }
  if (load_auto) {
    save_name = "zm_auto.dat";
    if (!zm_load_state(&zstate)) { fprintf(stderr, "could not load zm_auto.dat\n"); return 1; }
  }
  zm_run(&zstate);
  fflush(stdout);
  fprintf(stderr, "\n[%u instructions executed]\n", (unsigned)zstate.instructions);
  return 0;
}
