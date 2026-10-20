#ifndef ZMACHINE_H
#define ZMACHINE_H

// A small Z-machine interpreter (version 3: the Infocom games Zork I-III,
// Starcross, Planetfall, ...). Portable C with no library dependencies: the
// host supplies all I/O through zm_io_t and all memory through zm_mem_t, so the
// same code runs on a PC test harness and on the DM42.
//
// Only the writable ("dynamic") part of the story file is held in RAM. The rest
// is read on demand, a page at a time, through io.read() and a small cache.

#include <stdint.h>

#define ZM_PAGE 512                      // cache page size (power of two)

typedef struct zm zm_t;

typedef struct {
  // Read len bytes of the story file starting at offset off into buf; bytes past
  // the end of the file must read as zero. Return 0 on success.
  int  (*read)(void *ctx, uint32_t off, uint8_t *buf, uint32_t len);
  // Print one ZSCII character to the main window (13 = new line).
  void (*print)(void *ctx, int zscii);
  // Called before each line of input (version 3 status line). If is_time is set,
  // a and b are hours and minutes, otherwise score and turns.
  void (*status)(void *ctx, const char *where, int a, int b, int is_time);
  // Read a line of input into buf (at most max characters, no terminator needed).
  // Return its length, or -1 to quit the game.
  int  (*read_line)(void *ctx, char *buf, int max);
  // Save game: save_begin() returns 0 if the user went ahead; then save_write()
  // is called with successive chunks; save_end(ok) closes it. Restore is the same.
  int  (*save_begin)(void *ctx);
  int  (*save_write)(void *ctx, const uint8_t *data, uint32_t len);   // 0 = ok
  void (*save_end)(void *ctx, int ok);
  int  (*restore_begin)(void *ctx);
  int  (*restore_read)(void *ctx, uint8_t *data, uint32_t len);       // 0 = ok
  void (*restore_end)(void *ctx);
  // Diagnostics (errors, "game over"); may be NULL.
  void (*message)(void *ctx, const char *msg);
} zm_io_t;

typedef struct {
  uint32_t ret_pc;                       // where to continue when this routine returns
  uint32_t base;                         // index in the stack of this routine's first local
  uint8_t  nloc;                         // number of locals
  uint8_t  store;                        // variable that receives the return value
} zm_frame_t;

// Memory the host hands to the interpreter.
typedef struct {
  uint8_t    *dyn;        uint32_t dyn_cap;       // dynamic memory (must hold header word 0x0E bytes)
  uint16_t   *stack;      uint32_t stack_words;   // evaluation stack and locals
  zm_frame_t *frames;     uint32_t frame_cap;     // call frames
  uint8_t    *cache;      uint32_t cache_pages;   // cache_pages * ZM_PAGE bytes
  uint32_t   *cache_tag;  uint32_t *cache_use;    // cache_pages entries each
} zm_mem_t;

struct zm {
  zm_io_t   io;
  void     *ctx;
  uint8_t  *dyn;     uint32_t dyn_size, dyn_cap;
  uint8_t  *cache;   uint32_t *ctag, *cuse; uint32_t npages, tick, last_tag; uint8_t *last_page;
  uint16_t *stk;     uint32_t stk_cap, sp;
  zm_frame_t *fr;    uint32_t fr_cap, nfr;
  uint32_t  pc, initial_pc, save_pc;
  uint8_t   version;
  uint32_t  file_len, high_base, static_base, globals, objtab, dict, abbrev;
  int       running, screen_on;
  struct { uint32_t table, count; } out3[16];
  int       out3_n;
  char     *cap; int capn, capmax;        // capture buffer (status line)
  uint32_t  rng; int rng_fixed;
  uint32_t  instructions;
  uint16_t  cur_text, cur_parse;           // buffers of the read in progress
  int       in_read;                       // inside a line read (so a save can resume there)
  int       resume_read;                   // after a load: take the line read again
  uint16_t  resume_text, resume_parse;
};

enum { ZM_OK = 0, ZM_E_READ = 1, ZM_E_VERSION = 2, ZM_E_MEMORY = 3 };

// Set up from the story file. seed seeds the random number generator.
int  zm_init(zm_t *z, const zm_io_t *io, void *ctx, const zm_mem_t *mem, uint32_t seed);

// Run until the game quits or an error stops it.
void zm_run(zm_t *z);

// Save the complete state through io.save_*. If at_input is nonzero (only legal
// while io.read_line() is running) the saved game resumes at that input prompt.
// Returns nonzero on success.
int  zm_save_state(zm_t *z, int at_input);

// Load a state written by zm_save_state() through io.restore_*. Returns nonzero
// on success; call zm_run() afterwards to carry on.
int  zm_load_state(zm_t *z);

// Dictionary access for word completion. Words are lower case, up to 9 letters.
int  zm_dict_count(zm_t *z);
void zm_dict_word(zm_t *z, int index, char out[12]);

#endif
