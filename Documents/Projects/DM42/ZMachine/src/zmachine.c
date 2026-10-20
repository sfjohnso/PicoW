// A small Z-machine interpreter, version 3. See zmachine.h.
//
// References: the Z-machine Standards Document 1.1 (memory map, instruction
// formats, object table, Z-character encoding, opcode list).

#include "zmachine.h"

// ---- small helpers (no libc) ---------------------------------------------

static void zero(void *p, uint32_t n) { uint8_t *b = p; while (n--) *b++ = 0; }

static void zfail(zm_t *z, const char *msg) {
  if (z->io.message) z->io.message(z->ctx, msg);
  z->running = 0;
}

// ---- memory ---------------------------------------------------------------

static uint8_t page_byte(zm_t *z, uint32_t a) {
  uint32_t tag = a / ZM_PAGE;
  if (tag == z->last_tag) return z->last_page[a & (ZM_PAGE - 1)];

  uint32_t victim = 0, oldest = 0xFFFFFFFFu;
  for (uint32_t i = 0; i < z->npages; i++) {
    if (z->ctag[i] == tag) {
      z->cuse[i] = ++z->tick;
      z->last_tag = tag;
      z->last_page = z->cache + i * ZM_PAGE;
      return z->last_page[a & (ZM_PAGE - 1)];
    }
    if (z->cuse[i] < oldest) { oldest = z->cuse[i]; victim = i; }
  }
  uint8_t *slot = z->cache + victim * ZM_PAGE;
  if (z->io.read(z->ctx, tag * ZM_PAGE, slot, ZM_PAGE)) {
    z->ctag[victim] = 0xFFFFFFFFu;
    zfail(z, "story file read error");
    return 0;
  }
  z->ctag[victim] = tag;
  z->cuse[victim] = ++z->tick;
  z->last_tag = tag;
  z->last_page = slot;
  return slot[a & (ZM_PAGE - 1)];
}

static inline uint8_t rd8(zm_t *z, uint32_t a) {
  return a < z->dyn_size ? z->dyn[a] : page_byte(z, a);
}
static inline uint16_t rd16(zm_t *z, uint32_t a) {
  return (uint16_t)((rd8(z, a) << 8) | rd8(z, a + 1));
}
static inline void wr8(zm_t *z, uint32_t a, uint8_t v) {
  if (a < z->dyn_size) z->dyn[a] = v;          // writes outside dynamic memory are ignored
}
static inline void wr16(zm_t *z, uint32_t a, uint16_t v) {
  wr8(z, a, (uint8_t)(v >> 8)); wr8(z, a + 1, (uint8_t)v);
}

// ---- stack, frames and variables -----------------------------------------

static void push(zm_t *z, uint16_t v) {
  if (z->sp >= z->stk_cap) { zfail(z, "stack overflow"); return; }
  z->stk[z->sp++] = v;
}

static uint32_t eval_base(zm_t *z) { zm_frame_t *f = &z->fr[z->nfr - 1]; return f->base + f->nloc; }

static uint16_t pop(zm_t *z) {
  if (z->sp <= eval_base(z)) { zfail(z, "stack underflow"); return 0; }
  return z->stk[--z->sp];
}

static uint16_t getvar(zm_t *z, uint8_t v) {
  if (v == 0) return pop(z);
  if (v < 16) {
    zm_frame_t *f = &z->fr[z->nfr - 1];
    if (v - 1 >= f->nloc) { zfail(z, "bad local variable"); return 0; }
    return z->stk[f->base + v - 1];
  }
  return rd16(z, z->globals + 2u * (v - 16));
}

static void setvar(zm_t *z, uint8_t v, uint16_t val) {
  if (v == 0) { push(z, val); return; }
  if (v < 16) {
    zm_frame_t *f = &z->fr[z->nfr - 1];
    if (v - 1 >= f->nloc) { zfail(z, "bad local variable"); return; }
    z->stk[f->base + v - 1] = val;
    return;
  }
  wr16(z, z->globals + 2u * (v - 16), val);
}

// "Indirect" variable references (load, store, inc, dec, inc_chk, dec_chk, pull):
// variable 0 means the top of the stack, read or replaced in place.
static uint16_t peekvar(zm_t *z, uint8_t v) {
  if (v == 0) {
    if (z->sp <= eval_base(z)) { zfail(z, "stack underflow"); return 0; }
    return z->stk[z->sp - 1];
  }
  return getvar(z, v);
}

static void pokevar(zm_t *z, uint8_t v, uint16_t val) {
  if (v == 0) {
    if (z->sp <= eval_base(z)) { zfail(z, "stack underflow"); return; }
    z->stk[z->sp - 1] = val;
    return;
  }
  setvar(z, v, val);
}

// ---- output -----------------------------------------------------------------

static void zput(zm_t *z, int c) {
  if (z->cap) {
    if (z->capn < z->capmax - 1) z->cap[z->capn++] = (char)(c == 13 ? ' ' : c);
    return;
  }
  if (z->out3_n) {                               // output stream 3: into a table in memory
    uint32_t t = z->out3[z->out3_n - 1].table, n = z->out3[z->out3_n - 1].count;
    wr8(z, t + 2 + n, (uint8_t)c);
    z->out3[z->out3_n - 1].count = n + 1;
    return;
  }
  if (z->screen_on) z->io.print(z->ctx, c);
}

static void zput_zscii(zm_t *z, int c) {
  if (c == 13 || (c >= 32 && c <= 126)) zput(z, c);
  // ZSCII 0 prints nothing; the extra characters (155..251) do not occur in version 3
}

static void zput_num(zm_t *z, int16_t v) {
  char b[8]; int n = 0;
  int32_t x = v;
  if (x < 0) { zput(z, '-'); x = -x; }
  do { b[n++] = (char)('0' + x % 10); x /= 10; } while (x);
  while (n) zput(z, b[--n]);
}

// ---- Z-characters --------------------------------------------------------------

static const char A0[] = "abcdefghijklmnopqrstuvwxyz";
static const char A1[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
static const char A2[] = "0123456789.,!?_#'\"/\\-:()";    // Z-characters 8..31

// Print the Z-string at addr and return the address after it.
static uint32_t print_zstr(zm_t *z, uint32_t addr, int allow_abbrev) {
  int shift = 0, abbrev = 0, esc = 0, hi = 0;
  for (;;) {
    if (!z->running) return addr;
    uint16_t w = rd16(z, addr);
    addr += 2;
    for (int i = 0; i < 3; i++) {
      int c = (w >> (10 - 5 * i)) & 31;
      if (esc == 1) { hi = c; esc = 2; continue; }
      if (esc == 2) { zput_zscii(z, (hi << 5) | c); esc = 0; continue; }
      if (abbrev) {
        if (allow_abbrev) {
          uint32_t entry = z->abbrev + 2u * (32 * (abbrev - 1) + c);
          print_zstr(z, (uint32_t)rd16(z, entry) * 2, 0);
        }
        abbrev = 0;
        continue;
      }
      if (c == 0) { zput(z, ' '); shift = 0; continue; }
      if (c <= 3) { abbrev = c; continue; }
      if (c == 4) { shift = 1; continue; }
      if (c == 5) { shift = 2; continue; }
      int alpha = shift; shift = 0;
      if (alpha == 0) zput(z, A0[c - 6]);
      else if (alpha == 1) zput(z, A1[c - 6]);
      else if (c == 6) esc = 1;
      else if (c == 7) zput(z, 13);
      else zput(z, A2[c - 8]);
    }
    if (w & 0x8000) return addr;
  }
}

// Encode a word the way dictionary entries are stored (version 3: 6 Z-characters).
static void encode_word(const char *w, int len, uint8_t out[4]) {
  uint8_t zc[16];
  int n = 0;
  for (int i = 0; i < len && n < 6; i++) {
    char c = w[i];
    if (c >= 'a' && c <= 'z') { zc[n++] = (uint8_t)(c - 'a' + 6); continue; }
    int k = -1;
    for (int j = 0; A2[j]; j++) if (A2[j] == c) { k = j; break; }
    if (k >= 0) { zc[n++] = 5; zc[n++] = (uint8_t)(k + 8); }
    else { zc[n++] = 5; zc[n++] = 6; zc[n++] = (uint8_t)((c >> 5) & 31); zc[n++] = (uint8_t)(c & 31); }
  }
  while (n < 6) zc[n++] = 5;
  uint16_t w0 = (uint16_t)((zc[0] << 10) | (zc[1] << 5) | zc[2]);
  uint16_t w1 = (uint16_t)((zc[3] << 10) | (zc[4] << 5) | zc[5] | 0x8000);
  out[0] = (uint8_t)(w0 >> 8); out[1] = (uint8_t)w0; out[2] = (uint8_t)(w1 >> 8); out[3] = (uint8_t)w1;
}

// ---- dictionary and tokenising ------------------------------------------------

static int is_separator(zm_t *z, uint8_t c) {
  uint32_t n = rd8(z, z->dict);
  for (uint32_t i = 0; i < n; i++) if (rd8(z, z->dict + 1 + i) == c) return 1;
  return 0;
}

static uint16_t dict_lookup(zm_t *z, const char *w, int len) {
  uint8_t key[4];
  encode_word(w, len, key);
  uint32_t nsep = rd8(z, z->dict);
  uint32_t elen = rd8(z, z->dict + 1 + nsep);
  int16_t count = (int16_t)rd16(z, z->dict + 2 + nsep);
  uint32_t base = z->dict + 4 + nsep;

  int linear = count < 0;
  int32_t n = linear ? -count : count;
  int32_t lo = 0, hi = n - 1;
  while (lo <= hi) {
    int32_t mid = linear ? lo : (lo + hi) / 2;
    uint32_t e = base + (uint32_t)mid * elen;
    int cmp = 0;
    for (int i = 0; i < 4 && !cmp; i++) {
      uint8_t b = rd8(z, e + i);
      if (b != key[i]) cmp = key[i] < b ? -1 : 1;
    }
    if (!cmp) return (uint16_t)e;
    if (linear) lo++;
    else if (cmp < 0) hi = mid - 1;
    else lo = mid + 1;
  }
  return 0;
}

static void tokenise(zm_t *z, uint32_t text, uint32_t parse) {
  uint8_t maxw = rd8(z, parse);
  int count = 0;
  uint32_t i = text + 1, p = parse + 2;
  while (count < maxw) {
    uint8_t c;
    while ((c = rd8(z, i)) == ' ') i++;
    if (c == 0) break;
    uint32_t start = i;
    if (is_separator(z, c)) i++;
    else while ((c = rd8(z, i)) != 0 && c != ' ' && !is_separator(z, c)) i++;
    int len = (int)(i - start);
    char buf[12];
    int m = len < 9 ? len : 9;
    for (int k = 0; k < m; k++) buf[k] = (char)rd8(z, start + (uint32_t)k);
    wr16(z, p, dict_lookup(z, buf, m));
    wr8(z, p + 2, (uint8_t)len);
    wr8(z, p + 3, (uint8_t)(start - text));
    p += 4;
    count++;
  }
  wr8(z, parse + 1, (uint8_t)count);
}

// ---- objects (version 3: 9-byte entries, 31 default properties) -------------------

static uint32_t obj_addr(zm_t *z, uint16_t o) { return z->objtab + 62 + (uint32_t)(o - 1) * 9; }

static uint16_t obj_parent(zm_t *z, uint16_t o)  { return o ? rd8(z, obj_addr(z, o) + 4) : 0; }
static uint16_t obj_sibling(zm_t *z, uint16_t o) { return o ? rd8(z, obj_addr(z, o) + 5) : 0; }
static uint16_t obj_child(zm_t *z, uint16_t o)   { return o ? rd8(z, obj_addr(z, o) + 6) : 0; }
static void set_parent(zm_t *z, uint16_t o, uint16_t v)  { if (o) wr8(z, obj_addr(z, o) + 4, (uint8_t)v); }
static void set_sibling(zm_t *z, uint16_t o, uint16_t v) { if (o) wr8(z, obj_addr(z, o) + 5, (uint8_t)v); }
static void set_child(zm_t *z, uint16_t o, uint16_t v)   { if (o) wr8(z, obj_addr(z, o) + 6, (uint8_t)v); }

static void obj_remove(zm_t *z, uint16_t o) {
  uint16_t p = obj_parent(z, o);
  if (!p) return;
  if (obj_child(z, p) == o) set_child(z, p, obj_sibling(z, o));
  else {
    uint16_t c = obj_child(z, p);
    while (c && obj_sibling(z, c) != o) c = obj_sibling(z, c);
    if (c) set_sibling(z, c, obj_sibling(z, o));
  }
  set_parent(z, o, 0);
  set_sibling(z, o, 0);
}

static void obj_insert(zm_t *z, uint16_t o, uint16_t dest) {
  obj_remove(z, o);
  set_sibling(z, o, obj_child(z, dest));
  set_child(z, dest, o);
  set_parent(z, o, dest);
}

static int attr_test(zm_t *z, uint16_t o, uint16_t a) {
  return (rd8(z, obj_addr(z, o) + (a >> 3)) & (0x80 >> (a & 7))) != 0;
}
static void attr_set(zm_t *z, uint16_t o, uint16_t a, int on) {
  uint32_t addr = obj_addr(z, o) + (a >> 3);
  uint8_t v = rd8(z, addr), m = (uint8_t)(0x80 >> (a & 7));
  wr8(z, addr, on ? (v | m) : (v & ~m));
}

static uint32_t prop_first(zm_t *z, uint16_t o) {
  uint32_t pt = rd16(z, obj_addr(z, o) + 7);
  return pt + 1 + 2u * rd8(z, pt);
}

// Address of the data of property p of object o (0 if absent); *size gets its length.
static uint32_t prop_find(zm_t *z, uint16_t o, uint16_t p, int *size) {
  uint32_t a = prop_first(z, o);
  for (;;) {
    uint8_t s = rd8(z, a);
    if (!s) return 0;
    int num = s & 31, sz = (s >> 5) + 1;
    if (num == p) { if (size) *size = sz; return a + 1; }
    if (num < p) return 0;
    a += 1 + (uint32_t)sz;
  }
}

static void print_obj_name(zm_t *z, uint16_t o) {
  if (!o) return;
  uint32_t pt = rd16(z, obj_addr(z, o) + 7);
  print_zstr(z, pt + 1, 1);
}

// ---- calls, returns, branches ---------------------------------------------------------

static void do_call(zm_t *z, uint16_t packed, const uint16_t *args, int nargs, uint8_t store) {
  if (packed == 0) { setvar(z, store, 0); return; }
  uint32_t addr = (uint32_t)packed * 2;
  uint8_t nloc = rd8(z, addr++);
  if (nloc > 15) { zfail(z, "bad routine header"); return; }
  if (z->nfr >= z->fr_cap || z->sp + nloc + 8 > z->stk_cap) { zfail(z, "stack overflow"); return; }
  zm_frame_t *f = &z->fr[z->nfr++];
  f->ret_pc = z->pc; f->base = z->sp; f->nloc = nloc; f->store = store;
  for (int i = 0; i < nloc; i++) { z->stk[z->sp++] = rd16(z, addr); addr += 2; }
  for (int i = 0; i < nargs && i < nloc; i++) z->stk[f->base + (uint32_t)i] = args[i];
  z->pc = addr;
}

static void do_return(zm_t *z, uint16_t val) {
  if (z->nfr <= 1) { zfail(z, "return from the main routine"); return; }
  zm_frame_t *f = &z->fr[--z->nfr];
  z->sp = f->base;
  z->pc = f->ret_pc;
  setvar(z, f->store, val);
}

static void branch(zm_t *z, int cond) {
  uint8_t b = rd8(z, z->pc++);
  int on_true = (b & 0x80) != 0;
  int32_t off;
  if (b & 0x40) off = b & 0x3F;
  else {
    off = ((b & 0x3F) << 8) | rd8(z, z->pc++);
    if (off & 0x2000) off -= 0x4000;
  }
  if ((cond != 0) == on_true) {
    if (off == 0) do_return(z, 0);
    else if (off == 1) do_return(z, 1);
    else z->pc = (uint32_t)((int32_t)z->pc + off - 2);
  }
}

static void store_result(zm_t *z, uint16_t v) { setvar(z, rd8(z, z->pc++), v); }

// ---- random numbers ----------------------------------------------------------------------

static uint32_t rnd32(zm_t *z) {
  uint32_t x = z->rng;
  x ^= x << 13; x ^= x >> 17; x ^= x << 5;
  return z->rng = x;
}

// ---- status line and input -------------------------------------------------------------------

static void show_status(zm_t *z) {
  if (!z->io.status) return;
  char name[48];
  z->cap = name; z->capn = 0; z->capmax = (int)sizeof name;
  print_obj_name(z, rd16(z, z->globals));            // global 16: the current location
  name[z->capn] = 0;
  z->cap = 0;
  int a = (int16_t)rd16(z, z->globals + 2), b = (int16_t)rd16(z, z->globals + 4);
  z->io.status(z->ctx, name, a, b, (rd8(z, 1) & 2) != 0);
}

static void do_sread(zm_t *z, uint16_t text, uint16_t parse) {
  show_status(z);
  z->cur_text = text; z->cur_parse = parse; z->in_read = 1;
  int maxlen = rd8(z, text);
  char line[256];
  if (maxlen > 250) maxlen = 250;
  int n = z->io.read_line(z->ctx, line, maxlen);
  z->in_read = 0;
  if (n < 0) { z->running = 0; return; }
  if (n > maxlen) n = maxlen;
  for (int i = 0; i < n; i++) {
    char c = line[i];
    if (c >= 'A' && c <= 'Z') c = (char)(c + 32);
    wr8(z, text + 1u + (uint32_t)i, (uint8_t)c);
  }
  wr8(z, text + 1u + (uint32_t)n, 0);
  if (parse) tokenise(z, text, parse);
}

// ---- save, restore, restart, verify ------------------------------------------------------------

static int put_bytes(zm_t *z, const uint8_t *p, uint32_t n) { return z->io.save_write(z->ctx, p, n); }
static int put32(zm_t *z, uint32_t v) {
  uint8_t b[4] = { (uint8_t)(v >> 24), (uint8_t)(v >> 16), (uint8_t)(v >> 8), (uint8_t)v };
  return put_bytes(z, b, 4);
}
static uint32_t get32(const uint8_t *b) { return ((uint32_t)b[0] << 24) | ((uint32_t)b[1] << 16) | ((uint32_t)b[2] << 8) | b[3]; }

// The identity of the story: release, serial number, checksum (header bytes 2-3, 18-23, 28-29).
static void story_id(zm_t *z, uint8_t id[10]) {
  id[0] = z->dyn[2]; id[1] = z->dyn[3];
  for (int i = 0; i < 6; i++) id[2 + i] = z->dyn[0x12 + i];
  id[8] = z->dyn[0x1C]; id[9] = z->dyn[0x1D];
}

// Saved state: "ZMS4", story id (10), pc, dynamic size, sp, frame count, at_input flag,
// text and parse buffer addresses, then dynamic memory, frames and stack words.
int zm_save_state(zm_t *z, int at_input) {
  if (!z->io.save_begin || z->io.save_begin(z->ctx)) return 0;
  uint8_t id[10];
  story_id(z, id);
  int bad = 0;
  bad |= put_bytes(z, (const uint8_t *)"ZMS4", 4);
  bad |= put_bytes(z, id, 10);
  bad |= put32(z, at_input ? z->pc : z->save_pc);
  bad |= put32(z, z->dyn_size);
  bad |= put32(z, z->sp);
  bad |= put32(z, z->nfr);
  uint8_t r[5] = { (uint8_t)(at_input != 0), (uint8_t)(z->cur_text >> 8), (uint8_t)z->cur_text,
                   (uint8_t)(z->cur_parse >> 8), (uint8_t)z->cur_parse };
  bad |= put_bytes(z, r, 5);
  bad |= put_bytes(z, z->dyn, z->dyn_size);
  for (uint32_t i = 0; i < z->nfr && !bad; i++) {
    bad |= put32(z, z->fr[i].ret_pc);
    bad |= put32(z, z->fr[i].base);
    uint8_t t[2] = { z->fr[i].nloc, z->fr[i].store };
    bad |= put_bytes(z, t, 2);
  }
  for (uint32_t i = 0; i < z->sp && !bad; i++) {
    uint8_t t[2] = { (uint8_t)(z->stk[i] >> 8), (uint8_t)z->stk[i] };
    bad |= put_bytes(z, t, 2);
  }
  if (z->io.save_end) z->io.save_end(z->ctx, !bad);
  return !bad;
}

static void fix_header_flags(zm_t *z) {
  z->dyn[1] &= (uint8_t)~(0x10 | 0x20 | 0x40);       // status line available; no screen splitting; fixed pitch
}

static void reset_state(zm_t *z) {
  z->sp = 0; z->nfr = 1;
  z->fr[0].ret_pc = 0; z->fr[0].base = 0; z->fr[0].nloc = 0; z->fr[0].store = 0;
  z->pc = z->initial_pc;
  z->out3_n = 0; z->screen_on = 1; z->cap = 0;
  z->running = 1;
}

// Returns 0 on failure (state untouched unless a read error happens part-way, which
// stops the game), 1 for an ordinary save, 2 for a save made at an input prompt.
static int load_state(zm_t *z) {
  if (!z->io.restore_begin || z->io.restore_begin(z->ctx)) return 0;
  uint8_t h[4 + 10 + 16 + 5], id[10];
  int result = 0;
  int ok = !z->io.restore_read(z->ctx, h, sizeof h);
  story_id(z, id);
  if (ok) {
    ok = h[0] == 'Z' && h[1] == 'M' && h[2] == 'S' && h[3] == '4';
    for (int i = 0; i < 10 && ok; i++) if (h[4 + i] != id[i]) ok = 0;
    uint32_t pc = get32(h + 14), dsz = get32(h + 18), sp = get32(h + 22), nfr = get32(h + 26);
    int at_input = h[30];
    uint16_t text = (uint16_t)((h[31] << 8) | h[32]), parse = (uint16_t)((h[33] << 8) | h[34]);
    if (ok && (dsz != z->dyn_size || sp > z->stk_cap || nfr < 1 || nfr > z->fr_cap)) ok = 0;
    if (ok) {
      // From here the interpreter state is overwritten; a read error is fatal.
      ok = !z->io.restore_read(z->ctx, z->dyn, z->dyn_size);
      for (uint32_t i = 0; i < nfr && ok; i++) {
        uint8_t t[10];
        ok = !z->io.restore_read(z->ctx, t, 10);
        z->fr[i].ret_pc = get32(t); z->fr[i].base = get32(t + 4); z->fr[i].nloc = t[8]; z->fr[i].store = t[9];
      }
      for (uint32_t i = 0; i < sp && ok; i++) {
        uint8_t t[2];
        ok = !z->io.restore_read(z->ctx, t, 2);
        z->stk[i] = (uint16_t)((t[0] << 8) | t[1]);
      }
      if (!ok) { if (z->io.restore_end) z->io.restore_end(z->ctx); zfail(z, "restore failed part-way; game state lost"); return 0; }
      z->sp = sp; z->nfr = nfr; z->pc = pc;
      z->out3_n = 0; z->screen_on = 1;
      if (at_input) { z->resume_read = 1; z->resume_text = text; z->resume_parse = parse; }
      fix_header_flags(z);
      result = at_input ? 2 : 1;
    }
  }
  if (z->io.restore_end) z->io.restore_end(z->ctx);
  return result;
}

int zm_load_state(zm_t *z) { return load_state(z) != 0; }

static int load_dynamic(zm_t *z) {
  if (z->io.read(z->ctx, 0, z->dyn, z->dyn_size)) return ZM_E_READ;
  fix_header_flags(z);
  return ZM_OK;
}

static int verify_checksum(zm_t *z) {
  uint8_t buf[256];
  uint32_t sum = 0, off = 0x40;
  while (off < z->file_len) {
    uint32_t n = z->file_len - off < sizeof buf ? z->file_len - off : (uint32_t)sizeof buf;
    if (z->io.read(z->ctx, off, buf, n)) return 0;
    for (uint32_t i = 0; i < n; i++) sum += buf[i];
    off += n;
  }
  return (uint16_t)sum == rd16(z, 0x1C);
}

// ---- one instruction ----------------------------------------------------------------------------------

enum { T_LARGE = 0, T_SMALL = 1, T_VAR = 2, T_NONE = 3 };
enum { F_2OP, F_1OP, F_0OP, F_VAR };

static uint16_t fetch_operand(zm_t *z, int type) {
  uint16_t v;
  switch (type) {
  case T_LARGE: v = rd16(z, z->pc); z->pc += 2; return v;
  case T_SMALL: return rd8(z, z->pc++);
  default:      return getvar(z, rd8(z, z->pc++));
  }
}

static void bad_opcode(zm_t *z, int form, int op) {
  (void)form; (void)op;
  zfail(z, "illegal or unsupported opcode");
}

static void step(zm_t *z) {
  uint16_t a[4];
  int na = 0, form, op;
  uint32_t op_pc = z->pc;
  uint8_t b = rd8(z, z->pc++);
  z->instructions++;

  if (b < 0x80) {                                    // long form: always two operands
    form = F_2OP; op = b & 0x1F;
    a[0] = fetch_operand(z, (b & 0x40) ? T_VAR : T_SMALL);
    a[1] = fetch_operand(z, (b & 0x20) ? T_VAR : T_SMALL);
    na = 2;
  } else if (b < 0xC0) {                             // short form
    int t = (b >> 4) & 3;
    op = b & 0x0F;
    if (t == T_NONE) form = F_0OP;
    else { form = F_1OP; a[0] = fetch_operand(z, t); na = 1; }
  } else {                                           // variable form
    op = b & 0x1F;
    form = (b & 0x20) ? F_VAR : F_2OP;
    uint8_t types = rd8(z, z->pc++);
    for (int i = 0; i < 4; i++) {
      int t = (types >> (6 - 2 * i)) & 3;
      if (t == T_NONE) break;
      a[na++] = fetch_operand(z, t);
    }
  }
  if (!z->running) return;
  (void)op_pc;

  switch (form) {
  case F_2OP:
    switch (op) {
    case 1:  branch(z, a[0] == a[1] || (na > 2 && a[0] == a[2]) || (na > 3 && a[0] == a[3])); break;  // je
    case 2:  branch(z, (int16_t)a[0] < (int16_t)a[1]); break;                                          // jl
    case 3:  branch(z, (int16_t)a[0] > (int16_t)a[1]); break;                                          // jg
    case 4: { uint16_t v = (uint16_t)(peekvar(z, (uint8_t)a[0]) - 1); pokevar(z, (uint8_t)a[0], v);    // dec_chk
              branch(z, (int16_t)v < (int16_t)a[1]); break; }
    case 5: { uint16_t v = (uint16_t)(peekvar(z, (uint8_t)a[0]) + 1); pokevar(z, (uint8_t)a[0], v);    // inc_chk
              branch(z, (int16_t)v > (int16_t)a[1]); break; }
    case 6:  branch(z, obj_parent(z, a[0]) == a[1]); break;                                            // jin
    case 7:  branch(z, (a[0] & a[1]) == a[1]); break;                                                  // test
    case 8:  store_result(z, a[0] | a[1]); break;                                                      // or
    case 9:  store_result(z, a[0] & a[1]); break;                                                      // and
    case 10: branch(z, attr_test(z, a[0], a[1])); break;                                               // test_attr
    case 11: attr_set(z, a[0], a[1], 1); break;                                                        // set_attr
    case 12: attr_set(z, a[0], a[1], 0); break;                                                        // clear_attr
    case 13: pokevar(z, (uint8_t)a[0], a[1]); break;                                                   // store
    case 14: obj_insert(z, a[0], a[1]); break;                                                         // insert_obj
    case 15: store_result(z, rd16(z, (uint16_t)(a[0] + 2u * a[1]))); break;                            // loadw
    case 16: store_result(z, rd8(z, (uint16_t)(a[0] + a[1]))); break;                                  // loadb
    case 17: {                                                                                         // get_prop
      int sz = 0; uint32_t p = prop_find(z, a[0], a[1], &sz);
      uint16_t v;
      if (p) v = sz == 1 ? rd8(z, p) : rd16(z, p);
      else v = rd16(z, z->objtab + 2u * (a[1] - 1u));
      store_result(z, v); break; }
    case 18: store_result(z, (uint16_t)prop_find(z, a[0], a[1], 0)); break;                            // get_prop_addr
    case 19: {                                                                                         // get_next_prop
      uint16_t r = 0;
      if (a[1] == 0) r = rd8(z, prop_first(z, a[0])) & 31;
      else {
        int sz = 0; uint32_t p = prop_find(z, a[0], a[1], &sz);
        if (!p) { zfail(z, "get_next_prop: no such property"); break; }
        r = rd8(z, p + (uint32_t)sz) & 31;
      }
      store_result(z, r); break; }
    case 20: store_result(z, (uint16_t)(a[0] + a[1])); break;                                          // add
    case 21: store_result(z, (uint16_t)(a[0] - a[1])); break;                                          // sub
    case 22: store_result(z, (uint16_t)((int32_t)(int16_t)a[0] * (int16_t)a[1])); break;               // mul
    case 23: if (!(int16_t)a[1]) { zfail(z, "division by zero"); break; }                              // div
             store_result(z, (uint16_t)((int32_t)(int16_t)a[0] / (int16_t)a[1])); break;
    case 24: if (!(int16_t)a[1]) { zfail(z, "division by zero"); break; }                              // mod
             store_result(z, (uint16_t)((int32_t)(int16_t)a[0] % (int16_t)a[1])); break;
    default: bad_opcode(z, form, op);
    }
    break;

  case F_1OP:
    switch (op) {
    case 0:  branch(z, a[0] == 0); break;                                                              // jz
    case 1: { uint16_t s = obj_sibling(z, a[0]); store_result(z, s); branch(z, s != 0); break; }       // get_sibling
    case 2: { uint16_t c = obj_child(z, a[0]);   store_result(z, c); branch(z, c != 0); break; }       // get_child
    case 3:  store_result(z, obj_parent(z, a[0])); break;                                              // get_parent
    case 4:  store_result(z, a[0] ? (uint16_t)(((rd8(z, a[0] - 1u)) >> 5) + 1) : 0); break;            // get_prop_len
    case 5:  pokevar(z, (uint8_t)a[0], (uint16_t)(peekvar(z, (uint8_t)a[0]) + 1)); break;              // inc
    case 6:  pokevar(z, (uint8_t)a[0], (uint16_t)(peekvar(z, (uint8_t)a[0]) - 1)); break;              // dec
    case 7:  print_zstr(z, a[0], 1); break;                                                            // print_addr
    case 9:  obj_remove(z, a[0]); break;                                                               // remove_obj
    case 10: print_obj_name(z, a[0]); break;                                                           // print_obj
    case 11: do_return(z, a[0]); break;                                                                // ret
    case 12: z->pc = (uint32_t)((int32_t)z->pc + (int16_t)a[0] - 2); break;                            // jump
    case 13: print_zstr(z, (uint32_t)a[0] * 2, 1); break;                                              // print_paddr
    case 14: store_result(z, peekvar(z, (uint8_t)a[0])); break;                                        // load
    case 15: store_result(z, (uint16_t)~a[0]); break;                                                  // not
    default: bad_opcode(z, form, op);
    }
    break;

  case F_0OP:
    switch (op) {
    case 0:  do_return(z, 1); break;                                                                   // rtrue
    case 1:  do_return(z, 0); break;                                                                   // rfalse
    case 2:  z->pc = print_zstr(z, z->pc, 1); break;                                                   // print
    case 3:  z->pc = print_zstr(z, z->pc, 1); zput(z, 13); do_return(z, 1); break;                     // print_ret
    case 4:  break;                                                                                    // nop
    case 5:  z->save_pc = z->pc; branch(z, zm_save_state(z, 0)); break;                                // save
    case 6:  { int r = load_state(z); if (z->running && r != 2) branch(z, r); break; }                 // restore
    case 7:  { int e = load_dynamic(z); if (e) { zfail(z, "restart: story file read error"); break; }  // restart
               reset_state(z); break; }
    case 8:  do_return(z, pop(z)); break;                                                              // ret_popped
    case 9:  pop(z); break;                                                                            // pop
    case 10: z->running = 0; break;                                                                    // quit
    case 11: zput(z, 13); break;                                                                       // new_line
    case 12: show_status(z); break;                                                                    // show_status
    case 13: branch(z, verify_checksum(z)); break;                                                     // verify
    default: bad_opcode(z, form, op);
    }
    break;

  case F_VAR:
    switch (op) {
    case 0: {                                                                                          // call
      uint8_t sv = rd8(z, z->pc++);
      do_call(z, na > 0 ? a[0] : 0, a + 1, na > 0 ? na - 1 : 0, sv);
      break; }
    case 1:  wr16(z, (uint16_t)(a[0] + 2u * a[1]), a[2]); break;                                       // storew
    case 2:  wr8(z, (uint16_t)(a[0] + a[1]), (uint8_t)a[2]); break;                                    // storeb
    case 3: {                                                                                          // put_prop
      int sz = 0; uint32_t p = prop_find(z, a[0], a[1], &sz);
      if (!p) { zfail(z, "put_prop: no such property"); break; }
      if (sz == 1) wr8(z, p, (uint8_t)a[2]); else wr16(z, p, a[2]);
      break; }
    case 4:  do_sread(z, a[0], na > 1 ? a[1] : 0); break;                                              // sread
    case 5:  zput_zscii(z, a[0]); break;                                                               // print_char
    case 6:  zput_num(z, (int16_t)a[0]); break;                                                        // print_num
    case 7: {                                                                                          // random
      int16_t r = (int16_t)a[0];
      uint16_t v = 0;
      if (r > 0) v = (uint16_t)((rnd32(z) >> 8) % (uint32_t)r + 1);
      else if (r < 0) z->rng = ((uint32_t)(-(int32_t)r) * 2654435761u) | 1;
      else z->rng = (z->rng * 1664525u + 1013904223u) ^ z->instructions;
      store_result(z, v); break; }
    case 8:  push(z, a[0]); break;                                                                     // push
    case 9:  pokevar(z, (uint8_t)a[0], pop(z)); break;                                                 // pull
    case 10: case 11: break;                                                                           // split_window, set_window
    case 19: {                                                                                         // output_stream
      int16_t s = (int16_t)a[0];
      if (s == 3) {
        if (z->out3_n >= 16) { zfail(z, "too many nested memory output streams"); break; }
        z->out3[z->out3_n].table = a[1]; z->out3[z->out3_n].count = 0; z->out3_n++;
      } else if (s == -3) {
        if (z->out3_n > 0) { z->out3_n--; wr16(z, z->out3[z->out3_n].table, (uint16_t)z->out3[z->out3_n].count); }
      } else if (s == 1) z->screen_on = 1;
      else if (s == -1) z->screen_on = 0;
      break; }
    case 20: case 21: break;                                                                           // input_stream, sound_effect
    default: bad_opcode(z, form, op);
    }
    break;
  }
}

// ---- setup and run ---------------------------------------------------------------------------------------

int zm_init(zm_t *z, const zm_io_t *io, void *ctx, const zm_mem_t *m, uint32_t seed) {
  zero(z, sizeof *z);
  z->io = *io; z->ctx = ctx;
  z->dyn = m->dyn; z->dyn_cap = m->dyn_cap;
  z->stk = m->stack; z->stk_cap = m->stack_words;
  z->fr = m->frames; z->fr_cap = m->frame_cap;
  z->cache = m->cache; z->npages = m->cache_pages; z->ctag = m->cache_tag; z->cuse = m->cache_use;
  for (uint32_t i = 0; i < z->npages; i++) { z->ctag[i] = 0xFFFFFFFFu; z->cuse[i] = 0; }
  z->last_tag = 0xFFFFFFFFu;
  z->rng = seed | 1;

  uint8_t h[64];
  if (io->read(ctx, 0, h, 64)) return ZM_E_READ;
  z->version = h[0];
  if (z->version != 3) return ZM_E_VERSION;
  z->high_base   = (uint32_t)((h[4] << 8) | h[5]);
  z->initial_pc  = (uint32_t)((h[6] << 8) | h[7]);
  z->dict        = (uint32_t)((h[8] << 8) | h[9]);
  z->objtab      = (uint32_t)((h[10] << 8) | h[11]);
  z->globals     = (uint32_t)((h[12] << 8) | h[13]);
  z->static_base = (uint32_t)((h[14] << 8) | h[15]);
  z->abbrev      = (uint32_t)((h[0x18] << 8) | h[0x19]);
  z->file_len    = (uint32_t)((h[0x1A] << 8) | h[0x1B]) * 2;
  z->dyn_size    = z->static_base;
  if (z->dyn_size < 64 || z->dyn_size > z->dyn_cap) return ZM_E_MEMORY;
  if (z->npages < 2) return ZM_E_MEMORY;
  int e = load_dynamic(z);
  if (e) return e;
  reset_state(z);
  return ZM_OK;
}

int zm_dict_count(zm_t *z) {
  int16_t n = (int16_t)rd16(z, z->dict + 2 + rd8(z, z->dict));
  return n < 0 ? -n : n;
}

// Decode dictionary entry `index` (the first 4 bytes of the entry are 6 Z-characters).
void zm_dict_word(zm_t *z, int index, char out[12]) {
  uint32_t nsep = rd8(z, z->dict);
  uint32_t elen = rd8(z, z->dict + 1 + nsep);
  uint32_t e = z->dict + 4 + nsep + (uint32_t)index * elen;
  int n = 0, shift = 0;
  for (int w = 0; w < 2; w++) {
    uint16_t word = rd16(z, e + 2u * (uint32_t)w);
    for (int i = 0; i < 3; i++) {
      int c = (word >> (10 - 5 * i)) & 31;
      if (c == 5 && shift == 0) { shift = 2; continue; }          // padding or shift to punctuation
      if (c == 4) { shift = 1; continue; }
      if (c < 6) { shift = 0; continue; }
      int alpha = shift; shift = 0;
      char ch = alpha == 0 ? A0[c - 6] : alpha == 1 ? A1[c - 6] : (c >= 8 ? A2[c - 8] : 0);
      if (ch && n < 11) out[n++] = ch;
    }
  }
  out[n] = 0;
}

void zm_run(zm_t *z) {
  while (z->running) {
    if (z->resume_read) { z->resume_read = 0; do_sread(z, z->resume_text, z->resume_parse); }
    else step(z);
  }
}
