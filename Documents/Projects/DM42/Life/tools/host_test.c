#include <stdio.h>
#include <string.h>
#include "patterns.h"
void life_fix_edges(uint8_t *g) { (void)g; }
static const char *names[] = {"glider","lwss","rpentomino","acorn","diehard","pentadecathlon","pulsar","gosper"};
static const char *pats[]  = {PAT_GLIDER,PAT_LWSS,PAT_RPENTOMINO,PAT_ACORN,PAT_DIEHARD,PAT_PENTADECATHLON,PAT_PULSAR,PAT_GOSPER_GUN};
int main(void) {
  static uint8_t g[GBYTES];
  for (int p = 0; p < 8; p++) {
    int w, h; rle_size(pats[p], &w, &h);
    printf("%s size %d %d\n", names[p], w, h);
    for (int o = 0; o < 8; o++) {
      memset(g, 0, sizeof g);
      rle_stamp(g, pats[p], 197, 118, o);     // near the corner: exercises wrapping
      printf("%s o%d:", names[p], o);
      for (int y = 0; y < H; y++) for (int x = 0; x < W; x++)
        if ((ROW(g, y)[x >> 3] >> (x & 7)) & 1) printf(" %d,%d", x, y);
      printf("\n");
    }
  }
  memset(g, 0xFF, sizeof g); clear_rect(g, 198, 118, 4, 4);
  int cleared = 0; for (int y = 0; y < H; y++) for (int x = 0; x < W; x++) if (!((ROW(g,y)[x>>3]>>(x&7))&1)) cleared++;
  printf("clear_rect cleared %d (expect 16)\n", cleared);
  return 0;
}
