/*
 * life.s - Conway's Game of Life, Thumb-2 (Cortex-M4), 8 cells per operation
 *
 * void life_step(const uint8_t *src, uint8_t *dst);
 * void life_fix_edges(uint8_t *grid);
 *
 * Grid layout: 122 stored rows of GS = 27 bytes. Stored row r (1..120) is
 * Life row r-1; stored row 0 is a copy of stored row 120 and stored row 121 a
 * copy of stored row 1. Within a stored row, byte 0 is a copy of byte 25 and
 * byte 26 a copy of byte 1; bytes 1..25 hold the 200 cells, cell x being bit
 * (x & 7) of byte 1 + (x >> 3). The copies make the torus wrap free, so the
 * inner loop has no edge tests.
 *
 * life_step writes every data byte of dst, then calls life_fix_edges(dst) to
 * refresh the copies. Callers that fill a grid by hand must call
 * life_fix_edges themselves.
 *
 * For one byte of 8 cells, the eight neighbours are 8 bit-planes (the three
 * above, the three below, and west and east). They are added into a small
 * saturating counter n0 (ones), n1 (twos), n2 (anything >= 4), one plane at a
 * time:
 *     carry0 = n0 & x;  n0 ^= x;  carry1 = n1 & carry0;  n1 ^= carry0;  n2 |= carry1
 * A cell is alive next generation iff it has 2 or 3 neighbours and is either
 * already alive or has an odd count:   n1 & ~n2 & (n0 | self)
 *
 * Registers in life_step:
 *   r0,r1,r2  n0,n1,n2      r3-r5,r10  plane scratch
 *   r6  cursor in src       r7  end of this row's data
 *   r8  end of grid         r9  dst - src       r11  dst base
 */
        .syntax unified
        .cpu    cortex-m4
        .thumb

        .equ    GS, 27
        .equ    ROWS, 120
        .equ    DBYTES, 25

        /* x += plane (see above); destroys x, uses t as scratch */
        .macro  ADDP x, t
        and     \t, r0, \x
        eor     r0, r0, \x
        and     \x, r1, \t
        eor     r1, r1, \t
        orr     r2, r2, \x
        .endm

        /* d = plane of west neighbours = ((cur << 1) | (left >> 7)) & 255; d may equal left */
        .macro  WEST d, cur, left
        lsr     \d, \left, #7
        orr     \d, \d, \cur, lsl #1
        and     \d, \d, #255
        .endm

        /* d = plane of east neighbours = ((cur >> 1) | (right << 7)) & 255; d may equal right */
        .macro  EAST d, cur, right
        lsl     \d, \right, #7
        orr     \d, \d, \cur, lsr #1
        and     \d, \d, #255
        .endm

        .text
        .global life_step
        .thumb_func
        .type   life_step, %function
life_step:
        push    {r4-r11, lr}
        mov     r11, r1                     /* dst base, for the edge fix */
        sub     r9, r1, r0                  /* dst - src */
        add     r6, r0, #(GS + 1)           /* stored row 1, first data byte */
        add     r8, r0, #((ROWS + 1) * GS + 1)

row_loop:
        add     r7, r6, #DBYTES
byte_loop:
        /* row above: its own plane starts the counter */
        ldrb    r3, [r6, #-GS]
        ldrb    r4, [r6, #-(GS + 1)]
        ldrb    r5, [r6, #-(GS - 1)]
        WEST    r4, r3, r4
        EAST    r5, r3, r5
        mov     r0, r3
        movs    r1, #0
        movs    r2, #0
        ADDP    r4, r10
        ADDP    r5, r10

        /* row below */
        ldrb    r3, [r6, #GS]
        ldrb    r4, [r6, #(GS - 1)]
        ldrb    r5, [r6, #(GS + 1)]
        WEST    r4, r3, r4
        EAST    r5, r3, r5
        ADDP    r3, r10
        ADDP    r4, r10
        ADDP    r5, r10

        /* this row: west and east only; r3 keeps the cell itself */
        ldrb    r3, [r6]
        ldrb    r4, [r6, #-1]
        ldrb    r5, [r6, #1]
        WEST    r4, r3, r4
        EAST    r5, r3, r5
        ADDP    r4, r10
        ADDP    r5, r10

        /* next = n1 & ~n2 & (n0 | self) */
        orr     r10, r0, r3
        and     r1, r1, r10
        bic     r1, r1, r2
        strb    r1, [r6, r9]

        adds    r6, r6, #1
        cmp     r6, r7
        blo     byte_loop

        adds    r6, r6, #2                  /* skip right copy and next left copy */
        cmp     r6, r8
        blo     row_loop

        mov     r0, r11
        pop     {r4-r11, lr}
        b       life_fix_edges
        .size   life_step, .-life_step


        .global life_fix_edges
        .thumb_func
        .type   life_fix_edges, %function
life_fix_edges:
        /* left/right copies in stored rows 1..120 */
        movs    r1, #ROWS
        add     r2, r0, #GS
1:      ldrb    r3, [r2, #25]
        strb    r3, [r2, #0]
        ldrb    r3, [r2, #1]
        strb    r3, [r2, #26]
        add     r2, r2, #GS
        subs    r1, r1, #1
        bne     1b

        /* stored row 0 <- stored row 120 (r0 advances to stored row 1) */
        add     r2, r0, #(ROWS * GS)
        movs    r1, #GS
2:      ldrb    r3, [r2], #1
        strb    r3, [r0], #1
        subs    r1, r1, #1
        bne     2b

        /* stored row 121 <- stored row 1 */
        add     r2, r0, #(ROWS * GS)
        movs    r1, #GS
3:      ldrb    r3, [r0], #1
        strb    r3, [r2], #1
        subs    r1, r1, #1
        bne     3b
        bx      lr
        .size   life_fix_edges, .-life_fix_edges
