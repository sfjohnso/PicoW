/*
 * life.s - one generation of Conway's Game of Life, Thumb-2 (Cortex-M4)
 *
 * void life_step(const uint8_t *src, uint8_t *dst);
 *
 * Grid is W x H cells, 1 bit per cell, STRIDE bytes per row. Cell (x,y) is
 * bit (x & 7) of byte [y*STRIDE + (x >> 3)]. The edges wrap (torus).
 * Every cell of dst is written, so dst needs no clearing.
 *
 * Register use:
 *   r4  y            r5  x            r6/r7/r8  rows y-1 / y / y+1 of src
 *   r9  neighbours   r10 row y of dst r11 x-1   r12 centre cell
 *   r0  x+1 (then scratch)            r1-r3 scratch
 */
        .syntax unified
        .cpu    cortex-m4
        .thumb

        .equ    W, 200
        .equ    H, 120
        .equ    STRIDE, 25

        .macro  GETBIT rp, xc, acc
        lsr     r2, \xc, #3
        ldrb    r2, [\rp, r2]
        and     r3, \xc, #7
        lsr     r2, r2, r3
        and     r2, r2, #1
        add     \acc, \acc, r2
        .endm

        .text
        .global life_step
        .thumb_func
        .type   life_step, %function
life_step:
        push    {r4-r11, lr}
        sub     sp, sp, #8
        str     r0, [sp]
        str     r1, [sp, #4]
        movs    r4, #0

row_loop:
        ldr     r1, [sp]
        movs    r2, #STRIDE
        cmp     r4, #0
        ite     eq
        moveq   r0, #(H-1)
        subne   r0, r4, #1
        mul     r0, r0, r2
        add     r6, r1, r0
        mul     r0, r4, r2
        add     r7, r1, r0
        cmp     r4, #(H-1)
        ite     eq
        moveq   r0, #0
        addne   r0, r4, #1
        mul     r0, r0, r2
        add     r8, r1, r0
        ldr     r1, [sp, #4]
        mul     r0, r4, r2
        add     r10, r1, r0
        movs    r5, #0

col_loop:
        cmp     r5, #0
        ite     eq
        moveq   r11, #(W-1)
        subne   r11, r5, #1
        cmp     r5, #(W-1)
        ite     eq
        moveq   r0, #0
        addne   r0, r5, #1

        movs    r9, #0
        GETBIT  r6, r11, r9
        GETBIT  r6, r5, r9
        GETBIT  r6, r0, r9
        GETBIT  r8, r11, r9
        GETBIT  r8, r5, r9
        GETBIT  r8, r0, r9
        GETBIT  r7, r11, r9
        GETBIT  r7, r0, r9
        mov     r12, #0
        GETBIT  r7, r5, r12

        /* birth on 3; survival on 2 or 3 */
        movs    r2, #0
        cmp     r9, #3
        it      eq
        moveq   r2, #1
        cmp     r9, #2
        it      eq
        moveq   r2, r12

        /* store the new cell into dst */
        lsr     r1, r5, #3
        and     r3, r5, #7
        ldrb    r0, [r10, r1]
        mov     r12, #1
        lsl     r12, r12, r3
        bic     r0, r0, r12
        lsl     r2, r2, r3
        orr     r0, r0, r2
        strb    r0, [r10, r1]

        adds    r5, r5, #1
        cmp     r5, #W
        blt     col_loop

        adds    r4, r4, #1
        cmp     r4, #H
        blt     row_loop

        add     sp, sp, #8
        pop     {r4-r11, pc}
        .size   life_step, .-life_step
