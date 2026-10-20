/*
 * anim.s - apply one animation frame to the LCD line buffers (Thumb-2, Cortex-M4)
 *
 * void apply_frame(const uint8_t *rec, int nlines, uint8_t *(*line_addr)(int));
 *
 * rec points at nlines records of 51 bytes: a line number (0..239) and that line's
 * 50 bytes, already in the LCD's own layout (pixel x is bit x&7 of byte x>>3, 0 = black).
 * line_addr is the SDK's lcd_line_addr(), which returns the line's buffer and marks the
 * line dirty, so only lines that changed are sent to the display.
 *
 * Each line is copied as twelve unaligned words plus one halfword (the Cortex-M4 allows
 * unaligned LDR/STR; LDM/STM would not be safe here).
 *
 *   r4 record pointer   r5 lines left   r6 line_addr   r3 destination
 */
        .syntax unified
        .cpu    cortex-m4
        .thumb

        .text
        .global apply_frame
        .thumb_func
        .type   apply_frame, %function
apply_frame:
        push    {r4-r8, lr}                 /* six registers: keeps the stack 8-byte aligned */
        mov     r4, r0
        mov     r5, r1
        mov     r6, r2
        cmp     r5, #0
        beq     done
next_line:
        ldrb    r0, [r4], #1                /* line number */
        blx     r6                          /* r0 = line_addr(line); marks the line dirty */
        mov     r3, r0
        mov     r2, #12
copy_words:
        ldr     r1, [r4], #4
        str     r1, [r3], #4
        subs    r2, r2, #1
        bne     copy_words
        ldrh    r1, [r4], #2                /* the last two bytes */
        strh    r1, [r3], #2
        subs    r5, r5, #1
        bne     next_line
done:
        pop     {r4-r8, pc}
        .size   apply_frame, .-apply_frame
