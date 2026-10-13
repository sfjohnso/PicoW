/*
 * ack.s - brute-force Ackermann function, Thumb-2 (Cortex-M4), resumable
 *
 * int ack_run(ack_state_t *s, uint32_t budget);      (see ack_engine.h)
 *
 * No recursion: the pending outer calls A(m-1, .) are kept as a stack of m
 * values, stored run-length encoded. One step is one of
 *
 *     m == 0   n += 1, then m = pop()           (done if the stack is empty)
 *     n == 0   m -= 1, n = 1
 *     else     push(m - 1), n -= 1
 *
 * which is exactly one call of the textbook recursion, so the step count
 * equals the number of calls A(m,n) makes.
 *
 * The topmost run is cached in registers; runs below it are in vals[]/cnts[].
 *
 * Registers while running:
 *   r0 m        r1 n         r2 steps left in this slice
 *   r3 vals     r4 cnts      r5 nruns (including the cached top)
 *   r6 topval   r7 topcnt    r8 depth      r9 peak     r10 maxruns
 *   r11 state   r12, lr scratch
 */
        .syntax unified
        .cpu    cortex-m4
        .thumb

        .equ    S_M,        0
        .equ    S_N,        4
        .equ    S_VALS,     8
        .equ    S_CNTS,     12
        .equ    S_NRUNS,    16
        .equ    S_TOPVAL,   20
        .equ    S_TOPCNT,   24
        .equ    S_DEPTH,    28
        .equ    S_PEAK,     32
        .equ    S_MAXRUNS,  36
        .equ    S_STEPS_LO, 40
        .equ    S_STEPS_HI, 44

        .text
        .global ack_run
        .thumb_func
        .type   ack_run, %function
ack_run:
        push    {r4-r11, lr}
        sub     sp, sp, #8
        str     r1, [sp]                    /* budget at entry */
        mov     r11, r0
        mov     r2, r1
        ldr     r0, [r11, #S_M]
        ldr     r1, [r11, #S_N]
        ldr     r3, [r11, #S_VALS]
        ldr     r4, [r11, #S_CNTS]
        ldr     r5, [r11, #S_NRUNS]
        ldr     r6, [r11, #S_TOPVAL]
        ldr     r7, [r11, #S_TOPCNT]
        ldr     r8, [r11, #S_DEPTH]
        ldr     r9, [r11, #S_PEAK]
        ldr     r10, [r11, #S_MAXRUNS]

loop:
        cmp     r2, #0
        beq     out_budget
        sub     r2, r2, #1
        cmp     r0, #0
        beq     m_zero
        cmp     r1, #0
        beq     n_zero

        /* push(m - 1); n -= 1 */
        sub     r12, r0, #1
        cmp     r5, #0
        beq     push_first
        cmp     r6, r12
        beq     push_same
        sub     lr, r5, #1                  /* spill the cached top run */
        strb    r6, [r3, lr]
        str     r7, [r4, lr, lsl #2]
        add     r5, r5, #1
        cmp     r5, r10
        bhi     err_runs
        mov     r6, r12
        mov     r7, #1
        b       pushed
push_same:
        add     r7, r7, #1
        b       pushed
push_first:
        mov     r6, r12
        mov     r7, #1
        mov     r5, #1
pushed:
        add     r8, r8, #1
        cmp     r8, r9
        it      hi
        movhi   r9, r8
        sub     r1, r1, #1
        b       loop

n_zero:
        sub     r0, r0, #1
        mov     r1, #1
        b       loop

m_zero:
        adds    r1, r1, #1
        bcs     err_ovf
        cmp     r8, #0
        beq     done
        mov     r0, r6                      /* m = pop() */
        sub     r8, r8, #1
        subs    r7, r7, #1
        bne     loop
        subs    r5, r5, #1                  /* that run is used up */
        beq     loop
        sub     lr, r5, #1                  /* uncache the run below it */
        ldrb    r6, [r3, lr]
        ldr     r7, [r4, lr, lsl #2]
        b       loop

out_budget:
        mov     r12, #0                     /* ACK_MORE */
        b       finish
done:
        mov     r12, #1                     /* ACK_DONE */
        b       finish
err_ovf:
        mov     r12, #2                     /* ACK_OVERFLOW */
        b       finish
err_runs:
        mov     r12, #3                     /* ACK_RUNS */

finish:
        str     r0, [r11, #S_M]
        str     r1, [r11, #S_N]
        str     r5, [r11, #S_NRUNS]
        str     r6, [r11, #S_TOPVAL]
        str     r7, [r11, #S_TOPCNT]
        str     r8, [r11, #S_DEPTH]
        str     r9, [r11, #S_PEAK]
        ldr     r0, [sp]
        subs    r0, r0, r2                  /* steps executed in this call */
        ldr     r1, [r11, #S_STEPS_LO]
        adds    r1, r1, r0
        str     r1, [r11, #S_STEPS_LO]
        ldr     r1, [r11, #S_STEPS_HI]
        adc     r1, r1, #0
        str     r1, [r11, #S_STEPS_HI]
        mov     r0, r12
        add     sp, sp, #8
        pop     {r4-r11, pc}
        .size   ack_run, .-ack_run
