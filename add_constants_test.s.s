; Generated by CompilerKit
; Target: 64-bit

.section __TEXT,__text,regular,pure_instructions

.globl _main
.p2align 2
_main:
.cfi_startproc
	str x29, [sp, #-16]!	; save FP and LR with pre-decrement
	str x30, [sp, #8]
	mov x29, sp	; set up frame pointer
	sub sp, sp, #64	; allocate 64 bytes
.Lmain_entry:
	mov w0, #2	; load constant 2
	mov w1, #3	; load constant 3
	add w2, w0, w1	; add
	mov w0, w2	; return value
	mov sp, x29	; restore stack pointer
	ldr x29, [sp], #16	; restore FP and LR with post-increment
	ldr x30, [sp, #-8]
	ret	; return
.cfi_endproc
