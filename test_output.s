; Generated by CompilerKit
; Target: 64-bit

.section __TEXT,__text,regular,pure_instructions

.globl _main
.p2align 2
_main:
.cfi_startproc
	sub sp, sp, #16	; allocate 16 bytes
entry:
	mov w0, #42	; load constant 42
	add sp, sp, #16	; deallocate stack frame
	ret	; return
.cfi_endproc
