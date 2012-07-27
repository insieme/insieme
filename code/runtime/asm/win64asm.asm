
; NASM has a simple rule for dealing with memory addressing in instructions:
; Everything in square brackets is a reference to a memory address,
; while everything outside of square brackets is a constant.

; functions defined somewhere else
extern _irt_wi_trampoline
global lwt_continue_impl
global irt_time_ticks
global irt_time_ticks_available
global irt_time_ticks_constant


; C function signature:
; void lwt_continue_impl(irt_work_item *wi /*rcx*/, wi_implementation_func* func /*rdx*/,
;	intptr_t *newstack /*r8*/, intptr_t *basestack /*r9*/)
PROC_FRAME      lwt_continue_impl
	; in the prologue one must (from yasm manual): save register parameters in their shadow locations (if needed),
	; save any non-volatile registers that are used,
	; allocate stack space for local variables,
	; and establish a register as a stack frame pointer

	push        rbp     ; save prospective frame pointer
	[pushreg    rbp]	; create unwind data (in case of errors; for debugging purposes)
	push		rbx 
	[pushreg    rbx]
	push		rdi
	[pushreg    rdi]	
	push		rsi
	[pushreg    rsi]
	push		r12 
	[pushreg    r12]
	push		r13 
	[pushreg    r13]
	push		r14
	[pushreg    r14]
	push		r15
	[pushreg    r15]
	;lea         rbp,[rsp+0x20]  ; assign the frame pointer with a bias of 32, lea: load effective address

	[endprolog]
	
	; swap stacks
	mov [r9], rsp  
	mov rsp, [r8]
	; call function if func != NULL 
	mov rax, rcx
	mov rcx, rdx 
	jrcxz NOCALL 
	; rdx still has func, rcx needs to be restored, then call 
	mov rcx, rax
	call _irt_wi_trampoline
	; restore registers for other coroutine
	NOCALL:	
	
	; epilog: free any allocated stack space and restore non-volatile registers before returning to the calling function. 
	;lea         rsp,[rbp-0x20]  ; This is the official epilog
	pop r15 
	pop r14 
	pop r13 
	pop r12 
	pop rsi 
	pop rdi 
	pop rbx
	pop rbp
	
	ret
ENDPROC_FRAME

; get the rdtsc value
PROC_FRAME irt_time_ticks
	; no need to save any registers because we only operate on volatile registers
	[endprolog]
	rdtsc				; edx now holds upper 32 bits, eax lower 32
	shl		rdx, 32		; shift rdx left by 32 bits
	or		rax, rdx	; add bits of rdx to rax
	ret
ENDPROC_FRAME

; use cpuid to check if tsc is available
PROC_FRAME irt_time_ticks_available
	; nothing to do
	[endprolog]
	mov		eax, 0x00000001
	cpuid						; result is in edx
	and		edx, 0x00000010		; if this yields zero, the zero flag zf is set
	jz		.ISZERO				; jump if zf is not zero, labels with a dot are local labels
	mov rax, 0x00000001			; we return 1
	ret
	.ISZERO:
	mov rax, 0x00000000			; we return 0
	ret
ENDPROC_FRAME

; check if tick frequency is constant
PROC_FRAME irt_time_ticks_constant
	; nothing to do
	[endprolog]
	mov eax, 0x80000007
	cpuid
	and edx, 0x00000100			; the 8th bit represents the TscInvariant bit
	jz		.ISZERO				; jump if zf is not zero
	mov rax, 0x00000001			; we return 1
	ret
	.ISZERO:
	mov rax, 0x00000000			; we return 0
	ret
ENDPROC_FRAME
