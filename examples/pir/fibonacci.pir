; Fibonacci function in PIR
; Computes the nth Fibonacci number iteratively

func fibonacci(n: i32) -> i32 {
entry:
  ; Check if n <= 1
  %cmp = icmp sle n, 1
  br %cmp, base_case, compute

base_case:
  ret n

compute:
  ; Initialize a = 0, b = 1
  %a = 0
  %b = 1
  %i = 1
  jmp loop_header

loop_header:
  ; Check if i < n
  %loop_cond = icmp slt %i, n
  br %loop_cond, loop_body, loop_exit

loop_body:
  ; Compute next Fibonacci number
  %next = add %a, %b
  
  ; Update values: a = b, b = next
  %a_new = %b
  %b_new = %next
  
  ; Increment counter
  %i_new = add %i, 1
  
  ; Continue loop with updated values
  jmp loop_header

loop_exit:
  ret %b
}