; PIR v0.9
; Factorial function in PIR
; Computes n! recursively

func factorial(n:i32) -> i32
entry:
  ; Check if n <= 1
  cmp = icmp.sle.i32 n, 1
  br cmp, base_case, recursive_case

base_case:
  one = 1
  ret one

recursive_case:
  ; Compute n - 1
  one = 1
  n_minus_1 = sub.i32 n, one
  
  ; Recursive call: factorial(n - 1)
  rec_result = call.i32 factorial, n_minus_1
  
  ; Return n * factorial(n - 1)
  result = mul.i32 n, rec_result
  ret result
endfunc

; Example usage
func main() -> i32
entry:
  five = 5
  result = call.i32 factorial, five
  ret result
endfunc