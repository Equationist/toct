; Factorial function in PIR
; Computes n! recursively

func factorial(n: i32) -> i32 {
entry:
  ; Check if n <= 1
  %cmp = icmp sle n, 1
  br %cmp, base_case, recursive_case

base_case:
  ret 1

recursive_case:
  ; Compute n - 1
  %n_minus_1 = sub n, 1
  
  ; Recursive call: factorial(n - 1)
  %rec_result = call factorial(%n_minus_1)
  
  ; Return n * factorial(n - 1)
  %result = mul n, %rec_result
  ret %result
}

; Example usage
func main() -> i32 {
entry:
  %input = 5
  %result = call factorial(%input)
  ret %result
}