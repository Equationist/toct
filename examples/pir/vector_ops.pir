; PIR v0.9
; Vector operations example in PIR
; Demonstrates SIMD vector instructions

func dot_product(a:ptr, b:ptr, len:i32) -> f32
entry:
  ; Initialize sum to zero vector
  zero_f = 0.0
  sum = splat zero_f, 4
  zero_i = 0
  i = zero_i
  jmp loop_header

loop_header:
  ; Check if we have at least 4 elements remaining
  remaining = sub.i32 len, i
  four = 4
  can_vectorize = icmp.sge.i32 remaining, four
  br can_vectorize, vector_loop, scalar_loop

vector_loop:
  ; Calculate byte offset (i * 4 for f32)
  four_i = 4
  offset = mul.i32 i, four_i
  
  ; Load 4 floats from each array
  ptr_a = ptradd a, offset
  ptr_b = ptradd b, offset
  vec_a = load.v4xf32 [ptr_a]
  vec_b = load.v4xf32 [ptr_b]
  
  ; Multiply vectors element-wise
  prod = fmul vec_a, vec_b
  
  ; Add to accumulator
  sum = fadd sum, prod
  
  ; Increment by 4
  four_inc = 4
  i = add.i32 i, four_inc
  jmp loop_header

scalar_loop:
  ; Process remaining elements one by one
  done = icmp.sge.i32 i, len
  br done, reduce, scalar_body

scalar_body:
  ; Calculate byte offset
  four_s = 4
  offset_s = mul.i32 i, four_s
  
  ; Load single elements
  ptr_a_s = ptradd a, offset_s
  ptr_b_s = ptradd b, offset_s
  val_a = load.f32 [ptr_a_s]
  val_b = load.f32 [ptr_b_s]
  
  ; Multiply and add to first lane of sum
  prod_s = fmul val_a, val_b
  lane0 = extractlane sum, 0
  new_lane0 = fadd lane0, prod_s
  sum = insertlane sum, 0, new_lane0
  
  ; Increment
  one = 1
  i = add.i32 i, one
  jmp scalar_loop

reduce:
  ; Horizontal sum of vector elements
  s0 = extractlane sum, 0
  s1 = extractlane sum, 1
  s2 = extractlane sum, 2
  s3 = extractlane sum, 3
  
  partial1 = fadd s0, s1
  partial2 = fadd s2, s3
  result = fadd partial1, partial2
  
  ret result
endfunc