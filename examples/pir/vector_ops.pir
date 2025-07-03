; Vector operations example in PIR
; Demonstrates SIMD vector instructions

func dot_product(a: ptr, b: ptr, len: i32) -> f32 {
entry:
  ; Initialize sum to zero vector
  %sum = splat 0.0, vec[4 x f32]
  %i = 0
  jmp loop_header

loop_header:
  ; Check if we have at least 4 elements remaining
  %remaining = sub len, %i
  %can_vectorize = icmp sge %remaining, 4
  br %can_vectorize, vector_loop, scalar_loop

vector_loop:
  ; Load 4 floats from each array
  %ptr_a = ptradd a, %i
  %ptr_b = ptradd b, %i
  %vec_a = load vec[4 x f32] %ptr_a
  %vec_b = load vec[4 x f32] %ptr_b
  
  ; Multiply vectors element-wise
  %prod = fmul %vec_a, %vec_b
  
  ; Add to accumulator
  %sum_new = fadd %sum, %prod
  
  ; Increment by 4
  %i_new = add %i, 4
  jmp loop_header

scalar_loop:
  ; Process remaining elements one by one
  %done = icmp sge %i, len
  br %done, reduce, scalar_body

scalar_body:
  ; Load single elements
  %ptr_a_s = ptradd a, %i
  %ptr_b_s = ptradd b, %i
  %val_a = load f32 %ptr_a_s
  %val_b = load f32 %ptr_b_s
  
  ; Multiply and add to first lane of sum
  %prod_s = fmul %val_a, %val_b
  %lane0 = extractlane %sum, 0
  %new_lane0 = fadd %lane0, %prod_s
  %sum_updated = insertlane %sum, %new_lane0, 0
  
  ; Increment
  %i_next = add %i, 1
  jmp scalar_loop

reduce:
  ; Horizontal sum of vector elements
  %s0 = extractlane %sum, 0
  %s1 = extractlane %sum, 1
  %s2 = extractlane %sum, 2
  %s3 = extractlane %sum, 3
  
  %partial1 = fadd %s0, %s1
  %partial2 = fadd %s2, %s3
  %result = fadd %partial1, %partial2
  
  ret %result
}