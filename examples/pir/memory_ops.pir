; Memory operations example in PIR
; Demonstrates various memory instructions

func memcpy_example(dst: ptr, src: ptr, n: i32) {
entry:
  ; Simple byte-by-byte copy
  %i = 0
  jmp loop_cond

loop_cond:
  %done = icmp sge %i, n
  br %done, exit, loop_body

loop_body:
  ; Calculate addresses
  %src_addr = ptradd src, %i
  %dst_addr = ptradd dst, %i
  
  ; Load byte from source
  %byte = load i8 %src_addr
  
  ; Store byte to destination
  store %byte, %dst_addr
  
  ; Increment counter
  %i_next = add %i, 1
  jmp loop_cond

exit:
  ret
}

func array_sum(arr: ptr, len: i32) -> i32 {
entry:
  %sum = 0
  %i = 0
  jmp loop_header

loop_header:
  %continue = icmp slt %i, len
  br %continue, loop_body, done

loop_body:
  ; Get pointer to array[i]
  %elem_ptr = gep arr, %i
  
  ; Load array[i]
  %elem = load i32 %elem_ptr
  
  ; Add to sum
  %new_sum = add %sum, %elem
  
  ; Increment index
  %new_i = add %i, 1
  
  jmp loop_header

done:
  ret %sum
}

func swap(a: ptr, b: ptr) {
entry:
  ; Load values
  %val_a = load i32 a
  %val_b = load i32 b
  
  ; Store swapped values
  store %val_b, a
  store %val_a, b
  
  ret
}

; Example with stack allocation
func stack_array_example() -> i32 {
entry:
  ; Allocate array of 10 i32s on stack
  %array = alloca 40, 4  ; 40 bytes, 4-byte aligned
  
  ; Initialize array[0] = 42
  %ptr0 = gep %array, 0
  store 42, %ptr0
  
  ; Initialize array[1] = 23
  %ptr1 = gep %array, 1
  store 23, %ptr1
  
  ; Load and return sum
  %val0 = load i32 %ptr0
  %val1 = load i32 %ptr1
  %sum = add %val0, %val1
  
  ret %sum
}