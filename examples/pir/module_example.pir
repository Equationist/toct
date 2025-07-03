; PIR v0.9
; Complete module example showing all top-level declarations

; Type declarations
type Vec3 = struct<<f32, f32, f32>>
type Color = packed_struct<<i8, i8, i8, i8>>

; Global variables
global counter:i32 init 0
global pi:f32 align 4 init 3.14159

; Constants
const max_size:i32 init 1024
const origin:Vec3 init <<0.0, 0.0, 0.0>>

; Simple increment function
func inc() -> void
entry:
  val = load.i32 [counter]
  one = 1
  val = add.nsw.i32 val, one
  store.i32 val, [counter]
  ret
endfunc

; Add two vectors
func vec3_add(a:Vec3, b:Vec3) -> Vec3
entry:
  ; Extract components
  a_x = extractvalue a, 0
  a_y = extractvalue a, 1
  a_z = extractvalue a, 2
  
  b_x = extractvalue b, 0
  b_y = extractvalue b, 1
  b_z = extractvalue b, 2
  
  ; Add components
  x = fadd a_x, b_x
  y = fadd a_y, b_y
  z = fadd a_z, b_z
  
  ; Build result
  result = insertvalue undef, x, 0
  result = insertvalue result, y, 1
  result = insertvalue result, z, 2
  
  ret result
endfunc

; Main function with annotations
func main() -> i32    @{"pure":true, "pre":"true", "post":"ret >= 0"}
entry:
  ; Call increment
  call.void inc
  
  ; Load and return counter
  val = load.i32 [counter]
  ret val
endfunc