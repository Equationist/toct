; Struct manipulation example in PIR
; Demonstrates struct types and field access

; Define a Point struct type implicitly through usage
func create_point(x: f32, y: f32) -> struct{f32, f32} {
entry:
  ; Allocate space for struct
  %ptr = alloca 8, 4
  
  ; Store x at offset 0
  %x_ptr = fieldaddr %ptr, 0
  store x, %x_ptr
  
  ; Store y at offset 4
  %y_ptr = fieldaddr %ptr, 1
  store y, %y_ptr
  
  ; Load and return the struct
  %point = load struct{f32, f32} %ptr
  ret %point
}

func distance_squared(p1: struct{f32, f32}, p2: struct{f32, f32}) -> f32 {
entry:
  ; Extract coordinates from first point
  %x1 = extractvalue p1, 0
  %y1 = extractvalue p1, 1
  
  ; Extract coordinates from second point
  %x2 = extractvalue p2, 0
  %y2 = extractvalue p2, 1
  
  ; Compute differences
  %dx = fsub %x2, %x1
  %dy = fsub %y2, %y1
  
  ; Square the differences
  %dx2 = fmul %dx, %dx
  %dy2 = fmul %dy, %dy
  
  ; Sum of squares
  %dist2 = fadd %dx2, %dy2
  ret %dist2
}

; Example with packed struct for RGB color
func blend_colors(c1: packed_struct{i8, i8, i8}, c2: packed_struct{i8, i8, i8}, factor: f32) -> packed_struct{i8, i8, i8} {
entry:
  ; Extract RGB components
  %r1 = extractvalue c1, 0
  %g1 = extractvalue c1, 1
  %b1 = extractvalue c1, 2
  
  %r2 = extractvalue c2, 0
  %g2 = extractvalue c2, 1
  %b2 = extractvalue c2, 2
  
  ; Convert to float for blending
  %r1_f = uitofp %r1 to f32
  %g1_f = uitofp %g1 to f32
  %b1_f = uitofp %b1 to f32
  
  %r2_f = uitofp %r2 to f32
  %g2_f = uitofp %g2 to f32
  %b2_f = uitofp %b2 to f32
  
  ; Compute 1 - factor
  %one = 1.0
  %inv_factor = fsub %one, factor
  
  ; Blend each component
  %r1_weighted = fmul %r1_f, %inv_factor
  %r2_weighted = fmul %r2_f, factor
  %r_blended = fadd %r1_weighted, %r2_weighted
  
  %g1_weighted = fmul %g1_f, %inv_factor
  %g2_weighted = fmul %g2_f, factor
  %g_blended = fadd %g1_weighted, %g2_weighted
  
  %b1_weighted = fmul %b1_f, %inv_factor
  %b2_weighted = fmul %b2_f, factor
  %b_blended = fadd %b1_weighted, %b2_weighted
  
  ; Convert back to i8
  %r_final = fptoui %r_blended to i8
  %g_final = fptoui %g_blended to i8
  %b_final = fptoui %b_blended to i8
  
  ; Build result struct
  %result = insertvalue undef, %r_final, 0
  %result2 = insertvalue %result, %g_final, 1
  %result3 = insertvalue %result2, %b_final, 2
  
  ret %result3
}