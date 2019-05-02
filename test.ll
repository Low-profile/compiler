%struct.String = type { i32, [0 x i8] }
%struct.IntArray = type { i32, [0 x i32] }
declare i8* @kool_alloc(i32)
declare %struct.String* @string_fromint(i32)
declare %struct.String* @string_alloc(i32)
declare %struct.String* @string_create(i8*)
declare %struct.String* @string_concat(%struct.String*, %struct.String*)
declare i1 @string_equals(%struct.String*, %struct.String*)
declare %struct.IntArray* @array_alloc(i32)
declare void @println_int(i32)
declare void @println_string(%struct.String*)
declare void @println_bool(i1)




define i32 @f(i32 %i) {
}

define i32 @main(i32 %arg0, i32 %arg1, i32 %arg2) {
  %a = alloca i32
  store i32 0, i32* %a
  %b = alloca i32
  store i32 0, i32* %b
  %1 = load i32* %a
  %2 = icmp ne i32* %a, 0
  br i1 %2, label %true1, label %false1
  true1:
  %3 = load i32* %a
  %4 = load i32* %a
  %5 = add i32 %4, 2
  store i32 %5, i32 %3
  br label %end1
  false1:
  ret void
  br label %end1
  end1:
  br label %cond1
  cond1:
  %6 = icmp ne i32 1, 0
  br i1 %6, label %true2, label %end2
  true2:
  %7 = call i32 @f(i32 1)
  br label %cond1
  end2:
  %8 = load i32* %b
  store i32 0, i32 %8
  br label %true3
  true3:
  %9 = load i32* %b
  %10 = sub i32 0, 1
  store i32 %10, i32 %9
  %11 = load i32* %b
  %12 = load i32* %b
  %13 = add i32 %12, 2
  store i32 %13, i32 %11
  br label %true3
}

