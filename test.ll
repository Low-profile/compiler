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




define i32 @main(i32 %int) {
  %1 = alloca [5 x i1]
}

