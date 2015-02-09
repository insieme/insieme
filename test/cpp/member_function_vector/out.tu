TU(
	Types:
		F_struct_A_ => struct F_struct_A_ <ops:std::vector<(A::()->unit),std::allocator<(A::()->unit)>>>
		__gnu_cxx::new_allocator<char>::const_reference => struct<_const_cpp_ref:ref<char>>
		__gnu_cxx::new_allocator<void (A::*)()>::const_reference => struct<_const_cpp_ref:ref<(A::()->unit)>>
		__gnu_cxx::new_allocator<wchar_t>::const_reference => struct<_const_cpp_ref:ref<wchar<32>>>
		std::allocator<void (A::*)()>::reference => struct<_cpp_ref:ref<(A::()->unit)>>
		__gnu_cxx::new_allocator<wchar_t>::reference => struct<_cpp_ref:ref<wchar<32>>>
		__gnu_cxx::new_allocator<char>::reference => struct<_cpp_ref:ref<char>>
		std::vector<void (A::*)(), std::allocator<void (A::*)()> >::const_reference => struct<_const_cpp_ref:ref<(A::()->unit)>>
		__gnu_cxx::new_allocator<void (A::*)()>::reference => struct<_cpp_ref:ref<(A::()->unit)>>
		A => struct A : [F_struct_A_] <>
		std::vector<void (A::*)(), std::allocator<void (A::*)()> >::reference => struct<_cpp_ref:ref<(A::()->unit)>>
		std::allocator<void (A::*)()>::const_reference => struct<_const_cpp_ref:ref<(A::()->unit)>>,
	Globals:
		__gnu_cxx___numeric_traits_integer___is_signed:ref<bool> => <uninitalized>
		__gnu_cxx___numeric_traits_integer___digits:ref<int<4>> => <uninitalized>
		__gnu_cxx___numeric_traits_floating___max_digits10:ref<int<4>> => <uninitalized>
		__gnu_cxx___numeric_traits_floating___is_signed:ref<bool> => true
		__gnu_cxx___numeric_traits_floating___digits10:ref<int<4>> => <uninitalized>
		__gnu_cxx___numeric_traits_floating___max_exponent10:ref<int<4>> => <uninitalized>,
	Initializer:
		,
	Functions:
		operator_new : ((uint<8>,ref<any>)->ref<any>) => fun(uint<8> v11, ref<any> v12) -> ref<any> {return v12;}
		main : (()->int<4>) => fun() -> int<4> {decl ref<A> v28 = ctor struct A : F_struct_A_ <> v1 :: () { }( var(undefined(type<A>)));A_doSomething(v28, 12);return 0;}
		operator_delete[] : ((ref<any>,ref<any>)->unit) => fun(ref<any> v22, ref<any> v23) -> unit { }
		operator_new[] : ((uint<8>,ref<any>)->ref<any>) => fun(uint<8> v17, ref<any> v18) -> ref<any> {return v18;}
		F_A__F : (F_struct_A_::()) => ctor F_struct_A_ v24 :: () {{v24->ops := std::vector( var(undefined(type<std::vector<A::() -> unit,std::allocator<A::() -> unit>>>)));};A_f;}
		A_f : (A::()->unit) => mfun A v25 :: () -> unit { }
		A_doSomething : (A::(int<4>)->unit) => mfun A v27 :: (int<4> v26) -> unit { }
		operator_delete : ((ref<any>,ref<any>)->unit) => fun(ref<any> v20, ref<any> v21) -> unit { },
	Entry Points:	{}
)