#pragma once 

namespace toIntercept{

	template <typename T>
	class Obj{
		int a;
	public:
		Obj (int x) : a(x) {}
		int get() { return a; }
	};

}// namespace
