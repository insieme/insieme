
#pragma once

template<int Dims>
struct Intercepted {
	Intercepted(const NotIntercepted<Dims>& x, int i) {}
};
