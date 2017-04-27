#pragma once

#include <initializer_list>

struct Intercepted {
	Intercepted(const std::initializer_list<int>& x) {}
};
