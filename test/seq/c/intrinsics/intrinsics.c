#include <x86intrin.h>

// we actually don't compare any output here. All we need is Insieme being able to compile this code here.
int main() {
	// use an intrinsic defined in ia32intrin.h, transitively included by x86intrin.h
	__rdtsc();

	// use a gcc intrinsic
	__builtin_ia32_pause();

	return 0;
}
