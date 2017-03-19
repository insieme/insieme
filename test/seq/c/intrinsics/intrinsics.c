#include <x86intrin.h>

// we actually don't compare any output here. All we need is Insieme being able to compile this code here.
int main() {
	__rdtsc();
	return 0;
}
