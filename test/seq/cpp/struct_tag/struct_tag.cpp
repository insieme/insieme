
#include <utility>

#include <cstdlib>
#include <sys/time.h>

void takePair(std::pair<int, int> p) {}

int main() {

	// Here we should not have the leading tag "struct" in the backend's output.
	// It won't really cause a problem most of the time, but we encountered situations where it does.
	std::pair<int, int> p;
	takePair(p);
	takePair(std::pair<int, int>({1, 2}));
	takePair({1, 2});

	// on the other hand, here on this C types we really need to have the "struct" tag, since for these types it only workes if it is present
	struct timeval tp;
	gettimeofday(&tp, NULL);
}
