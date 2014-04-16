#include "header.h"

int main() {
	AF* p = AF::getInst();
	return 0;

	AF::getInst()->cleanup();
	globalx = 0;
}
