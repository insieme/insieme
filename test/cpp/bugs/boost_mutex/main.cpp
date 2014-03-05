#include "header.h"

int main() {
	S x;
	x.useMutex();

	std::cout << x.tryLockMutex() << std::endl;
	x.unlockMutex();
	
	{
		boost::lock_guard<MutexType> lock(x.mutexInst);
		std::cout << x.tryLockMutex() << std::endl;
	}

	std::cout << x.tryLockMutex() << std::endl;
	x.unlockMutex();

	D d;

	return 0;
}
