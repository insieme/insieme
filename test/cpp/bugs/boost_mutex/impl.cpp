#include "header.h"

MutexType S::mutexInstStatic;

void S::useMutex() {
	boost::lock_guard<MutexType> lockStatic(mutexInstStatic);
	boost::lock_guard<MutexType> lock(mutexInst);
}

void S::lockMutex() {
	mutexInst.lock();
}

bool S::tryLockMutex() {
	return mutexInst.try_lock();
}
void S::unlockMutex() {
	mutexInst.unlock();
}
