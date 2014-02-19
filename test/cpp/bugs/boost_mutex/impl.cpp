#include "header.h"

//FIXME: NOT WORKING
//MutexType S::mutexInstStatic;

void S::useMutex() {
//FIXME: NOT WORKING
//	boost::lock_guard<MutexType> lockStatic(mutexInstStatic);
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
