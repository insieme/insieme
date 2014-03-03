#include <boost/thread.hpp>
typedef boost::mutex MutexType;

struct S {
	MutexType mutexInst;
	//FIXME: NOT WORKING
	static MutexType mutexInstStatic;
	
	void useMutex();
	void lockMutex();
	bool tryLockMutex();
	void unlockMutex();
};
