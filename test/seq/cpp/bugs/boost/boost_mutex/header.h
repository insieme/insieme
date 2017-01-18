#include <boost/thread.hpp>
typedef boost::mutex MutexType;

struct S {
	int m;
	S(int x=1) : m(x) { }
	mutable MutexType mutexInstMutable;
	MutexType mutexInst;
	static MutexType mutexInstStatic;
	
	void useMutex();
	void lockMutex();
	bool tryLockMutex();
	void unlockMutex();
};

struct D : public S {
	D() { }
};
