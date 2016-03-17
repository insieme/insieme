
#include <iostream>
#include <boost/thread.hpp>

int f(){
	static boost::mutex mut;
	mut.lock();
	mut.unlock();
	return 124;
}


int g(){
	return 0;
}


int main (){
	return 0;
}

