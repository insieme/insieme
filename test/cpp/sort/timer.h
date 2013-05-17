#include <sys/time.h>

namespace ayuso{

class timer{
	  struct timeval begin;
	  struct timeval end;
public:

	timer();
			
	void start();
	double stop();
	double elapsed();
	void reset();
};

}// namespace ayuso
