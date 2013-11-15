#include <sys/time.h>

using namespace std;


	class Timer {

		typedef long long unsigned time_t;

		time_t _start;

	public:

		void start() { 
			_start = getTime();
		};

		double stop() { 
			return (getTime() - _start) / (1000.0 * 1000.0 * 1000.0);
		};

	private:

		time_t getTime() const {
			struct timeval time;
			gettimeofday(&time, NULL); 
			return time.tv_sec * (time_t)(1000*1000*1000) + time.tv_usec;
		}

	};

