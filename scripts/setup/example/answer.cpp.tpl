#include "%PROJECT%/%MODULE%/answer.h"

#include <boost/optional.hpp>

namespace %PROJECT% {
namespace %MODULE% {

	int answer() {
		boost::optional<int> oi(42);
		return *oi;
	}

} // end namespace %MODULE%
} // end namespace %PROJECT%
