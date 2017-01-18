

#pragma once

namespace inner {

	/**
	 * A non-copy able class ... like a mutex.
	 */
	struct M {
		M(){}
	private:
		M(const M&);
		M& operator=(const M&);
	};

} // end namespace inner

