/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#pragma once

#include <array>

#include "insieme/analysis/cba/framework/forward_decl.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * A type representing fixed-size value sequences - a base type for call and thread-context implementations.
	 *
	 * @tparam T the type of the sequence elements
	 * @tparam s the size of the sequence
	 */
	template<typename T, unsigned s>
	struct Sequence : public utils::Printable {

		// some constants
		enum { size = s, empty = (s==0) };

		// the actual sequence
		std::array<T, s> sequence;

		// -- constructors -----------

		Sequence() : sequence() {}
		Sequence(const Sequence<T,s>& other) : sequence(other.sequence) {}
		Sequence(const std::array<T,s>& sequence) : sequence(sequence) {}

		template<typename ... E>
		Sequence(const E& ... e) : sequence((std::array<T,s>){{e...}}) {}

		// -- inspectors -----------

		const T& front() const {
			assert(!empty);
			return sequence.front();
		}

		const T& back() const {
			assert(!empty);
			return sequence.back();
		}

		bool startsWith(const T& e) const {
			return s == 0 || sequence.front() == e;
		}

		bool endsWith(const T& e) const {
			return s == 0 || sequence.back() == e;
		}

		// -- operators -----------

		bool operator==(const Sequence& other) const {
			return empty || this == &other || sequence == other.sequence;
		}

		bool operator!=(const Sequence& other) const {
			return !(*this == other);
		}

		bool operator<(const Sequence& other) const {
			return !empty && this != &other && sequence < other.sequence;
		}

		const T& operator[](std::size_t index) const {
			assert_lt(index, s) << "Sequence-Index out of bound!";
			return sequence[index];
		}

		Sequence<T,s>& operator<<=(const Label& label) {
			if (empty) return *this;
			for(unsigned i=0; i<(s-1); ++i) {
				sequence[i] = sequence[i+1];
			}
			sequence[s-1] = label;
			return *this;
		}

		Sequence<T,s>& operator>>=(const Label& label) {
			if (empty) return *this;
			for(int i=(s-1); i>=0; --i) {
				sequence[i] = sequence[i-1];
			}
			sequence[0] = label;
			return *this;
		}

		Sequence<T,s> operator<<(const Label& label) const {
			return Sequence(*this)<<=label;
		}

		Sequence<T,s> operator>>(const Label& label) const {
			return Sequence(*this)>>=label;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << sequence;
		};
	};

	/**
	 * The class representing threads.
	 */
	template<unsigned s>
	class ThreadID : public utils::Printable {

		Label spawn;
		Sequence<Label, s> spawnContext;
		int id;

	public:

		ThreadID() : spawn(0), spawnContext(), id(0) {}

		bool operator==(const ThreadID& other) const {
			return spawn == other.spawn && id == other.id && spawnContext == other.spawnContext;
		}

		bool operator<(const ThreadID& other) const {
			if (spawn != other.spawn) return spawn < other.spawn;
			if (id != other.id) return id < other.id;
			return spawnContext < other.spawnContext;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "<" << spawn << "," << spawnContext << "," << id << ">";
		};
	};

	/**
	 * The generic context class associated to
	 */
//	template<
//		unsigned call_context_size = 2,
//		unsigned thread_context_length = 2,
//		unsigned thread_spawn_context_length = 0
//	>
	struct Context : public utils::Printable {

//		typedef Sequence<Label, call_context_size> CallContext;
//
//		typedef ThreadID<thread_spawn_context_length> ThreadID;
//		typedef Sequence<ThreadID, thread_context_length> ThreadContext;

		typedef Sequence<Label, 2> CallContext;
		typedef Sequence<ThreadID<0>, 2> ThreadContext;

		CallContext callContext;
		ThreadContext threadContext;

		Context()
			: callContext(), threadContext() {}

		Context(const CallContext& callContext, const ThreadContext& threadContext = ThreadContext())
			: callContext(callContext), threadContext(threadContext) {}

		Context(const Context& other)
			: callContext(other.callContext), threadContext(other.threadContext) {}

		bool operator==(const Context& other) const {
			return this == &other ||
					(callContext == other.callContext && threadContext == other.threadContext);
		}

		bool operator!=(const Context& other) const {
			return !(*this == other);
		}

		bool operator<(const Context& other) const {
			if (*this == other) return false;
			if (callContext != other.callContext) return callContext < other.callContext;
			if (threadContext != other.threadContext) return threadContext < other.threadContext;
			assert(false && "How did you get here?");
			return false;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "[" << callContext << "," << threadContext << "]";
		};
	};

//	typedef Context<0,0,0> NoContext;
//	typedef Context<2,2,0> Context;

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
