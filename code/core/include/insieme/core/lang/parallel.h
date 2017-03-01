/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include "insieme/core/lang/extension.h"

#include "insieme/core/lang/array.h"

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Parallel Constructs ----------------------------


	/**
	 * An extension covering inspires parallel primitives.
	 */
	class ParallelExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ParallelExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		// import required modules
		IMPORT_MODULE(ArrayExtension);


		// -- Parallel Primitives -------------------------------------------------------------------------------------------

		// ---- spawn / merge ----

		/**
		 * The primitive spawning thread groups.
		 */
		LANG_EXT_LITERAL(Parallel, "parallel", "(job) -> threadgroup")

		/**
		 * The primitive waiting for the termination of a given thread group.
		 */
		LANG_EXT_LITERAL(Merge, "merge", "(threadgroup) -> unit")

		/**
		 * The primitive waiting for the termination of all thread groups spawned by the current thread.
		 */
		LANG_EXT_LITERAL(MergeAll, "merge_all", "() -> unit")


		// ---- identification ----

		/**
		 * The abstract type modeling thread groups.
		 */
		LANG_EXT_TYPE(ThreadGroup, "threadgroup")

		/**
		 * The primitive to get a handler to an enclosing thread group.
		 */
		LANG_EXT_LITERAL(GetThreadGroup, "get_thread_group", "(uint<'a>) -> threadgroup")

		/**
		 * The primitive to get a the index of the local thread in an enclosing thread group.
		 */
		LANG_EXT_LITERAL(GetThreadId, "get_thread_id", "(uint<'a>) -> uint<4>")

		/**
		 * The primitive to get the size of an enclosing thread group.
		 */
		LANG_EXT_LITERAL(GetGroupSize, "get_group_size", "(uint<'a>) -> uint<4>")


		// ---- collective operations ----

		/**
		 * The single work-sharing construct distributing workload among the threads of a group.
		 * This construct is a derived one to model its 'effect' for the analysis.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PFor, "pfor", R"(
			(g : threadgroup, a : int<'a>, b : int<'a>, c : int<'a>, f : (int<'a>, int<'a>, int<'a>)=>'b)->unit {
				f(a,b,c);
			}
		)")

		/**
		 * The single collective data distribution construct enabling threads of a work-group to distribute data
		 * among its members.
		 */
		LANG_EXT_LITERAL(Redistribute, "redistribute", "(threadgroup, 'a, (ref<array<'a>>, uint<8>, uint<8>)=>'b )->'b")


		// -- Parallel Operators --------------------------------------------------------------------------------------------

		/**
		 * A derived operator implementing a barrier among the threads of an enclosing thread group based on a
		 * redistribute call by utilizing its synchronizing side-effect.
		 */
		LANG_EXT_DERIVED(Barrier, R"(
			(g : threadgroup)->unit {
				redistribute(g, 0, (_ : ref<array<int<4>>>, _ : uint<8>, _ : uint<8>)->unit { });
			}
		)")

		/**
		 * A derived operator implementing a reduction among the threads of a thread group where every thread
		 * is contributing an element and a reduction operation and all threads receive the aggregated value.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PReduce, "preduce", R"(
			(g : threadgroup, v : 'a, op : ('b,'a)->'b, init : 'b)->'b {
				return redistribute(g, v,
				                    (data : ref<array<'a>>, size : uint<8>, tid : uint<8>) => array_reduce(data, num_cast(size, type_lit(int<8>)), op, init));
			}
		)")


		// -- Thread Group Size Handling ------------------------------------------------------------------------------------

		/**
		 * The type of value utilized to specify the possible range of jobs in a thread group.
		 */
		LANG_EXT_TYPE(JobRange, "JobRange")

		/**
		 * A job-range constructor defining only a lower boundary for the number of threads.
		 */
		LANG_EXT_LITERAL(CreateMinRange, "create_min_range", "(uint<8>)->JobRange")

		/**
		 * A job-range constructor defining a lower and upper boundary for the number of threads.
		 */
		LANG_EXT_LITERAL(CreateBoundRange, "create_bound_range", "(uint<8>, uint<8>)->JobRange")

		/**
		 * A job-range constructor defining a lower and upper boundary as well as a modula constraint for the number of threads.
		 */
		LANG_EXT_LITERAL(CreateBoundRangeMod, "create_bound_range_mod", "(uint<8>, uint<8>, uint<8>)->JobRange")


		// -- Locks ---------------------------------------------------------------------------------------------------------

		/**
		 * A type for a simple mutex lock.
		 */
		LANG_EXT_TYPE(Lock, "lock")

		/**
		 * A constructor for a mutex lock.
		 */
		LANG_EXT_LITERAL(LockInit, "lock_init", "(ref<lock>)->unit")

		/**
		 * An operator for acquiring a mutex lock.
		 */
		LANG_EXT_LITERAL(LockAcquire, "lock_acquire", "(ref<lock>)->unit")

		/**
		 * An operator for acquiring a mutex lock.
		 * In contrast to LockAcquire, this function returns immediately,
		 * if the lock has already been acquired by any thread
		 */
		LANG_EXT_LITERAL(LockTryAcquire, "lock_tryacquire", "(ref<lock>)->bool")

		/**
		 * An operator for releasing a mutex lock.
		 */
		LANG_EXT_LITERAL(LockRelease, "lock_release", "(ref<lock>)->unit")


		// -- Atomic Primitives ---------------------------------------------------------------------------------------------


		/**
		 * An atomic operation is a check & update operation that is conducted without the risk
		 * of concurrent interference of other threads on the targeted memory location.
		 *
		 * The primitive is modeled as a derived operator to cover its sequential semantic
		 * to be evaluated by the analysis framework. However, the synchronizing effect
		 * is 'implicit' and needs to be covered by the framework itself.
		 */
		LANG_EXT_DERIVED(Atomic, R"(
			(v : ref<'a, f,'v>, p : ('a)=>bool, f : ('a)=>'a)->'a {
				auto res = *v;
				if (p(*v)) {
					v = f(*v);
				}
				return res;
			}
		)")


		// -- Atomic Operators ----------------------------------------------------------------------------------------------


		// arithmetic

		LANG_EXT_DERIVED(AtomicFetchAndAdd, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				let test = (_ : 'a)=>true;
				let apply = (x : 'a)=>x+exp;
				return atomic(v, test, apply);
			}
		)")

		LANG_EXT_DERIVED(AtomicAddAndFetch, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				return atomic_fetch_and_add(v, exp) + exp;
			}
		)")

		LANG_EXT_DERIVED(AtomicFetchAndSub, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				let test = (_ : 'a)=>true;
				let apply = (x : 'a)=>x-exp;
				return atomic(v, test, apply);
			}
		)")

		LANG_EXT_DERIVED(AtomicSubAndFetch, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				return atomic_fetch_and_sub(v, exp) - exp;
			}
		)")

		// bitwise

		LANG_EXT_DERIVED(AtomicFetchAndAnd, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				let test = (_ : 'a) => true;
				let apply = (x : 'a) => x & exp;
				return atomic(v, test, apply);
			}
		)")

		LANG_EXT_DERIVED(AtomicAndAndFetch, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				return atomic_fetch_and_and(v, exp) & exp;
			}
		)")

		LANG_EXT_DERIVED(AtomicFetchAndOr, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				let test = (_ : 'a) => true;
				let apply = (x : 'a) => x | exp;
				return atomic(v, test, apply);
			}
		)")

		LANG_EXT_DERIVED(AtomicOrAndFetch, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				return atomic_fetch_and_or(v, exp) | exp;
			}
		)")

		LANG_EXT_DERIVED(AtomicFetchAndXor, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				let test = (_ : 'a) => true;
				let apply = (x : 'a) => x ^ exp;
				return atomic(v, test, apply);
			}
		)")

		LANG_EXT_DERIVED(AtomicXorAndFetch, R"(
			(v : ref<'a,f,'v>, exp : 'a) -> 'a {
				return atomic_fetch_and_xor(v, exp) ^ exp;
			}
		)")

		// test and set

		LANG_EXT_DERIVED(AtomicValCompareAndSwap, R"(
			(v : ref<'a,f,'v>, _old : 'a, _new : 'a) -> 'a {
				let test = (x : 'a) => x == _old;
				let apply = (_ : 'a) => _new;
				return atomic(v, test, apply);
			}
		)")

		LANG_EXT_DERIVED(AtomicBoolCompareAndSwap, R"(
			(v : ref<'a,f,'v>, _old : 'a, _new : 'a) -> bool {
				let test = (x : 'a) => x == _old;
				let apply = (_ : 'a) => _new;
				return atomic(v, test, apply) == _new;
			}
		)")


		// An extension representing a busy waiting loop
		LANG_EXT_DERIVED(BusyLoop, R"(
			(condition : ()=>bool) -> unit {
				while(condition()) { }
			}
		)")


		// Derived helpers

		LANG_EXT_DERIVED(GetDefaultThreads, R"(
			() -> uint<4> {
				var ref<uint<4>> nt = 0u;
				merge(parallel(job { if(get_thread_id(0u)==0u) { nt = get_group_size(0u); } }));
				return *nt;
			}
		)")

	};

} // end namespace lang
} // end namespace core
} // end namespace insieme
