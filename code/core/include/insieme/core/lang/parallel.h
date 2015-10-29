/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/lang/extension.h"

#include "insieme/core/lang/reference.h"
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
		IMPORT_MODULE(ReferenceExtension);
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
		LANG_EXT_LITERAL(MergeAll, "mergeAll", "() -> unit")


		// ---- identification ----

		/**
		 * The abstract type modeling thread groups.
		 */
		LANG_EXT_TYPE(ThreadGroup, "threadgroup")

		/**
		 * The primitive to get a handler to an enclosing thread group.
		 */
		LANG_EXT_LITERAL(GetThreadGroup,    "getThreadGroup",     "(uint<'a>) -> threadgroup")

		/**
		 * The primitive to get a the index of the local thread in an enclosing thread group.
		 */
		LANG_EXT_LITERAL(GetThreadId,       "getThreadID",        "(uint<'a>) -> uint<4>"    )

		/**
		 * The primitive to get the size of an enclosing thread group.
		 */
		LANG_EXT_LITERAL(GetGroupSize,      "getGroupSize",       "(uint<'a>) -> uint<4>"    )


		// ---- collective operations ----

		/**
		 * The single work-sharing construct distributing workload among the threads of a group.
		 * This construct is a derived one to model its 'effect' for the analysis.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PFor, "pfor",
                "                                                                                                        "
				"	lambda (threadgroup g, int<'a> a, int<'a> b, int<'a> c, (int<'a>, int<'a>, int<'a>)=>'b f)->unit {   "
				"				f(a,b,c);                                                                                "
				"	}                                                                                                    "
	            "                                                                                                        "
		)

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
		LANG_EXT_DERIVED_WITH_NAME(Barrier, "barrier",
				    "                                                                                                    "
					"	lambda (threadgroup g)->unit {                                                                   "
					"		redistribute(g, 0, lambda (ref<array<int<4>>> _, uint<8> _, uint<8> _)->unit { return; });   "
					"	}                                                                                                "
		            "                                                                                                    "
		)

		/**
		 * A derived operator implementing a reduction among the threads of a thread group where every thread
		 * is contributing an element and a reduction operation and all threads receive the aggregated value.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PReduce, "preduce",
				    "                                                                                                                                        "
					"	lambda (threadgroup g, 'a v, ('b,'a)->'b op, 'b init)->'b {                                                                          "
					"	   return redistribute(g, v,                                                                                                         "
					"				  lambda (ref<array<'a>> data, uint<8> size, uint<8> tid) => array_reduce(data, num_cast(size,type_lit(int<8>)), op, init)   "
					"	          );                                                                                                                         "
					"	}                                                                                                                                    "
		            "                                                                                                                                        "
		)


		// -- Thread Group Size Handling ------------------------------------------------------------------------------------

		/**
		 * The type of value utilized to specify the possible range of jobs in a thread group.
		 */
		LANG_EXT_TYPE(JobRange, "JobRange")

		/**
		 * A job-range constructor defining only a lower boundary for the number of threads.
		 */
		LANG_EXT_LITERAL(CreateMinRange, "MinRange", "(uint<8>)->JobRange")

		/**
		 * A job-range constructor defining a lower and upper boundary for the number of threads.
		 */
		LANG_EXT_LITERAL(CreateBoundRange, "BoundRange", "(uint<8>, uint<8>)->JobRange")

		/**
		 * A job-range constructor defining a lower and upper boundary as well as a modula constraint for the number of threads.
		 */
		LANG_EXT_LITERAL(CreateBoundRangeMod, "BoundRangeMod", "(uint<8>, uint<8>, uint<8>)->JobRange")


		// -- Locks ---------------------------------------------------------------------------------------------------------

		/**
		 * A type for a simple mutex lock.
		 */
		LANG_EXT_TYPE(Lock, "lock")

		/**
		 * A constructor for a mutex lock.
		 */
		LANG_EXT_LITERAL(LockInit,    "lock_init",    "(ref<lock>)->unit")

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
		LANG_EXT_DERIVED_WITH_NAME(Atomic, "atomic",
				    "                                                              "
					"	lambda (ref<'a, f,'v> v, ('a)=>bool p, ('a)=>'a f)->'a {   "
					"		decl auto res = *v;                                    "
					"		if (p(*v)) {                                           "
					"			v = f(*v);                                         "
					"		}                                                      "
					"		return res;                                            "
					"	}                                                          "
				    "                                                              "
		)


		// -- Atomic Operators ----------------------------------------------------------------------------------------------


		// arithmetic

		LANG_EXT_DERIVED_WITH_NAME(AtomicFetchAndAdd, "atomic_fetch_and_add", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	let test = lambda ('a _)=>true; "
														   "	let apply = lambda ('a x)=>x+exp; "
														   "	return atomic(v, test, apply); "
														   "}  ")

	   LANG_EXT_DERIVED_WITH_NAME(AtomicAddAndFetch, "atomic_add_and_fetch", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	return atomic_fetch_and_add(v, exp) + exp; "
														   "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicFetchAndSub, "atomic_fetch_and_sub", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	let test = lambda ('a _)=>true; "
														   "	let apply = lambda ('a x)=>x-exp; "
														   "	return atomic(v, test, apply); "
														   "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicSubAndFetch, "atomic_sub_and_fetch", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	return atomic_fetch_and_sub(v, exp) - exp; "
														   "}  ")

		// bitwise

		LANG_EXT_DERIVED_WITH_NAME(AtomicFetchAndAnd, "atomic_fetch_and_and", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	let test = lambda ('a _) => true; "
														   "	let apply = lambda ('a x) => x & exp; "
														   "	return atomic(v, test, apply); "
														   "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicAndAndFetch, "atomic_and_and_fetch", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	return atomic_fetch_and_and(v, exp) & exp; "
														   "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicFetchAndOr, "atomic_fetch_and_or", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														 "	let test = lambda ('a _) => true; "
														 "	let apply = lambda ('a x) => x | exp; "
														 "	return atomic(v, test, apply); "
														 "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicOrAndFetch, "atomic_or_and_fetch", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														 "	return atomic_fetch_and_or(v, exp) | exp; "
														 "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicFetchAndXor, "atomic_fetch_and_xor", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	let test = lambda ('a _) => true; "
														   "	let apply = lambda ('a x) => x ^ exp; "
														   "	return atomic(v, test, apply); "
														   "}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicXorAndFetch, "atomic_xor_and_fetch", "lambda (ref<'a,f,'v> v, 'a exp) -> 'a { "
														   "	return atomic_fetch_and_xor(v, exp) ^ exp; "
														   "}  ")

		// test and set

		LANG_EXT_DERIVED_WITH_NAME(AtomicValCompareAndSwap, "atomic_val_compare_and_swap", "lambda (ref<'a,f,'v> v, 'a _old, 'a _new) -> 'a { "
																		"	let test = lambda ('a x) => x == _old; "
																		"	let apply = lambda ('a _) => _new; "
																		"	return atomic(v, test, apply); "
																		"}  ")

		LANG_EXT_DERIVED_WITH_NAME(AtomicBoolCompareAndSwap, "atomic_bool_compare_and_swap", "lambda (ref<'a,f,'v> v, 'a _old, 'a _new) -> bool { "
																		  "	let test = lambda ('a x) => x == _old; "
																		  "	let apply = lambda ('a _) => _new; "
																		  "	return atomic(v, test, apply) == _new; "
																		  "}  ")


		// An extension representing a busy waiting loop
		LANG_EXT_DERIVED(BusyLoop, "lambda (()=>bool condition) -> unit { while(condition()) { } }");

//
//		GROUP(ParallelOp, Parallel, ParallelDetached)
//
//		LITERAL(Parallel, "parallel", "(job) -> threadgroup")
//		LITERAL(ParallelDetached, "parallel&", "(job) -> unit")
//
//		GROUP(MergeOp, Merge, MergeAll)
//
//		LITERAL(Merge, "merge", "(threadgroup) -> unit")
//		LITERAL(MergeAll, "mergeAll", "() -> unit")
//
//
//		GROUP(ThreadOp, GetThreadGroup, GetThreadId)
//
//		LITERAL(GetThreadGroup, "getThreadGroup", "(uint<#a>) -> threadgroup")
//		LITERAL(GetThreadId, "getThreadID", "(uint<#a>) -> int<4>")
//		LITERAL(GetGroupSize, "getGroupSize", "(uint<#a>) -> int<4>")
//
//		// the work-sharing construct
//		DERIVED(PFor, "pfor", "lambda (threadgroup g, int<#a> a, int<#a> b, int<#a> c, (int<#a>, int<#a>, int<#a>)=>'a f)->unit { f(a,b,c); }")
//
//		// the data-sharing construct
//		LITERAL(Redistribute, "redistribute", "(threadgroup, 'a, (ref<array<'a,1>>, uint<8>, uint<8>)=>'b )->'b")
//
//		// some derivades
//		DERIVED(Barrier, "barrier", "lambda (threadgroup g)->unit { redistribute(g, 0, lambda (ref<array<int<4>,1>> _, uint<8> _, uint<8> _)->unit { return; }); }")
//		DERIVED(PReduce, "preduce", "lambda (threadgroup g, 'a v, ('b,'a)->'b op, 'b init)->'b { "
//		                            "   return redistribute(g, v, "
//		                            "              lambda (ref<array<'a,1>> data, uint<8> size, uint<8> pid)=> array_reduce(data, size, op, init) );"
//		                            "}")
//
//		// Channels -----------------------------------------------------------------------------------------------------------
//
//		LITERAL(ChannelCreate, "channel_create", "(type<'a>, intTypeParam<#n>) -> channel<'a,#n>")
//		LITERAL(ChannelRelease, "channel_release", "(channel<'a,#n>) -> unit")
//
//		LITERAL(ChannelSend, "channel_send", "(channel<'a,#n>, 'a) -> unit")
//		LITERAL(ChannelRecv, "channel_recv", "(channel<'a,#n>) -> 'a")
//		LITERAL(ChannelProbe, "channel_probe", "(channel<'a,#n>) -> bool")
//
//		// Atomics ------------------------------------------------------------------------------------------------------------
//
//		DERIVED(Atomic, "atomic", "lambda (ref<'a> v, ('a)=>bool p, ('a)=>'a f)->'a { "
//		                          "	decl auto res = *v; "
//		                          "	if (p(*v)) { "
//		                          "		v = f(*v); "
//		                          "	} "
//		                          "	return res; "
//		                          "} ")
//
//		// arithmetic
//
//		DERIVED(AtomicFetchAndAdd, "atomic_fetch_and_add", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	let test = lambda ('a _)=>true; "
//		                                                   "	let apply = lambda ('a x)=>x+exp; "
//		                                                   "	return atomic(v, test, apply); "
//		                                                   "}  ")
//
//		DERIVED(AtomicAddAndFetch, "atomic_add_and_fetch", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	return atomic_fetch_and_add(v, exp) + exp; "
//		                                                   "}  ")
//
//		DERIVED(AtomicFetchAndSub, "atomic_fetch_and_sub", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	let test = lambda ('a _)=>true; "
//		                                                   "	let apply = lambda ('a x)=>x-exp; "
//		                                                   "	return atomic(v, test, apply); "
//		                                                   "}  ")
//
//		DERIVED(AtomicSubAndFetch, "atomic_sub_and_fetch", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	return atomic_fetch_and_sub(v, exp) - exp; "
//		                                                   "}  ")
//
//		// bitwise
//
//		DERIVED(AtomicFetchAndAnd, "atomic_fetch_and_and", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	let test = lambda ('a _) => true; "
//		                                                   "	let apply = lambda ('a x) => x & exp; "
//		                                                   "	return atomic(v, test, apply); "
//		                                                   "}  ")
//
//		DERIVED(AtomicAndAndFetch, "atomic_and_and_fetch", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	return atomic_fetch_and_and(v, exp) & exp; "
//		                                                   "}  ")
//
//		DERIVED(AtomicFetchAndOr, "atomic_fetch_and_or", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                 "	let test = lambda ('a _) => true; "
//		                                                 "	let apply = lambda ('a x) => x | exp; "
//		                                                 "	return atomic(v, test, apply); "
//		                                                 "}  ")
//
//		DERIVED(AtomicOrAndFetch, "atomic_or_and_fetch", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                 "	return atomic_fetch_and_or(v, exp) | exp; "
//		                                                 "}  ")
//
//		DERIVED(AtomicFetchAndXor, "atomic_fetch_and_xor", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	let test = lambda ('a _) => true; "
//		                                                   "	let apply = lambda ('a x) => x ^ exp; "
//		                                                   "	return atomic(v, test, apply); "
//		                                                   "}  ")
//
//		DERIVED(AtomicXorAndFetch, "atomic_xor_and_fetch", "lambda (ref<'a> v, 'a exp) -> 'a { "
//		                                                   "	return atomic_fetch_and_xor(v, exp) ^ exp; "
//		                                                   "}  ")
//
//		// test and set
//
//		DERIVED(AtomicValCompareAndSwap, "atomic_val_compare_and_swap", "lambda (ref<'a> v, 'a _old, 'a _new) -> 'a { "
//		                                                                "	let test = lambda ('a x) => x == _old; "
//		                                                                "	let apply = lambda ('a _) => _new; "
//		                                                                "	return atomic(v, test, apply); "
//		                                                                "}  ")
//
//		DERIVED(AtomicBoolCompareAndSwap, "atomic_bool_compare_and_swap", "lambda (ref<'a> v, 'a _old, 'a _new) -> bool { "
//		                                                                  "	let test = lambda ('a x) => x == _old; "
//		                                                                  "	let apply = lambda ('a _) => _new; "
//		                                                                  "	return atomic(v, test, apply) == _new; "
//		                                                                  "}  ")
//
//
//		// Range Handling -----------------------------------------------------------------------------------------------------
//
//		TYPE(JobRange, "JobRange")
//
//		LITERAL(CreateMinRange, "MinRange", "(uint<8>)->JobRange")                             // lower boundery only
//		LITERAL(CreateBoundRange, "BoundRange", "(uint<8>, uint<8>)->JobRange")                // lower and upper bound
//		LITERAL(CreateBoundRangeMod, "BoundRangeMod", "(uint<8>, uint<8>, uint<8>)->JobRange") // lower, upper and modula bound
//
//
//		// Locks -----------------------------------------------------------
//
//		TYPE(Lock, "lock")
//
//		LITERAL(LockAcquire, "lock_acquire", "(ref<lock>)->unit")
//		LITERAL(LockRelease, "lock_release", "(ref<lock>)->unit")
//		LITERAL(LockInit, "lock_init", "(ref<lock>)->unit")
//
//		// --------------------------------------------------------------------------------------------------------------------


	};

} // end namespace lang
} // end namespace core
} // end namespace insieme
