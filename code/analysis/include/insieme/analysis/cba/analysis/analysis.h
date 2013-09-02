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

#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/entities.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {
//
//	// --- forward definitions of known analysis ---
//
	class Formula;
	template<typename C> class ArithmeticConstraintResolver;
	extern const TypedSetType<Formula,ArithmeticConstraintResolver> A;
	extern const TypedSetType<Formula,ArithmeticConstraintResolver> a;

	template<typename C> class BooleanConstraintResolver;
	extern const TypedSetType<bool,BooleanConstraintResolver> B;
	extern const TypedSetType<bool,BooleanConstraintResolver> b;

	template<typename C> class ControlFlowConstraintResolver;
	template<typename T> const TypedSetType<Callable<T>,ControlFlowConstraintResolver>& C();
	template<typename T> const TypedSetType<Callable<T>,ControlFlowConstraintResolver>& c();

	template<typename C> class Location;
	template<typename C> class ReferenceConstraintResolver;
	template<typename C> const TypedSetType<Location<C>,ReferenceConstraintResolver>& R();
	template<typename C> const TypedSetType<Location<C>,ReferenceConstraintResolver>& r();

	typedef core::ExpressionAddress ContextFreeCallable;
	template<typename C> class FunctionConstraintResolver;
	extern const TypedSetType<ContextFreeCallable,FunctionConstraintResolver> F;
	extern const TypedSetType<ContextFreeCallable,FunctionConstraintResolver> f;

	template<typename C> class ContextPredecessorResolver;
	extern const TypedSetType<Label,ContextPredecessorResolver> pred;

	struct Reachable;
	template<typename C> class ReachableInConstraintResolver;
	extern const TypedSetType<Reachable,ReachableInConstraintResolver> Rin;

	template<typename C> class ReachableOutConstraintResolver;
	extern const TypedSetType<Reachable,ReachableOutConstraintResolver> Rout;

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
