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

#include "insieme/analysis/cba/framework/_forward_decl.h"
#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/entities.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {

	// --- forward definitions of analysis utilized by the framework ---

	template<typename C> class ControlFlowConstraintGenerator;
	template<typename T> const DataAnalysisType<Callable<T>,ControlFlowConstraintGenerator>& C();
	template<typename T> const DataAnalysisType<Callable<T>,ControlFlowConstraintGenerator>& c();

	template<typename C> class Location;
	template<typename C> class ReferenceConstraintGenerator;
	template<typename C> const DataAnalysisType<Location<C>,ReferenceConstraintGenerator>& R();
	template<typename C> const DataAnalysisType<Location<C>,ReferenceConstraintGenerator>& r();

	class Callee;
	template<typename C> class FunctionConstraintGenerator;
	extern const DataAnalysisType<Callee,FunctionConstraintGenerator> F;
	extern const DataAnalysisType<Callee,FunctionConstraintGenerator> f;

	template<typename C> class ContextPredecessorGenerator;
	extern const SetBasedAnalysisType<Label,ContextPredecessorGenerator> pred;

	struct Reachable;
	template<typename C> class ReachableInConstraintGenerator;
	extern const SetBasedAnalysisType<Reachable,ReachableInConstraintGenerator> Rin;

	template<typename C> class ReachableOutConstraintGenerator;
	extern const SetBasedAnalysisType<Reachable,ReachableOutConstraintGenerator> Rout;

	class Formula;
	template<typename C> class ArithmeticConstraintGenerator;
	extern const DataAnalysisType<Formula,ArithmeticConstraintGenerator> A;
	extern const DataAnalysisType<Formula,ArithmeticConstraintGenerator> a;

	template<typename C> class BooleanConstraintGenerator;
	extern const DataAnalysisType<bool,BooleanConstraintGenerator> B;
	extern const DataAnalysisType<bool,BooleanConstraintGenerator> b;


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
