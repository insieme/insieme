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

#include "insieme/annotations/data_annotations.h"
#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

	typedef insieme::utils::map::PointerMap<core::VariablePtr, insieme::utils::map::PointerMap<core::ExpressionPtr, ACCESS_TYPE> > AccessMap;

	class KernelPoly {
		core::NodePtr& program;
		const core::lang::BasicGenerator basic;
		core::IRBuilder builder;

    	std::vector<core::ExpressionPtr> kernels;
    	std::vector<core::ExpressionPtr> transformedKernels;

    	/*
    	 * gets an expression after the replacements of insertIndutionVariable have been done and fixes some ref and cast related errors
    	 * @param expr A (maybe erroneous) expresson
    	 * @return expr after fixing some issues
    	 */
    	core::ExpressionPtr cleanUsingMapper(const core::ExpressionPtr& expr);

    	/*
    	 * transforms a kernel into a loop nest which is analyzable by the polyhedral model
    	 * @param kernel The kernel to be transformed
    	 * @return the transformed kernel
    	 */
    	core::StatementPtr transformKernelToLoopnest(core::ExpressionAddress kernel);

    	/*
    	 * tries to find kernel functions
    	 * @param lambda The node to be checked
    	 * @return true if the lambda is a kernel-function-call, false otherwise
    	 */
    	core::ExpressionPtr isKernelFct(const core::CallExprPtr& call);

    	/*
    	 * transforms a kernel to use the get_*_id functions directly where ever possible
    	 * @param kernel The kernel to be transformed
    	 * @return the transformed kernel
    	 */
    	core::ExpressionPtr insertInductionVariables(core::ExpressionPtr kernel);

    	/*
    	 * Generates a map with one entry for every global variable and an Expression for the lower and upper boundary for its accesses
    	 * @param
    	 * kernel the kernel function to be analyzed
    	 * @return
    	 * A map with one entry for each global variable containing a map which's keys are the expressions accessing it
    	 */
    	AccessMap collectArrayAccessIndices(core::ExpressionPtr kernel);

    	/*
    	 * Checks if the variable var is an induction variable of a loop inside kernel
    	 * @param var The variable to be checked
    	 * @param kernel The LambdaExpr in which's parameter list to search
    	 * @param lowerBound an empty ExpressionPtr reference in which the lowerBound of the loop is stored in case of success
    	 * @param upperBound an empty ExpressionPtr reference in which the upperBound of the loop is stored in case of success
    	 * @return true if var is found as an induction variable inside the kernel, false otherwise
    	 */
    	bool isInductionVariable(core::VariablePtr var, core::LambdaExprPtr kernel, core::ExpressionPtr& lowerBound, core::ExpressionPtr& upperBound);

    	/*
    	 * Checks if the variable var is a parameter of LambdaExpr kernel
    	 * @param var The variable to be checked
    	 * @param kernel The LambdaExpr in which's parameter list to search
    	 * @return true if var is found in the parameter list of kernel, false otherwise
    	 */
    	bool isParameter(core::VariablePtr var, core::LambdaExprPtr kernel);

    	/*
    	 * Takes the expression of the index argument of a subscript to a global variable and generates the lower and upper boundary of it,
    	 * trying to get rid of local and loop-induction variables. If this is not possible 0 (lower bound) and infinity (upper bound) are returned
    	 * @param access The index expression of a subscript operation
    	 * @param kernel The lambdaExression representing the kernel function
    	 * @param accessType The access type for this expression (read or write)
    	 * @return first the lower bound for the passed index expression
    	 *         second the upper bound for the passed index expression
    	 */
    	std::pair<core::ExpressionPtr, core::ExpressionPtr> genBoundaries(core::ExpressionPtr access, core::ExpressionPtr kernel, ACCESS_TYPE accessType);

    	/*
    	 * generates the Work Item - Data Item relation function for all kernels inside program
    	 */
    	void genWiDiRelation();

	public:
    	KernelPoly(core::NodePtr& program): program(program), basic(program->getNodeManager()), builder(program->getNodeManager()) {
    		this->genWiDiRelation();
    	}

    	std::vector<core::ExpressionPtr>& getKernels() { return kernels; }
    	std::vector<core::ExpressionPtr>& getTransformedKernels() { return transformedKernels; }

	};


} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

