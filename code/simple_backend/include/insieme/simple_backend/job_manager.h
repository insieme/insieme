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

#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"

#include "insieme/simple_backend/code_management.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace simple_backend {

class Converter;

/**
 * A simple class handling the creation of parallel jobs within the target code.
 */
class JobManager {

	/**
	 * A info struct maintaining all the information stored per job.
	 */
	struct JobInfo {
		string structName;			// the name of the struct holding local variables
		string funName;				// the name of the function to be executed
		CodeFragmentPtr structDefinition;	// the definition of the job struct
		CodeFragmentPtr jobFunction;		// the definition of the function computing the job
		vector<core::VariablePtr> capturedVars; 	// the variables to be captured within the job branches

		/**
		 * A simple constructor for this type.
		 */
		JobInfo(
				const string& structName,
				const string& funName,
				CodeFragmentPtr& structDef,
				CodeFragmentPtr& funDef,
				const vector<core::VariablePtr>& capturedVars)
			:
				structName(structName),
				funName(funName),
				structDefinition(structDef),
				jobFunction(funDef),
				capturedVars(capturedVars) { }
	};

	/**
	 * A info struct containing all the information to be stored for a pfor-body.
	 */
	struct PForBodyInfo {
		string functionName;		// the name of the function realizing the body
		string captureStructName;	// the name of the struct containing the captured variables
		CodeFragmentPtr funDefinition;		// the definition of the function
		CodeFragmentPtr captureStruct;		// the definition of the capture struct
		vector<core::VariablePtr> capturedVars; 	// the variables to be captured for this parallel for

		PForBodyInfo(
				const string& functionName,
				const string& captureStructName,
				const CodeFragmentPtr& funDefinition,
				const CodeFragmentPtr& captureStruct,
				const vector<core::VariablePtr> capturedVars)
			:
				functionName(functionName),
				captureStructName(captureStructName),
				funDefinition(funDefinition),
				captureStruct(captureStruct),
				capturedVars(capturedVars) { }
	};

	/**
	 * The conversion context this manager is part of.
	 */
	Converter& cc;

	/**
	 * A map linking jobs to their definitions within the resulting source code.
	 */
	utils::map::PointerMap<core::JobExprPtr, JobInfo> jobs;

	/**
	 * A map linking pfor body statements to their definitions within the resulting source code.
	 */
	utils::map::PointerMap<core::ExpressionPtr, PForBodyInfo> bodies;

public:

	/**
	 * Creates a new instance of a Job Manager.
	 */
	JobManager(Converter& conversionContext) : cc(conversionContext) { }

	/**
	 * Requests this manager to add the code required for creating a Job instance at the current place
	 * within the given context.
	 *
	 * @param context the context this job should be created in
	 * @param job the job to be created
	 */
	void createJob(const CodeFragmentPtr& context, const core::JobExprPtr& job);

	/**
	 * Creates a call to the pfor-runtime function based on the given call expression.
	 */
	void createPFor(const CodeFragmentPtr& context, const core::ExpressionPtr& pforCall);

private:

	/**
	 * Resolves the given job expression and obtains the necessary information for creating a job.
	 */
	JobInfo resolveJob(const core::JobExprPtr& job);

	/**
	 * Resolves the given pfor body statement.
	 */
	PForBodyInfo resolvePForBody(const core::ExpressionPtr& body);
};


}
}
