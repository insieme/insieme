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

#include "insieme/simple_backend/job_manager.h"

#include "insieme/simple_backend/backend_convert.h"

#include "insieme/core/ast_builder.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace simple_backend {

namespace {

	/**
	 * A pre-processing step for Job branches removing unnecessary capture init expressions.
	 */
	const ExpressionPtr& preprocessJobBranch(const ExpressionPtr& expression) {
		if (expression->getNodeType() == NT_CaptureInitExpr) {
			CaptureInitExprPtr init = static_pointer_cast<const CaptureInitExpr>(expression);
			if (init->getValues().empty()) {
				return init->getLambda();
			}
		}
		return expression;
	}

}


void JobManager::createJob(const CodePtr& context, const core::JobExprPtr& job) {

	// resolve job information
	JobInfo info = resolveJob(job);

	// add dependencies
	context->addDependency(info.structDefinition);
	context->addDependency(info.jobFunction);

	// construct job creation statement
	CodeStream& cStr = context->getCodeStream();

	NodeManager& manager = job->getNodeManager();
	TypePtr uintType = manager.basic.getUInt8();
	ExpressionPtr min = Literal::get(manager, "1", uintType);
	ExpressionPtr max = Literal::get(manager, "1<<20", uintType);

	// evaluate range - primitive variant
	ExpressionPtr range = job->getThreadNumRange();
	if (analysis::isCallOf(range, manager.basic.getCreateMinRange())) {
		CallExprPtr call = static_pointer_cast<const CallExpr>(range);
		min = call->getArguments()[0];
	} else if (analysis::isCallOf(range, manager.basic.getCreateBoundRange())) {
		CallExprPtr call = static_pointer_cast<const CallExpr>(range);
		min = call->getArguments()[0];
		max = call->getArguments()[1];
	} else {
		assert("Unsupported range specification discovered!");
	}

	StmtConverter& converter = cc.getStmtConverter();

	cStr << "memcpy(malloc(sizeof(" << info.structName << ")),";

	cStr << "&((" << info.structName << "){";

		cStr << "sizeof(" << info.structName << "),";
		converter.convert(min);
		cStr << ",";
		converter.convert(max);
		cStr << ",";
		cStr << "&" << info.funName;


		// local shared variables
		for_each(job->getLocalDecls(), [&](const core::DeclarationStmtPtr& cur) {
			ExpressionPtr init = cur->getInitialization();
			cStr << ",";
			converter.convert(init);
		});

	cStr << "}),";

	cStr << "sizeof(" << info.structName << ")";

	cStr << ")";
}


JobManager::JobInfo JobManager::resolveJob(const core::JobExprPtr& job) {

	// try lookup ..
	auto pos = jobs.find(job);
	if (pos != jobs.end()) {
		return pos->second;
	}

	// resolve job

	// To be created:
	//   - the struct for representing the current Job 		 => struct fragment
	//   - the function actually processing the current job  => function fragment


	// Prepare some utilities
	NodeManager& manager = job->getNodeManager();
	ASTBuilder builder(manager);

	// get a name for the job
	string name = cc.getNameManager().getName(job);
	string structName = "struct " + name;
	string funName = "fun" + name;

	// Step a) create job struct
	CodePtr jobStruct = std::make_shared<CodeFragment>("struct for job " + name);
	CodeStream& structStream = jobStruct->getCodeStream();

	structStream << structName << " { " << CodeStream::indR << "\n";
		structStream << "unsigned structSize;\n";
		structStream << "unsigned min, max;\n";
		structStream << "void (*fun)(isbr_JobArgs*);";

		Converter& converter = cc;
		for_each(job->getLocalDecls(), [&](const core::DeclarationStmtPtr& cur) {
			VariablePtr var = cur->getVariable();
			string varName = converter.getNameManager().getName(var);
			structStream << "\n";
			structStream <<  converter.getTypeManager().formatParamter(jobStruct, var->getType(), varName, false) << ";";
		});

	structStream << CodeStream::indL << "\n";
	structStream << "};\n";

	// Step b) create job function
	CodePtr function = std::make_shared<CodeFragment>("function for job " + name);
	CodeStream& funStream = function->getCodeStream();
	funStream << "void fun" << name << "(isbr_JobArgs* args)" << CodeStream::indR << " {\n";

		funStream << "// ----------- Unpacking local scope variables ----------\n";

		for_each(job->getLocalDecls(), [&](const core::DeclarationStmtPtr& cur) {

			VariablePtr var = cur->getVariable();

			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			converter.getVariableManager().addInfo(var, info);

			string varName = converter.getNameManager().getName(var);
			funStream << converter.getTypeManager().formatParamter(function, var->getType(), varName, false);
			funStream << " = ((" << structName << "*)args)->" << varName << ";\n";

		});

		funStream << "// ------------------ Processing Guards -----------------\n";

		StmtConverter& stmtConverter = converter.getStmtConverter();
		for_each(job->getGuardedStmts(), [&](const core::JobExpr::GuardedStmt& cur) {

			// add condition
			funStream << "if(" << ") {" << CodeStream::indR << "\n";
			stmtConverter.convert(builder.callExpr(preprocessJobBranch(cur.second)), function);
			funStream << ";\nreturn;" << CodeStream::indL << "}";

		});

		funStream << "// ------------------ Default processing -----------------\n";

		stmtConverter.convert(builder.callExpr(preprocessJobBranch(job->getDefaultStmt())), function);
		funStream << ";";


	funStream << CodeStream::indL << "\n";
	funStream << "}\n";



	// Assemble result
	JobInfo info(structName, funName, jobStruct, function);
	jobs.insert(std::make_pair(job, info));
	return info;

	//// check if local decls exist, if so generate struct to hold them and populate it
	//auto localDecls = ptr->getLocalDecls();
	//if(localDecls.size() > 0) {
	//	string structName = nameGen.getName(ptr, "jobLocalDecls");
	//	string structVarName = structName + "__var";
	//	CodePtr structCode = defCodePtr->addDependency(structName);
	//	CodeStream& sCStr = structCode->getCodeStream();
	//	// definition
	//	sCStr << "struct " << structName << CodeStream::indR << " {\n";
	//	// variable declaration
	//	cStr << "struct " << structName << "* " << structVarName << " = new " << structName << ";";
	//	for_each(localDecls, [&](const DeclarationStmtPtr& cur) {
	//		auto varExp = cur->getVarExpression();
	//		// generate definition
	//		sCStr << this->printTypeName(varExp->getType()) << " " << varExp->getIdentifier().getName() << ";";
	//		// populate entry
	//		cStr << structVarName << "." << varExp->getIdentifier().getName() << " = ";
	//		this->visit(cur->getInitialization());
	//		cStr << ";";
	//	});
	//	sCStr << CodeStream::indL << "};";
	//	// TODO finish job generation (when runtime lib available)
	//}

}


} // end namespace simple_backend
} // end namespace insieme
