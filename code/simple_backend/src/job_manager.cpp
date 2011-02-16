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
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/iterator_utils.h"

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
	ExpressionPtr max = Literal::get(manager, "isbr_getMaxThreads()", uintType);

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

		// auto-captured variables
		for_each(info.capturedVars, [&](const core::VariablePtr& var) {
			cStr << ",";
			converter.convert(var);
		});

	cStr << "}),";

	cStr << "sizeof(" << info.structName << ")";

	cStr << ")";
}


void JobManager::createPFor(const CodePtr& context, const core::ExpressionPtr& call) {

	// check format
	assert(analysis::isCallOf(call, cc.getLangBasic().getPFor()));

	CodeStream& out = context->getCodeStream();
	StmtConverter& stmtConverter = cc.getStmtConverter();

	auto args = static_pointer_cast<const core::CallExpr>(call)->getArguments();

	// resolve body
	const PForBodyInfo& info = resolvePForBody(args[4]);

	// add dependencies
	context->addDependency(info.funDefinition);
	context->addDependency(info.captureStruct);

	// add call
	// void isbr_pfor(isbr_ThreadGroup group, isbr_PForRange range, void (*fun)(isbr_PForRange range));
	out << "isbr_pfor(";
	stmtConverter.convert(args[0]);
	out << ",(isbr_PForRange){";

	stmtConverter.convert(args[1]);
	out << ", ";
	stmtConverter.convert(args[2]);
	out << ", ";
	stmtConverter.convert(args[3]);
	out << ", ";

	unsigned numVars = info.capturedVars.size();
	if (numVars > 0) {

		out << "&((" << info.captureStructName << "){";

		for (unsigned i=0; i<numVars; i++) {
			stmtConverter.convert(info.capturedVars[i]);
			if (i< numVars-1) {
				out << ", ";
			}
		}

		out << "})";
	} else {
		out << "0";
	}

	out << "},";
	out << "&" << info.functionName;
	out << ")";

}


namespace {

	/**
	 * A small helper-visitor collecting all variables which should be automatically
	 * captured by jobs for their branches.
	 */
	class VariableCollector : public core::ASTVisitor<> {

		/**
		 * A reference to the resulting list of variables.
		 */
		vector<core::VariablePtr>& list;

	public:

		/**
		 * Creates a new instance of this visitor based on the given list of variables.
		 * @param list the list to be filled by this collector.
		 */
		VariableCollector(vector<core::VariablePtr>& list) : core::ASTVisitor<>(false), list(list) {}

	protected:

		/**
		 * Visits a variable and adds the variable to the resulting list (without duplicates).
		 * It also terminates the recursive decent.
		 */
		void visitVariable(const core::VariablePtr& var) {
			// collect this variable
			if (!contains(list, var)) {
				list.push_back(var);
			}
		}

		/**
		 * Visiting a lambda expression terminates the recursive decent since a new scope
		 * is started.
		 */
		void visitLambdaExpr(const core::LambdaExprPtr& lambda) {
			// break recursive decent when new scope is started
		}

		/**
		 * Types are generally ignored by this visitor for performance reasons (no variables will
		 * occur within types).
		 */
		void visitType(const core::TypePtr& type) {
			// just ignore types
		}

		/**
		 * The default behavior for all other node types is to recursively decent by iterating
		 * through the child-node list.
		 */
		void visitNode(const core::NodePtr& node) {
			assert(node->getNodeType() != core::NT_LambdaExpr);
			// visit all children recursively
			for_each(node->getChildList(), [this](const NodePtr& cur){
				this->visit(cur);
			});
		}

	};


	/**
	 * Collects a list of variables to be captures by a job for proper initialization
	 * of the various job branches.
	 */
	vector<core::VariablePtr> getVariablesToBeCaptured(const core::ExpressionPtr& job) {
		vector<core::VariablePtr> res;

		// collect all variables potentially captured by this job
		VariableCollector collector(res);
		collector.visit(job);

		return res;
	}

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

	// collect list of captured variables
	vector<core::VariablePtr> varList = getVariablesToBeCaptured(job);
	utils::map::PointerMap<core::NodePtr, core::NodePtr> mapping;
	for_each(varList, [&](const core::VariablePtr& var) {
		mapping.insert(std::make_pair(var, builder.variable(var->getType())));
	});

	// replace the variables
	core::JobExprPtr targetJob = static_pointer_cast<const core::JobExpr>(core::transform::replaceAll(manager, job, mapping, true));


	// Step a) create job struct
	CodePtr jobStruct = std::make_shared<CodeFragment>("struct for job " + name);
	CodeStream& structStream = jobStruct->getCodeStream();

	structStream << structName << " { " << CodeStream::indR << "\n";
		structStream << "unsigned structSize;\n";
		structStream << "unsigned min, max;\n";
		structStream << "void (*fun)(isbr_JobArgs*);";

		Converter& converter = cc;
		for_each(targetJob->getLocalDecls(), [&](const core::DeclarationStmtPtr& cur) {
			VariablePtr var = cur->getVariable();
			string varName = converter.getNameManager().getName(var);
			structStream << "\n";
			structStream <<  converter.getTypeManager().formatParamter(jobStruct, var->getType(), varName, false) << ";";
		});

		structStream << "\n// ---- additional captured variables -----";
		for_each(varList, [&](const core::VariablePtr& cur) {

			core::VariablePtr var = static_pointer_cast<const core::Variable>(mapping.find(cur)->second);
			string varName = converter.getNameManager().getName(var);
			structStream << "\n";
			structStream <<  converter.getTypeManager().formatParamter(jobStruct, var->getType(), varName, false) << ";";
			structStream << "\t // Variable: " << toString(*var);
		});

	structStream << CodeStream::indL << "\n";
	structStream << "};\n";

	// Step b) create job function
	CodePtr function = std::make_shared<CodeFragment>("function for job " + name);
	CodeStream& funStream = function->getCodeStream();
	funStream << "void fun" << name << "(isbr_JobArgs* args)" << CodeStream::indR << " {\n";

		funStream << "// ----------- Unpacking local scope variables ----------\n";

		for_each(targetJob->getLocalDecls(), [&](const core::DeclarationStmtPtr& cur) {

			VariablePtr var = cur->getVariable();

			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			converter.getVariableManager().addInfo(var, info);

			string varName = converter.getNameManager().getName(var);
			funStream << converter.getTypeManager().formatParamter(function, var->getType(), varName, false);
			funStream << " = ((" << structName << "*)args)->" << varName << ";\n";

		});

		funStream << "// ---------- Unpacking auto-captured variables ---------\n";
		for_each(varList, [&](const core::VariablePtr& cur) {

			core::VariablePtr var = static_pointer_cast<const core::Variable>(mapping.find(cur)->second);
			string varName = converter.getNameManager().getName(var);

			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			converter.getVariableManager().addInfo(var, info);

			funStream << converter.getTypeManager().formatParamter(function, var->getType(), varName, false);
			funStream << " = ((" << structName << "*)args)->" << varName << ";\n";
		});


		funStream << "// ------------------ Processing Guards -----------------\n";

		StmtConverter& stmtConverter = converter.getStmtConverter();
		for_each(targetJob->getGuardedStmts(), [&](const core::JobExpr::GuardedStmt& cur) {

			// add condition
			funStream << "if(" << ") {" << CodeStream::indR << "\n";
			stmtConverter.convert(builder.callExpr(preprocessJobBranch(cur.second)), function);
			funStream << ";\nreturn;" << CodeStream::indL << "}";

		});

		funStream << "// ------------------ Default processing -----------------\n";

		stmtConverter.convert(builder.callExpr(preprocessJobBranch(targetJob->getDefaultStmt())), function);
		funStream << ";";


	funStream << CodeStream::indL << "\n";
	funStream << "}\n";

	// Assemble result
	JobInfo info(structName, funName, jobStruct, function, varList);
	jobs.insert(std::make_pair(job, info));
	return info;

}


namespace {

	/**
	 * Prepares the loop body of a pfor loop for being processed. If possible, the loop body will be in-lined.
	 */
	core::StatementPtr prepareLoopBody(ASTBuilder& builder, const core::ExpressionPtr& body, const core::VariablePtr& inductionVar) {

		// verify type of loop body
		assert(body->getType()->getNodeType() == NT_FunctionType);

		// to be inlined:
		//		- a simple function
		//		- a capture-init/function combination based on variables only

		// handle simple function
		if (body->getNodeType() == NT_LambdaExpr) {
			LambdaExprPtr lambda = static_pointer_cast<const LambdaExpr>(body);
			if (!lambda->isRecursive()) {

				// get body and replace parameter with induction variable
				const VariablePtr& parameter = lambda->getParameterList()[0];
				const NodePtr inlined = transform::replaceAll(builder.getNodeManager(), lambda->getBody(), parameter, inductionVar);
				return static_pointer_cast<const Statement>(inlined);
			}

		} else if (body->getNodeType() == NT_CaptureInitExpr) {

			CaptureInitExprPtr capture = static_pointer_cast<const CaptureInitExpr>(body);
			if (capture->getLambda()->getNodeType() == NT_LambdaExpr) {

				LambdaExprPtr lambda = static_pointer_cast<const LambdaExpr>(capture->getLambda());
				if (!lambda->isRecursive()) {

					// collect initialization variables
					utils::map::PointerMap<VariablePtr, VariablePtr> varMap;

					// add function parameter (there is only one of those)
					varMap.insert(std::make_pair(lambda->getParameterList()[0], inductionVar));

					const CaptureInitExpr::Values& values = capture->getValues();
					const Lambda::CaptureList& params = lambda->getCaptureList();

					bool valid = values.size() == params.size();
					// add values - param mapping
					for (unsigned i=0; valid && i<values.size(); i++) {

						// test value => has to be a variable
						valid = valid && values[i]->getNodeType() == NT_Variable;
						if (valid) {
							varMap.insert(std::make_pair(params[i], static_pointer_cast<const Variable>(values[i])));
						}
					}

					// replace all parameters by their value and return result
					if (valid) {
						const NodePtr inlined = transform::replaceVars(builder.getNodeManager(), lambda->getBody(), varMap);
						return static_pointer_cast<const Statement>(inlined);
					}
				}
			}

		}

		// handle capture initialization nodes + function


		// default handling => just call loop body function
		return builder.callExpr(body, inductionVar);
	}
}


JobManager::PForBodyInfo JobManager::resolvePForBody(const core::ExpressionPtr& body) {

	// try lookup ..
	auto pos = bodies.find(body);
	if (pos != bodies.end()) {
		return pos->second;
	}

	// some general assertions
	assert(body->getType()->getNodeType() == NT_FunctionType);

	// obtain a name for this body function
	string name = cc.getNameManager().getName(body);
	string structName = "struct capture" + name;

	// collect variables to be captured
	vector<core::VariablePtr> varList = getVariablesToBeCaptured(body);

	// create the capture struct
	CodePtr captureStruct = std::make_shared<CodeFragment>("capture-struct for pfor-body " + name);
	CodeStream& captureStream = captureStruct->getCodeStream();

	captureStream << structName << " {" << CodeStream::indR;

		for_each(varList, [&](const core::VariablePtr& var) {
			captureStream << "\n";
			string varName = cc.getNameManager().getName(var);
			captureStream << cc.getTypeManager().formatParamter(captureStruct, var->getType(), varName, false);
			captureStream << ";";
		});

	captureStream << CodeStream::indL << "\n";
	captureStream << "};\n";

	// create the function
	CodePtr function = std::make_shared<CodeFragment>("function for pfor-body " + name);
	function->addDependency(captureStruct);
	CodeStream& funStream = function->getCodeStream();

	ASTBuilder builder(body->getNodeManager());

	// create function realizing the pfor-loop body capable of iterating over a range
	funStream << "void " << name << "(const isbr_PForRange range) {" << CodeStream::indR << "\n";

		// extract captured variables
		NodeManager& manager = body->getNodeManager();
		VariableManager& varManager = cc.getVariableManager();
		utils::map::PointerMap<core::VariablePtr, core::VariablePtr> captureMap;
		funStream << "// ----- captured variables -----\n";
		for_each(varList, [&](const core::VariablePtr& var) {

			// get local variable
			core::VariablePtr localVar = builder.variable(var->getType());
			captureMap.insert(std::make_pair(var, localVar));

			// register location info for new variable
			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			varManager.addInfo(localVar, info);


			string varName = cc.getNameManager().getName(var);
			string localName = cc.getNameManager().getName(localVar);
			funStream << cc.getTypeManager().formatParamter(function, var->getType(), localName, false);
			funStream << " = ((" << structName << "*)(range.context))->" << varName << ";\n";
		});


		// replace captured variables within body expression
		ExpressionPtr loopBody = static_pointer_cast<const core::Expression>(transform::replaceVars(manager, body, captureMap));

		// add for-loop
		VariablePtr inductionVar = builder.variable(static_pointer_cast<const FunctionType>(loopBody->getType())->getArgumentTypes()[0]);
		cc.getNameManager().setName(inductionVar, "__it");

		funStream << "\n// ----- process iterations -----\n";
		funStream << "for(";
		funStream << cc.getTypeManager().getTypeName(function, inductionVar->getType(), true);
		funStream << " __it = range.start; __it<range.end; __it+=range.step) {" << CodeStream::indR << "\n";

			// add loop body
			cc.getStmtConverter().convert(prepareLoopBody(builder, loopBody, inductionVar), function);
			funStream << ";";

		funStream << CodeStream::indL << "\n";
		funStream << "}\n";

	funStream << CodeStream::indL << "\n";
	funStream << "}\n";


	// Assemble result
	PForBodyInfo info(name, structName, function, captureStruct, varList);
	bodies.insert(std::make_pair(body, info));
	return info;

}


} // end namespace simple_backend
} // end namespace insieme
