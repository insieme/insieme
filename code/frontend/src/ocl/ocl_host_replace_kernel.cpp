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
#include <fstream>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/ocl/ocl_host_replace_kernel.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"
#include "insieme/frontend/extensions/ocl_kernel_extension.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
bool KernelCodeRetriver::saveString(const core::LiteralPtr& lit) {
	path = lit->getStringValue();
	return true;
}

bool KernelCodeRetriver::saveString(const core::CallExprPtr& call) {
	if (const LiteralPtr lit = dynamic_pointer_cast<const Literal>(call->getFunctionExpr())) {
		if (gen.isRefVectorToRefArray(lit)) {
			if (const LiteralPtr pl = dynamic_pointer_cast<const Literal>(utils::tryRemove(gen.getRefDeref(), call->getArgument(0)))) {
				path = pl->getStringValue();
				return true;
			}
		}

		if(lit->getStringValue() == "shrFindFilePath") {

			const ExpressionPtr arg = utils::tryRemove(gen.getRefDeref(), call->getArgument(0));
			if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(arg))
				return saveString(call);
			if(const LiteralPtr lit = dynamic_pointer_cast<const Literal>(arg))
				return saveString(lit);
			if(const VariablePtr var = dynamic_pointer_cast<const Variable>(arg)){
				KernelCodeRetriver nkcr(var, call, program);
				visitDepthFirstInterruptible(program, nkcr);
				string pathFound = nkcr.getKernelFilePath();
				if(pathFound.size() > 0) {
					path = pathFound;
					return true;
				}
			}
		}
	}
	return false;
}

bool KernelCodeRetriver::visitNode(const core::NodePtr& node) {
	if (node == breakingStmt) {
		return true; // stop recursion
	}
	return false; // go on with search
}

bool KernelCodeRetriver::visitCallExpr(const core::CallExprPtr& callExpr) {
	if (callExpr->getFunctionExpr() != gen.getRefAssign())
		return false;
	// check if it is the variable we are looking for
	if (const VariablePtr pathVar = dynamic_pointer_cast<const Variable>(pathToKernelFile)) {
		if (const VariablePtr lhs = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0))) {
			if (lhs->getId() != pathVar->getId())
				return false;
		}

		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
			return saveString(rhs);
		}
	}

	if(callExpr->getArgument(0) == pathToKernelFile) {
		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
			return saveString(rhs);
		}

	}

	return false;
}

bool KernelCodeRetriver::visitDeclarationStmt(const core::DeclarationStmtPtr& decl) {
	if (const VariablePtr& pathVar = dynamic_pointer_cast<const Variable>(pathToKernelFile)) {
		if (decl->getVariable()->getId() != pathVar->getId())
			return false;

		if (const CallExprPtr initCall = dynamic_pointer_cast<const CallExpr>(utils::tryRemoveAlloc(decl->getInitialization()))) {
			return saveString(initCall);
		}
	}
	return false;
}

const ProgramPtr loadKernelsFromFile(string path, const IRBuilder& builder, const std::vector<boost::filesystem::path>& includeDirs) {
	// delete quotation marks form path
	if (path[0] == '"')
		path = path.substr(1, path.length() - 2);
std::cout << "Path: " << path << std::endl;
	std::ifstream check;
			string root = path;
	size_t nIncludes = includeDirs.size();
	// try relative path first
	check.open(path);
	// if not found now, check the include directories
	for (size_t i = 0; i < nIncludes && check.fail(); ++i) {
		check.close();
		// try with include paths
		path = (includeDirs.at(i) / root).string();
		check.open(path);
	}
	// if there is still no match, try the paths of the input files
/*	no job, no current file :/
	if (check.fail()) {
		assert(job.getFiles().size() == 1u);
		string ifp = job.getFiles()[0].string();
		size_t slash = ifp.find_last_of("/");
		path = ifp.substr(0u, slash + 1) + root;
		check.open(path);
	}
*/
	check.close();

	if (check.fail()) {// no kernel file found, leave the error printing to the compiler frontend
	//		std::cerr << "FAIL! " << path << std::endl;
		path = root;
	}

	LOG(INFO) << "Converting kernel file '" << path << "' to IR...";
	ConversionJob kernelJob(path, includeDirs);
	kernelJob.registerFrontendPlugin<extensions::OclKernelPlugin>();
//	kernelJob.setFiles(toVector<frontend::path>(path));
	return kernelJob.execute(builder.getNodeManager(), false);
}

}

KernelReplacer::KernelReplacer(core::NodePtr prog, const std::vector<boost::filesystem::path>& includeDirs) : prog(prog), includeDirs(includeDirs){
	findKernelNames();
	collectArguments();
	replaceKernels();
	loadKernelCode();

//	std::cout << printer::PrettyPrinter(this->prog) << std::endl;
}

void KernelReplacer::findKernelNames() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	NodeAddress pA(prog);

	TreePatternPtr nameLiteral = pattern::var("kernel_name", pattern::any);
	TreePatternPtr mayEncapsulatedNameLiteral = irp::callExpr(pattern::any, pattern::any, *pattern::any << nameLiteral << *pattern::any) | nameLiteral;
	TreePatternPtr clCreateKernel = irp::callExpr(pattern::any, irp::literal("clCreateKernel"),	pattern::any <<
			mayEncapsulatedNameLiteral << pattern::var("err", pattern::any) );
	TreePatternPtr createKernelPattern = pattern::var("clCreateKernel", irp::callExpr(pattern::any, irp::atom(mgr.getLangBasic().getRefAssign()),
			pattern::var("kernel", pattern::any) << clCreateKernel));

	irp::matchAllPairs(createKernelPattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createKernel) {
//std::cout << "kernel: " << *createKernel["clCreateKernel"].getValue() << std::endl;

		std::string kernelName = createKernel["kernel_name"].getValue().as<LiteralPtr>()->getStringValue();
		ExpressionAddress kernelExpr = matchAddress >> createKernel["kernel"].getValue().as<ExpressionAddress>();
		ExpressionAddress kernelVar = utils::extractVariable(kernelExpr).as<ExpressionAddress>();

		kernelVar = utils::getRootVariable(kernelVar).as<ExpressionAddress>();

//std::cout << "varAddr: " << kernelVar << " - " << *kernelVar << std::endl;
		kernelNames[kernelName] = kernelVar;

		// remove the clCreateKernel call including the assignment and its lhs
		prog = transform::replaceAll(mgr, prog, NodePtr(createKernel["clCreateKernel"].getValue()), builder.getNoOp(), false);
	});
}


void KernelReplacer::collectArguments() {
	NodeManager& mgr = prog->getNodeManager();
	const lang::BasicGenerator& gen = mgr.getLangBasic();
	IRBuilder builder(mgr);

	NodeAddress pA(prog);
	NodeMap replacements;

	TreePatternPtr localOrGlobalVar = node(node(node(single(aT(irp::genericType("_cl_kernel"))) << single(pattern::any))
			<< single(pattern::any)) << single(pattern::any));

	TreePatternPtr clSetKernelArg = var("clSetKernelArg", irp::callExpr(pattern::any, irp::literal("clSetKernelArg"),
			(aT(pattern::var("kernel", localOrGlobalVar))) << pattern::var("arg_index", irp::literal())
			<< pattern::var("arg_size", pattern::any) << pattern::var("arg_value", pattern::any) ));


	irp::matchAllPairs(clSetKernelArg, pA, [&](const NodeAddress& matchAddress, const AddressMatch& setArg) {
//		std::cout << "Kernel: " << setArg["kernel"].getValue() << "\n\tidx: " << *setArg["arg_index"].getValue() << " val "
//				<< *getVarOutOfCrazyInspireConstruct1(setArg["arg_value"].getValue().as<ExpressionPtr>(), IRBuilder(mgr)) << std::endl;

		ExpressionAddress kernel = utils::getRootVariable(matchAddress >> setArg["kernel"].getValue().as<ExpressionAddress>()).as<ExpressionAddress>();
		LiteralPtr idx = setArg["arg_index"].getValue().as<LiteralPtr>();
		ExpressionPtr arg = setArg["arg_value"].getValue().as<ExpressionPtr>();
		ExpressionPtr sizeArg = setArg["arg_size"].getValue().as<ExpressionPtr>();

		int argIdx = insieme::utils::numeric_cast<int>(idx->getStringValue());

		// reserve memory
		for(int i = kernelTypes[kernel].size(); i <= argIdx; ++i)
			kernelTypes[kernel].push_back(TypePtr());

		VariablePtr tuple = builder.variable(kernel->getType());

		VariablePtr src = builder.variable(arg->getType());
		VariableList params;
		params.push_back(tuple);
		StatementList body;
		ExpressionPtr idxArg = (gen.isUInt8(idx->getType()) ? idx.as<ExpressionPtr>() :	builder.castExpr(gen.getUInt8(), idx));

		if(utils::isNullPtr(arg)) {
			// in this case arg is a local variable which has to be declared in host code
			// need to read size parameter
			ExpressionPtr size;
			TypePtr type;
			ExpressionPtr hostPtr;
			utils::extractSizeFromSizeof(sizeArg, size, type);
			assert(size && "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.");

			// add ref<array< >> to type
			type = builder.refType(builder.arrayType(type));

			//collect types of the arguments
			kernelTypes[kernel].at(argIdx) = (type);


			// refresh variables used in the generation of the local variable
			VariableMap varMapping;
			utils::refreshVariables(size, varMapping, builder);

			ExpressionPtr tupleAccess = builder.callExpr(gen.getTupleRefElem(), tuple, idxArg, builder.getTypeLiteral(type));
			ExpressionPtr allocMemory = builder.refVar(builder.callExpr(type, gen.getArrayCreate1D(), builder.getTypeLiteral(type), size));

			body.push_back(builder.callExpr(gen.getUnit(), gen.getRefAssign(), tupleAccess, allocMemory));
			body.push_back(builder.returnStmt(builder.intLit(0)));

			//ßßß		kernelArgs[kernel].push_back(builder.getTypeLiteral((builder.refType(type))));

			if(varMapping.empty()) { // no variable from outside needed
				FunctionTypePtr fTy = builder.functionType(kernel->getType(), gen.getInt4());
				LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));
				replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, kernel);
				return;
			}

			TypeList argTys;
			ExpressionList args;
			args.push_back(kernel);
			argTys.push_back(kernel->getType());
			// add the needed variables
			for_each(varMapping, [&](std::pair<VariablePtr, VariablePtr> varMap) {
				args.push_back(varMap.first);
				argTys.push_back(varMap.first->getType());
				params.push_back(varMap.second);
			});

			FunctionTypePtr fTy = builder.functionType(argTys, gen.getInt4());
			LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));
			replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, args);


			// initialize local memory place with undefined
	/*		return builder.callExpr(gen.getUnit(), gen.getRefAssign(), builder.callExpr(builder.refType(type), gen.getTupleRefElem(), kernel,
					(gen.isUInt8(idx) ? idx :	builder.castExpr(gen.getUInt8(), idx)),
					builder.getTypeLiteral(type)), builder.refVar(builder.callExpr(type, gen.getArrayCreate1D(), builder.getTypeLiteral(type), size)));
	*/
			return;
		}

//	std::cout << "ARGUMENT: \t" << *arg->getType() << "  " << *arg << std::endl;
		//collect types of the arguments
		kernelTypes[kernel].at(argIdx) = (arg->getType());

//		arg = utils::getVarOutOfCrazyInspireConstruct(arg);

	//	kernelArgs[kernel] = builder.variable(builder.tupleType(argTypes));
	//ßßß	kernelArgs[kernel].push_back(arg);

		FunctionTypePtr fTy = builder.functionType(toVector(kernel->getType().as<TypePtr>(), utils::tryDeref(arg)->getType()), gen.getInt4());
		params.push_back(src);

		body.push_back(builder.assign( builder.callExpr(gen.getTupleRefElem(), tuple, idxArg,
										builder.getTypeLiteral(src->getType())), src));
		body.push_back(builder.returnStmt(builder.intLit(0)));
		LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));

		// store argument in a tuple
		replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, kernel, utils::tryDeref(arg));

/*
for_each(kernelTypes[kernel], [](NodePtr doll) {
	std::cout << "new arg: " << (doll) << std::endl;
});
*/
	});

	// perform the replacements
	prog = transform::replaceAll(mgr, prog, replacements, false);
}

void KernelReplacer::replaceKernels() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	// generate tuple variables for each kernel and replace kernel variables
	ExpressionMap kernelReplacements;

	for_each(kernelTypes, [&](std::pair<ExpressionPtr, std::vector<TypePtr> > kT) {
		ExpressionPtr k = kT.first;
		TupleTypePtr tt = builder.tupleType(kT.second);
		ExpressionPtr replacement = k.isa<VariablePtr>() ? builder.variable(tt).as<ExpressionPtr>()
				: builder.literal(tt, kT.first.as<LiteralPtr>()->getStringValue());
		kernelReplacements[kT.first] = replacement;
	});

	prog = transform::replaceVarsRecursiveGen(mgr, prog, kernelReplacements);
}

void KernelReplacer::loadKernelCode() {
	NodeManager& mgr = prog->getNodeManager();
	NodeAddress pA(prog);

	TreePatternPtr clCreateProgramWithSource = var("cpws", irp::callExpr(pattern::any, irp::literal("clCreateProgramWithSource"),	pattern::any <<
			pattern::any << var("kernelCodeString", pattern::any) << pattern::any << pattern::var("err", pattern::any) ));
	TreePatternPtr kernelAssign = irp::callExpr(irp::atom(mgr.getLangBasic().getRefAssign()),
			var("kernelVar", pattern::any), (clCreateProgramWithSource));
	TreePatternPtr kernelDecl = irp::declarationStmt(aT(var("kernelVar", pattern::any)) ,aT(clCreateProgramWithSource));
	TreePatternPtr clCreateProgramWithSourcePattern = kernelAssign | kernelDecl;

	irp::matchAllPairs(clCreateProgramWithSourcePattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createKernel) {

		lookForKernelFilePragma(createKernel["kernelVar"].getValue().as<ExpressionPtr>()->getType(), createKernel["cpws"].getValue().as<ExpressionPtr>());
	});
}

ProgramPtr KernelReplacer::findKernelsUsingPathString(const ExpressionPtr& path, const ExpressionPtr& root, const ProgramPtr& mProgram) {
	std::set<string> kernelFileCache;
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	lang::BasicGenerator gen(mgr);
	ConversionJob job;
	ProgramPtr kernels;

	if(const CallExprPtr callSaC = dynamic_pointer_cast<const CallExpr>(path)) {
		if(const LiteralPtr stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
			if(gen.isRefVectorToRefArray(stringAsChar)) {
				if(const LiteralPtr path = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
					// check if file has already been added
//std::cout << "\n using path string " << path->getStringValue() << " \n\n";
					if(kernelFileCache.find(path->getStringValue()) == kernelFileCache.end()) {
						kernelFileCache.insert(path->getStringValue());
						kernels = loadKernelsFromFile(path->getStringValue(), builder, includeDirs);
					}
					//return;
// set source string to an empty char array
//					ret = builder.refVar(builder.literal("", builder.arrayType(gen.getChar())));
				}
			}
		}
	}

	string kernelFilePath;
	if(const VariablePtr pathVar = dynamic_pointer_cast<const Variable>(utils::tryRemove(gen.getRefDeref(), path))) {
		KernelCodeRetriver kcr(pathVar, root, mProgram);
		visitDepthFirst(mProgram, kcr);
		kernelFilePath = kcr.getKernelFilePath();
	}

	if(const CallExprPtr pathCall = dynamic_pointer_cast<const CallExpr>(utils::tryRemove(gen.getRefDeref(), path))) {
		if(gen.isCompositeRefElem(pathCall->getFunctionExpr())) {
			KernelCodeRetriver kcr(pathCall, root, mProgram);
			visitDepthFirst(mProgram, kcr);
			kernelFilePath = kcr.getKernelFilePath();
		}
	}
	if(kernelFilePath.size() > 0) {
		// check if file has already been added
		if(kernelFileCache.find(kernelFilePath) == kernelFileCache.end()) {
			kernelFileCache.insert(kernelFilePath);
			kernels = loadKernelsFromFile(kernelFilePath, builder, includeDirs);
		}
	}

	return kernels;
}

std::vector<ExpressionPtr> KernelReplacer::lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	lang::BasicGenerator gen(mgr);
	ConversionJob job;
	// set to store paths of loaded kernel files

	std::vector<ExpressionPtr> kernelEntries;

	if(type == builder.refType(builder.refType(builder.arrayType((builder.genericType("_cl_program")))))) {
		if(CallExprPtr cpwsCall = dynamic_pointer_cast<const CallExpr>(utils::tryRemoveAlloc(createProgramWithSource))) {
			if(insieme::annotations::ocl::KernelFileAnnotationPtr kfa = dynamic_pointer_cast<insieme::annotations::ocl::KernelFileAnnotation>
					(createProgramWithSource->getAnnotation(insieme::annotations::ocl::KernelFileAnnotation::KEY))) {
				const string& path = kfa->getKernelPath();
				if(cpwsCall->getFunctionExpr() == gen.getRefDeref() && cpwsCall->getArgument(0)->getNodeType() == NT_CallExpr)
					cpwsCall = dynamic_pointer_cast<const CallExpr>(cpwsCall->getArgument(0));
				if(const LiteralPtr clCPWS = dynamic_pointer_cast<const Literal>(cpwsCall->getFunctionExpr())) {
					if(clCPWS->getStringValue() == "clCreateProgramWithSource") {
						// check if file has already been added
						if(kernelFileCache.find(path) == kernelFileCache.end()) {
							kernelFileCache.insert(path);
							const ProgramPtr kernels = loadKernelsFromFile(path, builder, includeDirs);
							for_each(kernels->getEntryPoints(), [&](ExpressionPtr kernel) {
									kernelEntries.push_back(kernel);
							});
						}
					}
				}
			}
		}
	}
	return kernelEntries;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme

