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

#include "insieme/utils/map_utils.h"

#include "insieme/core/ir_node.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/utils/member_access_literal_updater.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/ocl/ocl_host_compiler.h"
#include "insieme/frontend/ocl/ocl_host_1st_pass.h"
#include "insieme/frontend/ocl/ocl_host_2nd_pass.h"
#include "insieme/frontend/ocl/ocl_host_3rd_pass.h"

namespace ba = boost::algorithm;

//#include "insieme/core/ir_visitor.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/c/naming.h"

namespace insieme {
namespace frontend {
namespace ocl {
using namespace insieme::core;

/*
class fuVisitor: public core::IRVisitor<void> {
	void visitNode(const NodePtr& node) {
		if(insieme::annotations::ocl::KernelFileAnnotationPtr kfa =
				dynamic_pointer_cast<insieme::annotations::ocl::KernelFileAnnotation>(node->getAnnotation(insieme::annotations::ocl::KernelFileAnnotation::KEY))) {
			std::cout << "Found kernel file Pragma at node \n" << node << std::endl;
		}
	}
	void visitCallExpr(const CallExprPtr& call) {
		if(call->getType()->toString().find("type") != string::npos){
			std::cout << " -> " << call->getType() << " " << call << std::endl;
		}
	}
public:
	fuVisitor(): IRVisitor<void>(true) {}
};*/

ProgramPtr HostCompiler::compile() {
//	fuVisitor FAKK;
//	visitDepthFirst(transformedProg,FAKK);

	//    HostVisitor oclHostVisitor(builder, mProgram);
	HostMapper oclHostMapper(builder, mProgram);

	const ProgramPtr& interProg = dynamic_pointer_cast<const core::Program>(oclHostMapper.mapElement(0, mProgram));
	assert(interProg && "First pass of OclHostCompiler corrupted the program");

	if(oclHostMapper.getnKernels() == 0) {
		LOG(INFO) << "No OpenCL kernel functions found";
//		return mProgram;
	}
	LOG(INFO) << "Adding " << oclHostMapper.getnKernels() << " OpenCL kernels to host Program... ";

	const vector<ExpressionPtr>& kernelEntries = oclHostMapper.getKernels();

	const ProgramPtr& progWithEntries = core::Program::addEntryPoints(builder.getNodeManager(), interProg, kernelEntries);
	const ProgramPtr& progWithKernels = core::Program::remEntryPoints(builder.getNodeManager(), progWithEntries, kernelEntries);
	KernelArgs kernelArgs = oclHostMapper.getKernelArgs();

	Host2ndPass oh2nd(oclHostMapper.getKernelNames(), oclHostMapper.getClMemMapping(), oclHostMapper.getEquivalenceMap(), progWithKernels, builder);
	oh2nd.mapNamesToLambdas(kernelEntries);
//	oh2nd.updateKernelArgs(kernelArgs, oclHostMapper.getReplacements());

	ClmemTable cl_mems = oh2nd.getCleanedStructures();
//	for_each(cl_mems, [](std::pair<VariablePtr, VariablePtr> a) {
//		std::cout << "\nHate " << *a.first << " : " << *a.first->getType() << " " << *a.second << " : " << *a.second->getType();
//	});

	HostMapper3rdPass ohm3rd(builder, cl_mems, kernelArgs, oclHostMapper.getLocalMemDecls(), oh2nd.getKernelNames(),
		oh2nd.getKernelLambdas(), oclHostMapper.getEquivalenceMap(), oclHostMapper.getReplacements(), progWithKernels);

	/*	if(core::ProgramPtr newProg = dynamic_pointer_cast<const core::Program>(ohm3rd.mapElement(0, progWithKernels))) {
	 mProgram = newProg;
	 return newProg;
	 } else
	 assert(newProg && "Second pass of OclHostCompiler corrupted the program");
	 */
	NodePtr transformedProg = ohm3rd.mapElement(0, progWithKernels);

	insieme::utils::map::PointerMap<NodePtr, NodePtr>& tmp = oclHostMapper.getReplacements();
	for_each(cl_mems, [&](std::pair<const VariablePtr, VariablePtr> t){
		tmp[t.first] = t.second;
//		if(dynamic_pointer_cast<const StructType>(t.second->getType())) {
//			replacing the types of all structs with the same type. Should get rid of cl_* stuff in structs
//			std::cout << "Replacing ALL \n" << t.first << " " << t.first->getType() << "\nwith\n" << t.second << " " << t.second->getType() << "\n";
//		}
	});


	if(core::ProgramPtr newProg = dynamic_pointer_cast<const core::Program>(
			core::transform::replaceAll(builder.getNodeManager(), transformedProg, tmp, false))) {
		// remove unnecessary derefs

		NodeMapping* h;
		auto mapper = makeLambdaMapper([this, &h](unsigned index, const NodePtr& element)->NodePtr{
			// stop recursion at type level
			if (element->getNodeCategory() == NodeCategory::NC_Type) {
				return element;
			}

			if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(element)) {
				const vector<TypePtr>& paramTys = static_pointer_cast<const FunctionType>(call->getFunctionExpr()->getType())->getParameterTypes()->getTypes();
				ExpressionList newArgs;
				bool update = false;
				int cnt = 0;

				if(paramTys.size() == call->getArguments().size()) { // undefined functions have an empty parameter list
					for_each(call->getArguments(), [&](const ExpressionPtr& arg){
						const CallExprPtr& fArg = dynamic_pointer_cast<const CallExpr>(arg);

						if( fArg &&	this->builder.getNodeManager().getLangBasic().isRefDeref(fArg->getFunctionExpr()) &&
								(!dynamic_pointer_cast<const RefType>(arg->getType()) && arg->getType()->getNodeType() != core::NT_GenericType ) &&
								(!!dynamic_pointer_cast<const RefType>(paramTys.at(cnt)))) {
							update = true; // remove unnecessary drefs
							newArgs.push_back(fArg->getArgument(0));
						} else {
							newArgs.push_back(arg);
						}
						++cnt;
					});
					if(update) {
						return this->builder.callExpr(call->getType(), call->getFunctionExpr(), newArgs)->substitute(this->builder.getNodeManager(), *h);
					}
				}
			}

			return element->substitute(builder.getNodeManager(), *h);
		});
		h = &mapper;
		mProgram = h->map(0, newProg);
//std::cout << "Replacements: \n" << cl_mems.begin()->first->getType() << " " << cl_mems.begin()->second->getType() << std::endl;
//		transform::utils::MemberAccessLiteralUpdater malu(builder);
//		mProgram = dynamic_pointer_cast<const core::Program>(malu.mapElement(0, newProg));

//std::cout << "\nReplacements: \n\t" << join("\n\t", cl_mems, [](std::ostream& out, const std::pair<VariablePtr, VariablePtr>& cur) {
//	out << *cur.first->getType() << " " << *cur.first << " => " << *cur.second->getType() << " " << *cur.second;
//}) << "\n\n";
		mProgram = core::transform::fixTypesGen(builder.getNodeManager(), mProgram, cl_mems, false);
//	LOG(FATAL) << printer::PrettyPrinter(newProg);

		// removes cl_* variables from argument lists of lambdas
		auto cleaner = makeLambdaMapper([this, &h](unsigned index, const NodePtr& element)->NodePtr{
			// stop recursion at type level
			if (element->getNodeCategory() == NodeCategory::NC_Type) {
				return element;
			}

			if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(element)) {
				if(const LambdaExprPtr& lambda = dynamic_pointer_cast<const LambdaExpr>(call->getFunctionExpr())) {
					ExpressionList newArgs;
					core::VariableList newParams;
					const core::VariableList& oldParams = lambda->getParameterList()->getElements();
					TypeList paramTypes;
					bool update = false;
					int cnt = 0;

					for_each(call->getArguments(), [&](const ExpressionPtr& arg){
						// do nothing if the argument type is not a cl_* type
						if(arg->getType()->toString().find("array<_cl_") == string::npos) {
							newArgs.push_back(arg);
							newParams.push_back(oldParams.at(cnt));
							paramTypes.push_back(oldParams.at(cnt)->getType());
						} else {
							// do not port cl_* types to the new type
							update = true;
//std::cout << "\ndropping " << *arg->getType() << " - " << *arg << std::endl;
						}
						++cnt;
					});
					if(update) {
						const LambdaExprPtr newLambda = this->builder.lambdaExpr(this->builder.functionType(paramTypes, call->getType()), newParams, lambda->getBody());
						return this->builder.callExpr(call->getType(), newLambda, newArgs)->substitute(this->builder.getNodeManager(), *h);
					}
				}
			}

			return element->substitute(builder.getNodeManager(), *h);
		});

		h = &cleaner;
		mProgram = h->map(0, mProgram);
	} else
		assert(newProg && "Third pass of OclHostCompiler corrupted the program");

	assert(mProgram && "OclHostCompiler corrupted the program");

	return mProgram;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
