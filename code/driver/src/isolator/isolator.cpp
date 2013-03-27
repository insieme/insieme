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

#include <stdio.h>
#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/driver/isolator/isolator.h"
#include "insieme/driver/isolator/lang_extension.h"
#include "insieme/driver/driver_config.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"


#include "insieme/backend/addon.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/operator_converter.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/utils/compiler/compiler.h"

#include "insieme/transform/pattern/ir_pattern.h"

namespace insieme {
namespace driver {
namespace isolator {


	namespace {

		namespace c_ast = insieme::backend::c_ast;

		class Instrumenter : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;
			const core::lang::BasicGenerator& basic;
			const Extension& ext;
			core::IRBuilder builder;

		public:

			Instrumenter(core::NodeManager& manager)
				: manager(manager),
				  basic(manager.getLangBasic()),
				  ext(manager.getLangExtension<Extension>()),
				  builder(manager) {}


			const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				// skip types
				if (ptr->getNodeCategory() == core::NC_Type) {
					return ptr;
				}

				// replace nodes bottom up
				core::NodePtr res = ptr->substitute(manager, *this);

				// the rest is only for call expressions
				if (res->getNodeType() != core::NT_CallExpr) {
					return res;
				}

				// check calls
				core::CallExprPtr call = static_pointer_cast<core::CallExprPtr>(res);
				if (core::analysis::isCallOf(call, basic.getRefDeref())) {
					if (call->getType()->getNodeType() == core::NT_RefType) {
						// this one is reading a pointer ...
						return core::CallExpr::get(manager, call->getType(), ext.getReadPtr(), call->getArguments());
					} else {
						// it is reading a value ...
						return core::CallExpr::get(manager, call->getType(), ext.getRead(), call->getArguments());
					}
				}

				if (core::analysis::isCallOf(call, basic.getRefAssign())) {
					if (call->getType()->getNodeType() == core::NT_RefType) {
						// this one is writing a pointer ...
						return core::CallExpr::get(manager, call->getType(), ext.getWritePtr(), call->getArguments());
					} else {
						// it is writing a value ...
						return core::CallExpr::get(manager, call->getType(), ext.getWrite(), call->getArguments());
					}
				}

				// if it is a call to ref-new => register result
				if (core::analysis::isCallOf(call, basic.getRefNew())) {
					auto size = builder.callExpr(basic.getUInt8(), basic.getSizeof(),
							builder.getTypeLiteral(call->getArgument(0)->getType()));
					return builder.callExpr(call->getType(), ext.getRegisterBlock(), call, size);
				}

				// handle array creation
				if (core::analysis::isCallOf(call, basic.getArrayCreate1D())) {
					auto size = builder.mul(
								builder.callExpr(basic.getUInt8(), basic.getSizeof(), call->getArgument(0)),
								call->getArgument(1)
							);
					return builder.callExpr(call->getType(), ext.getRegisterBlock(), call, size);
				}

				// nothing to be updated
				return res;
			}

		};


		core::ProgramPtr addRegionBoundaries(const core::ProgramPtr& program, const vector<core::StatementAddress>& regions) {

			core::NodeManager& manager = program->getNodeManager();
			core::IRBuilder builder(manager);
			const Extension& ext = manager.getLangExtension<Extension>();

			unsigned counter = 0;
			std::map<core::NodeAddress, core::NodePtr> replacements;
			for_each(regions, [&](const core::StatementAddress& cur) {

				// build replacement { START, TAG, <code>, END }
				auto id = builder.getIntParamLiteral(counter++);
				core::ExpressionPtr start = builder.callExpr(ext.getStart(), id);
				core::ExpressionPtr end = builder.callExpr(ext.getStop(), id);

				// extract free variables
				vector<core::VariablePtr> free = core::analysis::getFreeVariables(cur.getAddressedNode());
				std::sort(free.begin(), free.end(), compare_target<core::VariablePtr>());

				vector<core::StatementPtr> commands;
				commands.push_back(start);
				unsigned varCounter = 0;
				for_each(free, [&](const core::VariablePtr& cur) {
					assert(cur->getType()->getNodeType() == core::NT_RefType && "Supporting only ref-variables!");
					commands.push_back(builder.callExpr(ext.getTagBlock(), cur, builder.getIntParamLiteral(varCounter++)));
				});

				commands.push_back(cur.getAddressedNode());
				commands.push_back(end);

				replacements[cur] = builder.compoundStmt(commands);
			});

			return static_pointer_cast<core::ProgramPtr>(core::transform::replaceAll(manager, replacements));
		}


		core::ProgramPtr addInitFinishCalls(const core::ProgramPtr& program) {
			core::NodeManager& manager = program->getNodeManager();
			const Extension& ext = manager.getLangExtension<Extension>();

			// search body of main ...
			vector<core::ExpressionAddress> entrypoints = core::ProgramAddress(program)->getEntryPoints();
			assert(entrypoints.size() == 1 && "Can only handle a single entry point!");


			// get first entry point
			auto pattern =
					transform::pattern::aT(
							transform::pattern::var("x",
									transform::pattern::irp::compoundStmt(transform::pattern::anyList)
							)
					);

			auto match = pattern->matchAddress(core::ProgramAddress(program));
			assert(match && match->isVarBound("x") && "No Compound stmt found within program!");

			core::CompoundStmtAddress body = core::static_address_cast<core::CompoundStmtAddress>(match->getVarBinding("x").getValue());
			assert(body && "Body not found!");

			// create replacement
			core::IRBuilder builder(manager);
			auto init = builder.callExpr(ext.getInit());
			auto finish = builder.callExpr(ext.getFinish());
			auto replacement = builder.compoundStmt(init, body.getAddressedNode(), finish);

			return static_pointer_cast<core::ProgramPtr>(core::transform::replaceNode(manager, body, replacement));
		}

		core::StatementAddress isolate(unsigned id, const core::StatementAddress& region) {
			core::NodeManager& manager = region->getNodeManager();
			core::IRBuilder builder(manager);
			const Extension& ext = manager.getLangExtension<Extension>();

			// build a new context

			vector<core::StatementPtr> compound;

			// restore free variables
			vector<core::VariablePtr> free = core::analysis::getFreeVariables(region.getAddressedNode());
			std::sort(free.begin(), free.end(), compare_target<core::VariablePtr>());

			unsigned varCounter = 0;
			auto regionId = builder.getIntParamLiteral(id);
			for_each(free, [&](const core::VariablePtr& cur) {
				assert(cur->getType()->getNodeType() == core::NT_RefType && "Supporting only ref-variables!");
				compound.push_back(builder.declarationStmt(cur, builder.getZero(cur->getType())));
				compound.push_back(builder.callExpr(ext.getLoad(), cur, regionId, builder.getIntParamLiteral(varCounter++)));
			});

			// run code
			compound.push_back(region.getAddressedNode());		// TODO: wrap into parallel ...

			// finish task
			compound.push_back(builder.callExpr(ext.getFinalize()));

			// wrap isolated kernel into a program
			auto main = builder.lambdaExpr(builder.compoundStmt(compound), core::VariableList());
			core::ProgramPtr prog = core::Program::get(manager, toVector<core::ExpressionPtr>(main));


			// find pfor within isolated program again
			core::ExpressionAddress entryPoint = core::ProgramAddress(prog)->getElement(0);
			core::CompoundStmtAddress body = core::static_address_cast<core::LambdaExprAddress>(entryPoint)->getBody();
			core::StatementAddress pfor = body->getElement(body.size()-2);

			assert(core::analysis::isCallOf(pfor.getAddressedNode(), manager.getLangBasic().getPFor())
				&& "Restored address to incorrect location!");

			return pfor;
		}

	}



	vector<core::StatementAddress> isolate(const core::ProgramPtr& program, const vector<core::StatementAddress>& regions, const string& captureFile) {

		// TODO:
		//	- add init / finish call
		//  - add start / stop of regions
		//  - add read / write / read_ptr / write_ptr calls
		//  - add register blocks
		//  - add tagging of blocks

		core::NodeManager& manager = program->getNodeManager();

		// produce resulting program
		core::ProgramPtr instrumented = program;

		// start by marking regions (while addresses are still valid)
		instrumented = addRegionBoundaries(instrumented, regions);

		// replace read/write operations and register memory blocks
		instrumented = Instrumenter(manager).map(instrumented);

		// add init / finish calls
		instrumented = addInitFinishCalls(instrumented);


		// -- Create, Compile and Run instrumented Code --

		// create instrumented code
		auto backend = insieme::backend::runtime::RuntimeBackend::getDefault();
		backend->addAddOn<IsolatorAddOn>();
		auto targetCode = backend->convert(instrumented);

		// print target code ...
//		std::cout << "Target Code: \n" << *targetCode << "\n";


		// build and run to produce context file ..
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-I " SRC_DIR "../../runtime/include -g -O0 -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -ldl -lrt -lpthread -lm");
		compiler.addFlag("-DRECORD");



		// write source to file
		string srcFile = "./tmp_isolated.c";
		string binFile = "./tmp_isolated";

		std::fstream file(srcFile, std::fstream::out);
		file << *targetCode << "\n";
		file.close();

		bool success = utils::compiler::compile(srcFile, binFile, compiler);

		if (!success) assert(success && "Failed to compile instrumented version!");

//		string binFile = utils::compiler::compileToBinary(*targetCode, compiler);
//		assert(!binFile.empty() && "Unable to compile instrumented code!");

		LOG(INFO) << "Running instrumented binary file " << binFile << " ... \n";
std::cout << "Running instrumented binary file " << binFile << " ... \n";

		// run code
		int ret = system(("IRT_NUM_WORKERS=1 IRT_CONTEXT_FILE=" + captureFile + " " + binFile).c_str());
		if (ret!= 0) assert(ret == 0 && "Error running generated executable for region measurement");
//		// delete binary
//		if (boost::filesystem::exists(binFile)) {
//			boost::filesystem::remove(binFile);
//		}


		// create isolated
		unsigned counter = 0;
		vector<core::StatementAddress> res;
		for_each(regions, [&](const core::StatementAddress& cur) {
			res.push_back(isolate(counter++, cur));
		});

		// done
		return res;
	}


	namespace {

		backend::OperatorConverterTable& addOpSupport(core::NodeManager& manager, backend::OperatorConverterTable& table) {

			const Extension& ext = manager.getLangExtension<Extension>();

			#include "insieme/backend/operator_converter_begin.inc"


			// add support for INIT / FINISH

			table[ext.getInit()] = OP_CONVERTER({
				return c_ast::call(C_NODE_MANAGER->create("INIT"));
			});

			table[ext.getFinish()] = OP_CONVERTER({
				return c_ast::call(C_NODE_MANAGER->create("FINISH"));
			});


			// add support for START / STOP

			table[ext.getStart()] = OP_CONVERTER({
				return c_ast::call(C_NODE_MANAGER->create("START"), CONVERT_ARG(0));
			});

			table[ext.getStop()] = OP_CONVERTER({
				return c_ast::call(C_NODE_MANAGER->create("STOP"), CONVERT_ARG(0));
			});


			// add block registration

			table[ext.getRegisterBlock()] = OP_CONVERTER({
				return c_ast::call(C_NODE_MANAGER->create("REG_BLOCK"), CONVERT_ARG(0), CONVERT_ARG(1));
			});

			// tagging of blocks

			table[ext.getTagBlock()] = OP_CONVERTER({
				core::IRBuilder builder(ARG(0)->getNodeManager());
				return c_ast::call(C_NODE_MANAGER->create("TAG_BLOCK"), CONVERT_EXPR(builder.deref(ARG(0))), CONVERT_ARG(1));
			});


			// add read / write operators

			table[ext.getRead()] = OP_CONVERTER({
				core::IRBuilder builder(ARG(0)->getNodeManager());
				return c_ast::call(C_NODE_MANAGER->create("READ"), CONVERT_EXPR(builder.deref(ARG(0))));
			});

			table[ext.getReadPtr()] = OP_CONVERTER({
				core::IRBuilder builder(ARG(0)->getNodeManager());
				return c_ast::call(C_NODE_MANAGER->create("READ_PTR"), CONVERT_EXPR(builder.deref(ARG(0))));
			});

			table[ext.getWrite()] = OP_CONVERTER({
				core::IRBuilder builder(ARG(0)->getNodeManager());
				c_ast::BinaryOperationPtr assignment =
						static_pointer_cast<c_ast::BinaryOperation>(CONVERT_EXPR(builder.assign(ARG(0), ARG(1))));
				return c_ast::call(C_NODE_MANAGER->create("WRITE"),
						static_pointer_cast<c_ast::Expression>(assignment->operandA),
						static_pointer_cast<c_ast::Expression>(assignment->operandB));
			});

			table[ext.getWritePtr()] = OP_CONVERTER({
				core::IRBuilder builder(ARG(0)->getNodeManager());
				c_ast::BinaryOperationPtr assignment =
						static_pointer_cast<c_ast::BinaryOperation>(CONVERT_EXPR(builder.assign(ARG(0), ARG(1))));
				return c_ast::call(C_NODE_MANAGER->create("WRITE_PTR"),
						static_pointer_cast<c_ast::Expression>(assignment->operandA),
						static_pointer_cast<c_ast::Expression>(assignment->operandB));
			});


			// -- Restore Utilities --

			table[ext.getLoad()] = OP_CONVERTER({
				core::IRBuilder builder(ARG(0)->getNodeManager());
				return c_ast::call(C_NODE_MANAGER->create("LOAD_VALUE"),
						CONVERT_EXPR(builder.deref(ARG(0))), CONVERT_ARG(1), CONVERT_ARG(2));
			});

			table[ext.getFinalize()] = OP_CONVERTER({
				return c_ast::call(C_NODE_MANAGER->create("FINALIZE"));
			});

			#include "insieme/backend/operator_converter_end.inc"

			return table;
		}

	}

	void IsolatorAddOn::installOn(backend::Converter& converter) const {
		// register additional operators
		addOpSupport(converter.getNodeManager(), converter.getFunctionManager().getOperatorConverterTable());
	}


} // end namespace isolator
} // end namespace driver
} // end namespace insieme
