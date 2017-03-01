/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/backend/opencl/opencl_extension.h"
#include "insieme/backend/opencl/opencl_operator.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_entities.h"
#include "insieme/backend/opencl/opencl_analysis.h"
#include "insieme/backend/runtime/runtime_code_fragments.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/backend_config.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/varargs_extension.h"

#include "insieme/core/transform/manipulation.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace opencl {

	namespace utils {
		// used to model convinient shortcuts
		class CBuilder {
			c_ast::SharedCNodeManager manager;
		public:
			CBuilder(const c_ast::SharedCNodeManager& manager) : manager(manager) {}

			c_ast::TypePtr namedType(const std::string& name) {
				return manager->create<c_ast::NamedType>(manager->create(name));
			}

			c_ast::TypePtr vectorType(const std::string& name) {
				return manager->create<c_ast::VectorType>(namedType(name));
			}

			c_ast::VarDeclPtr varDecl(const c_ast::VariablePtr& var, const c_ast::InitializerPtr& init) {
				return manager->create<c_ast::VarDecl>(var, init);
			}

			c_ast::InitializerPtr initializer(const std::vector<c_ast::NodePtr>& expr) {
				return manager->create<c_ast::Initializer>(expr);
			}

			c_ast::InitializerPtr initializer(const c_ast::TypePtr& type, const std::vector<c_ast::NodePtr>& expr) {
				return manager->create<c_ast::Initializer>(type, expr);
			}

			c_ast::StructTypePtr structType(const std::string& name) {
				return manager->create<c_ast::StructType>(manager->create(name));
			}

			c_ast::TypePtr uint32Type() {
				return manager->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UInt32);
			}

			c_ast::StmtExprPtr stmtExpr(const std::vector<c_ast::NodePtr>& body) {
				assert_false(body.empty()) << "a statement expression must consist of at least one node";

				auto compound = c_ast::compound(body.front());
				// copy over all the statements -- body.front() is required to obtain a ref to the manager
				compound->statements = body;
				// wrap the compound into an expression
				return compound->getManager()->create<c_ast::StmtExpr>(compound);
			}
		};
	}

	OperatorConverterTable& addOpenCLSpecificOps(core::NodeManager& manager, OperatorConverterTable& table, const BackendConfig& config) {
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();

		#include "insieme/backend/operator_converter_begin.inc"
		table[oclExt.getRegisterKernel()] = OP_CONVERTER {
			// just register new kernel impl
			auto table = KernelTable::get(CONVERTER);
			// register the kernel within the table
			table->registerKernel(ARG(0), ARG(1), ARG(2));
			context.addDependency(table);
			context.getIncludes().insert("stddef.h");
			context.getIncludes().insert("irt_opencl.h");
			// no code substitute, only dependencies
			return c_ast::ExpressionPtr();
		};
		table[oclExt.getExecuteKernel()] = OP_CONVERTER {
			/*
			input:
			opencl_execute_kernel(1u, fun018, [fun019,fun020,fun021], varlist_pack(make_optional(sizeof(int<4>), callExpr)));

			output:
			({
				irt_opencl_requirement_func __irt_opencl_requirement_func_table[] = {
					&__insieme_fun019,
					&__insieme_fun020,
					&__insieme_fun021
				;
				int32_t tmp0 = callExpr();
				irt_opencl_execute(1, &__insieme_fun018, 3, __irt_opencl_requirement_func_table, 1, sizeof(int32_t), tmp0);
			})
			*/
			auto manager = C_NODE_MANAGER;
			core::IRBuilder builder(call->getNodeManager());
			utils::CBuilder cb(manager);

			auto id = CONVERT_ARG(0);
			auto nd = CONVERT_ARG(1);

			auto requirements = core::encoder::toValue<core::ExpressionList, core::encoder::DirectExprListConverter>(ARG(2));
			// obtain the type of requirement function
			auto requirementsTable = c_ast::var(cb.vectorType("irt_opencl_data_requirement_func"),
								  "__irt_opencl_data_requirement_func_table");
			// put together the init list for @var
			std::vector<c_ast::NodePtr> init;
			for(const auto& req : requirements)
				init.push_back(CONVERT_EXPR(req));

			auto va = call->getArgument(3);
			assert_true(core::analysis::isCallOf(va, call->getNodeManager().getLangExtension<core::lang::VarArgsExtension>().getVarlistPack())) << "expected varlist_pack as argument";
			// if core::IRBuilder was used -- which is the encouraged way of doing it -- it must be a tupleExpr
			auto tupleExpr = core::analysis::getArgument(va, 0).isa<core::TupleExprPtr>();
			assert_true(tupleExpr) << "expected a tupleExpr as argument of varlist_pack";
			// acquire the list of packed expressions and make sure the number is sane
			core::ExpressionList optionals = tupleExpr->getExpressions()->getElements();

			std::vector<c_ast::NodePtr> args;
			args.push_back(id);
			args.push_back(nd);
			args.push_back(CONVERT_EXPR(builder.uintLit(requirements.size())));
			args.push_back(requirementsTable);
			// inform the IRT about the number of optionals present
			args.push_back(CONVERT_EXPR(builder.uintLit(optionals.size())));
			for (const auto& expr : optionals) {
				auto optional = Optional::decode(expr);
				// check if size is acceptable for IRT first
				auto size = CONVERT_EXPR(optional->getSize()).isa<c_ast::UnaryOperationPtr>();
				assert_true(size && size->operation == c_ast::UnaryOperation::SizeOf)
					<< "an optional argument must be modeled as tuple (size,value)";
				auto type = size->operand.isa<c_ast::PrimitiveTypePtr>();
				assert_true(type) << "an optional argument must be of primitive type";
				// check for further restrictions
				switch (type->type) {
				case c_ast::PrimitiveType::Void:
				case c_ast::PrimitiveType::Int128:
				case c_ast::PrimitiveType::UInt128:
						// u/int128 is not allowed as IRT can only handle u/int64 in a
						// portable and reliable manner! -- c99 va_arg actually.
						assert_fail() << "invalid primitive type for optional argument";
						break;
				default:
						// no-op fall through
						break;
				}
				args.push_back(size);
				// type is fine, proceed with value
				auto value = optional->getValue();
				// in case we face a literal we need to convert the value on our own!
				if (value->getNodeType() == core::NT_Literal)
					args.push_back(CONVERT_EXPR(value));
				else
					// it might be a variable, call or bind
					args.push_back(CONVERTER.getFunctionManager().getValue(context, value));
				// generate the optional modifier as well
				switch (optional->getModifier()) {
				case Optional::Modifier::HOST_PRIMITIVE: args.push_back(manager->create("IRT_OPENCL_OPTIONAL_MODE_HOST_PRIMITIVE")); break;
				case Optional::Modifier::KRNL_BUFFER:    args.push_back(manager->create("IRT_OPENCL_OPTIONAL_MODE_KRNL_BUFFER")); break;
				}
			}

			std::vector<c_ast::NodePtr> body;
			// add the vardecl for the table
			body.push_back(cb.varDecl(requirementsTable, cb.initializer(init)));
			// finally construct the call
			body.push_back(c_ast::call(manager->create("irt_opencl_execute"), args));
			// as the op-converter must return an expression wrap it properly
			return cb.stmtExpr(body);
		};
		table[oclExt.getMakeDataRequirement()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			auto requirement = DataRequirement::decode(call);
			utils::CBuilder cb(manager);

			std::vector<c_ast::NodePtr> init;
			auto table = runtime::TypeTable::get(CONVERTER);
			// this step is very important, we need to register the type this requirement references
			// e.g ref<array<real<4>,1000>,f,f,plain> must register real<4>
			auto type = analysis::getUnderlyingType(requirement->getType());
			// register it and put type_id into initializer
			init.push_back(c_ast::lit(cb.uint32Type(), std::to_string(table->registerType(context, type))));

			// map the AccessMode to IRT enums
			switch (requirement->getAccessMode()) {
			case DataRequirement::AccessMode::RO:	init.push_back(manager->create("IRT_OPENCL_DATA_MODE_READ_ONLY")); break;
			case DataRequirement::AccessMode::WO:	init.push_back(manager->create("IRT_OPENCL_DATA_MODE_WRITE_ONLY")); break;
			case DataRequirement::AccessMode::RW:	init.push_back(manager->create("IRT_OPENCL_DATA_MODE_READ_WRITE")); break;
			}
			// add the indirection to grab the ranges at runtime
			init.push_back(CONVERT_EXPR(requirement->getNumRanges()));
			init.push_back(CONVERT_EXPR(requirement->getRangeExpr()));
			return cb.initializer(cb.namedType("irt_opencl_data_requirement"), init);
		};
		table[oclExt.getMakeDataRange()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			auto range = DataRange::decode(call);
			utils::CBuilder cb(manager);

			std::vector<c_ast::NodePtr> init;
			init.push_back(CONVERT_EXPR(range->getSize()));
			init.push_back(CONVERT_EXPR(range->getStart()));
			init.push_back(CONVERT_EXPR(range->getEnd()));

			return cb.initializer(cb.namedType("irt_opencl_data_range"), init);
		};
		table[oclExt.getMakeNDRange()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			auto ndrange = NDRange::decode(call);
			utils::CBuilder cb(manager);

			std::vector<c_ast::NodePtr> init;
			init.push_back(CONVERT_EXPR(ndrange->getWorkDim()));
			auto addWorkSizes = [&](const ExpressionList& sizes) {
				std::vector<c_ast::NodePtr> lst;
				for (const ExpressionPtr& size : sizes)
					lst.push_back(CONVERT_EXPR(size));
				while (lst.size() < 3)
					lst.push_back(c_ast::lit(cb.uint32Type(), "0"));
				return cb.initializer(lst);
			};
			init.push_back(addWorkSizes(ndrange->getGlobalOffsets()));
			init.push_back(addWorkSizes(ndrange->getGlobalWorkSizes()));
			init.push_back(addWorkSizes(ndrange->getLocalWorkSizes()));

			return cb.initializer(cb.namedType("irt_opencl_ndrange"), init);
		};
		table[oclExt.getWorkDim()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_work_dim"));
		};
		table[oclExt.getGlobalSize()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_global_size"), CONVERT_ARG(0));
		};
		table[oclExt.getGlobalId()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_global_id"), CONVERT_ARG(0));
		};
		table[oclExt.getLocalSize()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_local_size"), CONVERT_ARG(0));
		};
		table[oclExt.getLocalId()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_local_id"), CONVERT_ARG(0));
		};
		table[oclExt.getNumGroups()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_num_groups"), CONVERT_ARG(0));
		};
		table[oclExt.getGroupId()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			return c_ast::call(manager->create("get_group_id"), CONVERT_ARG(0));
		};
		table[oclExt.getPeel()] = OP_CONVERTER {
			return CONVERT_ARG(0);
		};
		#include "insieme/backend/operator_converter_end.inc"
		return table;
	}

	void addOpenCLSpecificHeaders(FunctionIncludeTable& table) {

	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
