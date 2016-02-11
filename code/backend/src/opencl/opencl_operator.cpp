/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/backend/opencl/opencl_extension.h"
#include "insieme/backend/opencl/opencl_operator.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_entities.h"
#include "insieme/backend/runtime/runtime_code_fragments.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/backend_config.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/instrumentation_extension.h"

#include "insieme/core/transform/manipulation.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace opencl {

	namespace utils {
		c_ast::TypePtr namedType(c_ast::SharedCNodeManager manager, const std::string& name) {
			return manager->create<c_ast::NamedType>(manager->create(name));
		}
		
		c_ast::TypePtr vectorType(c_ast::SharedCNodeManager manager, const std::string& name) {
			return manager->create<c_ast::VectorType>(namedType(manager, name));
		}
		
		c_ast::VarDeclPtr varDecl(c_ast::SharedCNodeManager manager, const c_ast::VariablePtr& var, const c_ast::InitializerPtr& init) {
			return manager->create<c_ast::VarDecl>(var, init);
		}
		
		c_ast::InitializerPtr initializer(c_ast::SharedCNodeManager manager, const std::vector<c_ast::NodePtr>& expr) {
			return manager->create<c_ast::Initializer>(expr);
		}
		
		c_ast::InitializerPtr initializer(c_ast::SharedCNodeManager manager, const c_ast::TypePtr& type, const std::vector<c_ast::NodePtr>& expr) {
			return manager->create<c_ast::Initializer>(type, expr);
		}
		
		c_ast::StmtExprPtr stmtExpr(c_ast::SharedCNodeManager manager, const c_ast::CompoundPtr& compound) {
			return manager->create<c_ast::StmtExpr>(compound);
		}
		
		c_ast::StructTypePtr structType(c_ast::SharedCNodeManager manager, const std::string& name) {
			return manager->create<c_ast::StructType>(manager->create(name));
		}
		
		c_ast::TypePtr uint32Type(c_ast::SharedCNodeManager manager) {
			return manager->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UInt32);
		}
	}

	OperatorConverterTable& addOpenCLSpecificOps(core::NodeManager& manager, OperatorConverterTable& table, const BackendConfig& config) {
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();

		#include "insieme/backend/operator_converter_begin.inc"
		table[oclExt.getRegisterKernel()] = OP_CONVERTER {
			// just register new kernel impl
			auto table = KernelTable::get(CONVERTER);
			// register the kernel within the table
			table->registerKernel(ARG(0), ARG(1));
			context.addDependency(table);
			context.getIncludes().insert("stddef.h");
			context.getIncludes().insert("irt_opencl.h");
			// no code substitute, only dependencies
			return c_ast::ExpressionPtr();
		};
		table[oclExt.getExecuteKernel()] = OP_CONVERTER {
			/*
			input:
			opencl_execute_kernel(1u, fun018, [fun019,fun020,fun021], varlist_pack(()));
			
			output:
			({
				irt_opencl_requirement_func __irt_opencl_requirement_func_table[] = {
					&__insieme_fun019,
					&__insieme_fun020,
					&__insieme_fun021
				;
				irt_opencl_execute(1, &__insieme_fun018, 3, __irt_opencl_requirement_func_table, 0);
			})
			*/
			auto manager = C_NODE_MANAGER;
			core::IRBuilder builder(call->getNodeManager());
			
			auto id = CONVERT_ARG(0);
			auto nd = CONVERT_ARG(1);
			
			auto reqs = core::encoder::toValue<core::ExpressionList, core::encoder::DirectExprListConverter>(ARG(2));
			auto size =	CONVERT_EXPR(builder.uintLit(reqs.size()));
			// obtain the type of requirement function
			auto var = c_ast::var(utils::vectorType(manager, "irt_opencl_data_requirement_func"),
								  "__irt_opencl_data_requirement_func_table");
			// put together the init list for @var
			std::vector<c_ast::NodePtr> init;			
			for(const auto& req : reqs)
				init.push_back(CONVERT_EXPR(req));
			
			// finally we can substitute it with the actual IRT call
			return utils::stmtExpr(manager, c_ast::compound(
				utils::varDecl(manager, var, utils::initializer(manager, init)), // this models the func_table 
				c_ast::call(manager->create("irt_opencl_execute"), id, nd, size, var, CONVERT_EXPR(builder.uintLit(0)))
			));
		};
		table[oclExt.getMakeDataRequirement()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			auto requirement = DataRequirement::decode(call);
			
			std::vector<c_ast::NodePtr> init;
			auto table = runtime::TypeTable::get(CONVERTER);
			// this step is very important, we need to register the type this requirement references
			// e.g ref<array<real<4>,1000>,f,f,plain> must register real<4>
			auto type = requirement->getType();
			if(lang::isReference(type)) type = lang::ReferenceType(type).getElementType();
			if(lang::isArray(type)) type = lang::ArrayType(type).getElementType();
			// register it and put type_id into initializer
			init.push_back(c_ast::lit(utils::uint32Type(manager), std::to_string(table->registerType(type))));
			
			// map the AccessMode to IRT enums
			switch (requirement->getAccessMode()) {
			case DataRequirement::AccessMode::RO:	init.push_back(manager->create("IRT_OPENCL_DATA_MODE_READ_ONLY")); break;
			case DataRequirement::AccessMode::WO:	init.push_back(manager->create("IRT_OPENCL_DATA_MODE_WRITE_ONLY")); break;
			case DataRequirement::AccessMode::RW:	init.push_back(manager->create("IRT_OPENCL_DATA_MODE_READ_WRITE")); break;
			}
			// add the indirection to grab the ranges at runtime
			init.push_back(CONVERT_EXPR(requirement->getNumRanges()));
			init.push_back(CONVERT_EXPR(requirement->getRangeExpr()));
			return utils::initializer(manager, utils::namedType(manager, "irt_opencl_data_requirement"), init);
		};
		table[oclExt.getMakeDataRange()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			auto range = DataRange::decode(call);
			
			std::vector<c_ast::NodePtr> init;
			init.push_back(CONVERT_EXPR(range->getSize()));
			init.push_back(CONVERT_EXPR(range->getStart()));
			init.push_back(CONVERT_EXPR(range->getEnd()));
			
			return utils::initializer(manager, utils::namedType(manager, "irt_opencl_data_range"), init);
		};
		table[oclExt.getMakeNDRange()] = OP_CONVERTER {
			auto manager = C_NODE_MANAGER;
			auto ndrange = NDRange::decode(call);
			
			std::vector<c_ast::NodePtr> init;
			init.push_back(CONVERT_EXPR(ndrange->getWorkDim()));
			auto addWorkSizes = [&](const ExpressionList& sizes) {
				std::vector<c_ast::NodePtr> lst;
				for (const ExpressionPtr& size : sizes)
					lst.push_back(CONVERT_EXPR(size));
				while (lst.size() < 3)
					lst.push_back(c_ast::lit(utils::uint32Type(manager), "0"));
				return utils::initializer(manager, lst);
			};
			init.push_back(addWorkSizes(ndrange->getGlobalWorkSizes()));
			init.push_back(addWorkSizes(ndrange->getLocalWorkSizes()));
			
			return utils::initializer(manager, utils::namedType(manager, "irt_opencl_ndrange"), init);
		};
		#include "insieme/backend/operator_converter_end.inc"
		return table;
	}
	
	void addOpenCLSpecificHeaders(FunctionIncludeTable& table) {

	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
