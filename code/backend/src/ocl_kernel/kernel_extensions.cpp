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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

#include "insieme/backend/ocl_kernel/kernel_extensions.h"

namespace insieme {
namespace backend {
namespace ocl_kernel{

	namespace {

		const string WRAP_TYPE_PREFIX = "_ocl_";

		core::TypePtr getWrapperType(const string& name, const core::TypePtr& type) {
			core::IRBuilder builder(type->getNodeManager());
			return builder.genericType(WRAP_TYPE_PREFIX + name, toVector(type));
		}

		bool isWrapperTypeInternal(const string& name, const core::TypePtr& type) {
			if (type->getNodeType() != core::NT_GenericType) {
				return false;
			}
			return static_pointer_cast<const core::GenericType>(type)->getFamilyName() == WRAP_TYPE_PREFIX + name;
		}

		const core::LiteralPtr getWrapLiteral(core::NodeManager& manager, const string& name) {
			core::IRBuilder builder(manager);

			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr wrapped = getWrapperType(name, alpha);
			core::TypePtr funType = builder.functionType(toVector(alpha), wrapped, true);
			return builder.literal(funType, WRAP_TYPE_PREFIX + "wrap_" + name);
		}

		const core::LiteralPtr getUnwrapLiteral(core::NodeManager& manager, const string& name) {
			core::IRBuilder builder(manager);

			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr wrapped = getWrapperType(name, alpha);
			core::TypePtr funType = builder.functionType(toVector(wrapped), alpha, true);
			return builder.literal(funType, WRAP_TYPE_PREFIX + "unwrap_" + name);
		}

		const core::LiteralPtr getGetter(core::NodeManager& manager, const string& name) {
			core::IRBuilder builder(manager);

			core::TypePtr uint4 = manager.getLangBasic().getUInt4();
			core::TypePtr funType = builder.functionType(toVector(uint4), uint4, true);
			return builder.literal(funType, name);
		}

		const core::LiteralPtr getKernelWrapper(core::NodeManager& manager) {
			core::IRBuilder builder(manager);

			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr funType = builder.functionType(toVector(alpha), alpha, true);
			return builder.literal(funType, "_ocl_kernel_wrapper");
		}

		const core::LiteralPtr getConvertBuiltin(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			//"(vector<'a,#l>, type<'b>)->vector<'b,#l>"
			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr beta = builder.typeVariable("b");
			core::TypePtr typeBeta = builder.genericType("type", toVector(beta));
			core::TypePtr funType = builder.functionType(toVector(alpha, typeBeta), beta, true);
			return builder.literal(funType, "_ocl_convert");
			/* // TO DO
			core::IRBuilder builder(manager);
			core::VariableIntTypeParamPtr vecLength = builder.variableIntTypeParam('l');
			core::TypePtr alpha = builder.vectorType(builder.typeVariable("a"), vecLength);
			core::TypePtr beta = builder.typeVariable("b");
			core::TypePtr typeBeta = builder.genericType("type", toVector(beta));
			core::TypePtr funType = builder.functionType(toVector(alpha, typeBeta), builder.vectorType(beta, vecLength), true);
			return builder.literal(funType, "_ocl_convert");
			*/
		}

	}


	Extensions::Extensions(core::NodeManager& manager)
	    :   core::lang::Extension(manager),
		 	wrapConst(getWrapLiteral(manager, "const")),
			unwrapConst(getUnwrapLiteral(manager, "const")),

			wrapGlobal(getWrapLiteral(manager, "global")),
			unwrapGlobal(getUnwrapLiteral(manager, "global")),

			wrapLocal(getWrapLiteral(manager, "local")),
			unwrapLocal(getUnwrapLiteral(manager, "local")),

			getLocalID(getGetter(manager, "get_local_id")),
			getGlobalID(getGetter(manager, "get_global_id")),
			getGroupID(getGetter(manager, "get_group_id")),
			getLocalSize(getGetter(manager, "get_local_size")),
			getGlobalSize(getGetter(manager, "get_global_size")),
			getNumGroups(getGetter(manager, "get_num_groups")),

			kernelWrapper(getKernelWrapper(manager)),
			convertBuiltin(getConvertBuiltin(manager)) {}



	core::TypePtr Extensions::getType(AddressSpace space, const core::TypePtr& type) const {
		switch(space) {
		case PRIVATE: return type;
		case LOCAL: return getLocalType(type);
		case GLOBAL: return getGlobalType(type);
		case CONSTANT: return getConstType(type);
		}
		assert(false && "Unsupported address space encountered!");
		return core::TypePtr();
	}

	bool Extensions::isWrapperType(const core::TypePtr& type) const {
		return isLocalType(type) || isGlobalType(type) || isConstType(type);
	}

	bool Extensions::isWrapperType(AddressSpace space, const core::TypePtr& type) const {
		switch(space) {
		case PRIVATE: return !isLocalType(type) && !isGlobalType(type) && !isConstType(type);
		case LOCAL: return isLocalType(type);
		case GLOBAL: return isGlobalType(type);
		case CONSTANT: return isConstType(type);
		}
		return false;
	}

	const core::LiteralPtr& Extensions::getWrapper(AddressSpace space) const {
		switch(space) {
		case PRIVATE: assert(false && "Not supported!");
		case LOCAL: return wrapLocal;
		case GLOBAL: return wrapGlobal;
		case CONSTANT: return wrapConst;
		}
		assert(false && "Unsupported address space encountered!");
		return wrapGlobal;
	}

	const core::LiteralPtr& Extensions::getUnWrapper(AddressSpace space) const {
		switch(space) {
		case PRIVATE: assert(false && "Not supported!");
		case LOCAL: return unwrapLocal;
		case GLOBAL: return unwrapGlobal;
		case CONSTANT: return unwrapConst;
		}
		assert(false && "Unsupported address space encountered!");
		return unwrapGlobal;
	}

	core::TypePtr Extensions::getGlobalType(const core::TypePtr& type) const {
		return getWrapperType("global", type);
	}

	core::TypePtr Extensions::getLocalType(const core::TypePtr& type) const {
		return getWrapperType("local", type);
	}

	core::TypePtr Extensions::getConstType(const core::TypePtr& type) const {
		return getWrapperType("const", type);
	}

	bool Extensions::isGlobalType(const core::TypePtr& type) const {
		return isWrapperTypeInternal("global", type);
	}

	bool Extensions::isLocalType(const core::TypePtr& type) const {
		return isWrapperTypeInternal("local", type);
	}

	bool Extensions::isConstType(const core::TypePtr& type) const {
		return isWrapperTypeInternal("const", type);
	}

	const core::TypePtr Extensions::getWrappedType(const core::TypePtr& type) const {
		if (isConstType(type) || isGlobalType(type) || isLocalType(type)) {
			return static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0];
		}
		return type;
	}

	core::ExpressionPtr Extensions::wrapExpr(AddressSpace addressSpace, const core::ExpressionPtr& value) const {

		if (addressSpace == PRIVATE) {
			return value;
		}

		core::IRBuilder builder(value->getNodeManager());

		const core::TypePtr type = getType(addressSpace, value->getType());
		const core::ExpressionPtr& wrapper = getWrapper(addressSpace);

		return builder.callExpr(type, wrapper, toVector(value));
	}

	core::ExpressionPtr Extensions::unWrapExpr(AddressSpace addressSpace, const core::ExpressionPtr& value) const {
		if (addressSpace == PRIVATE) {
			return value;
		}

		assert(isWrapperType(addressSpace, value->getType()));

		core::IRBuilder builder(value->getNodeManager());

		const core::TypePtr type = static_pointer_cast<const core::GenericType>(value->getType())->getTypeParameter()[0];
		const core::ExpressionPtr& wrapper = getUnWrapper(addressSpace);

		return builder.callExpr(type, wrapper, toVector(value));
	}

	core::ExpressionPtr Extensions::unWrapExpr(const core::ExpressionPtr& value) const {
		if (isLocalType(value->getType())) return unWrapExpr(LOCAL, value);
		if (isGlobalType(value->getType())) return unWrapExpr(GLOBAL, value);
		if (isConstType(value->getType())) return unWrapExpr(CONSTANT, value);
		return value;
	}


} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
