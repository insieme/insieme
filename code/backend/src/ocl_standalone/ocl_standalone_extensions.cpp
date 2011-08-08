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

#include "insieme/backend/ocl_standalone/ocl_standalone_extensions.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"

namespace insieme {
namespace backend {
namespace ocl_standalone {

	namespace {

		const core::TypePtr getKernelType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the irt_ocl_kernel type as a generic type
			return builder.genericType("irt_ocl_kernel");
		}

		const core::TypePtr getDeviceType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the irt_ocl_device type as a generic type
			return builder.genericType("irt_ocl_device");
		}

		const core::TypePtr getBufferType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the irt_ocl_buffer type as a generic type
			return builder.genericType("irt_ocl_buffer");
		}

		const core::LiteralPtr getInitDevices(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// void irt_ocl_init_devices();
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(), basic.getUnit());

			return builder.literal(type, "irt_ocl_init_devices");
		}

		const core::LiteralPtr getGetNumDevices(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr uint4Type = basic.getUInt4();
			// cl_uint irt_ocl_get_num_devices();
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(), uint4Type);

			return builder.literal(type, "irt_ocl_get_num_devices");
		}

		const core::LiteralPtr getGetDevice(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refDeviceType = builder.refType(getDeviceType(manager));
			core::TypePtr uint4Type = basic.getUInt4();

			// irt_ocl_device* irt_ocl_get_device(cl_uint id);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(uint4Type), refDeviceType);

			return builder.literal(type, "irt_ocl_get_device");
		}

		const core::LiteralPtr getReleaseDevices(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// void irt_ocl_release_devices();
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(), basic.getUnit());

			return builder.literal(type, "irt_ocl_release_devices");
		}

		const core::LiteralPtr getCreateKernel(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refKernelType = builder.refType(getKernelType(manager));
			core::TypePtr refDeviceType = builder.refType(getDeviceType(manager));
			core::TypePtr refCharType = builder.refType(basic.getChar());
			core::TypePtr enumType = builder.genericType("irt_ocl_create_kernel_flag");


			//irt_ocl_kernel* irt_ocl_create_kernel(irt_ocl_device* dev, const char* file_name,
			//		const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refDeviceType, refCharType, refCharType,refCharType, enumType), refKernelType);

			return builder.literal(type, "irt_ocl_create_kernel");
		}

		const core::LiteralPtr getSetKernelNDrange(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refKernelType = builder.refType(getKernelType(manager));
			core::TypePtr uint4Type = basic.getUInt4();
			core::TypePtr refUint4Type = builder.refType(uint4Type);

			// void irt_ocl_set_kernel_ndrange(irt_ocl_kernel* kernel, cl_uint work_dim, size_t* global_work_size, size_t* local_work_size);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refKernelType, uint4Type, refUint4Type, refUint4Type), basic.getUnit());

			return builder.literal(type, "irt_ocl_set_kernel_ndrange");
		}


		const core::LiteralPtr getRunKernel(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refKernelType = builder.refType(getKernelType(manager));
			core::TypePtr uint4Type = basic.getUInt4();
			core::TypePtr varListType = basic.getVarList();

			// void irt_ocl_run_kernel(irt_ocl_kernel* kernel, cl_uint num_args, ...);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refKernelType, uint4Type, varListType), basic.getUnit()); // FIXME: variable number parameters

			return builder.literal(type, "irt_ocl_run_kernel");
		}

		const core::LiteralPtr getReleaseKernel(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refKernelType = builder.refType(getKernelType(manager));

			// void irt_ocl_release_kernel(irt_ocl_kernel* kernel);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refKernelType), basic.getUnit());

			return builder.literal(type, "irt_ocl_release_kernel");
		}

		const core::LiteralPtr getCreateBuffer(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));
			core::TypePtr refDeviceType = builder.refType(getDeviceType(manager));
			core::TypePtr uint4Type = basic.getUInt4();
			core::TypePtr enumType = builder.genericType("cl_mem_flags");

			// irt_ocl_buffer* irt_ocl_create_buffer(irt_ocl_device* dev, cl_mem_flags flags, size_t size);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refDeviceType, enumType, uint4Type), refBufferType);

			return builder.literal(type, "irt_ocl_create_buffer");
		}

		const core::LiteralPtr getReadBuffer(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));
			core::TypePtr boolType = basic.getBool();
			core::TypePtr uint4Type = basic.getUInt4();

			// void irt_ocl_read_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t size, void* source_ptr);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refBufferType, boolType, uint4Type, basic.getAnyRef()),  basic.getUnit());

			return builder.literal(type, "irt_ocl_read_buffer");
		}

		const core::LiteralPtr getWriteBuffer(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));
			core::TypePtr boolType = basic.getBool();
			core::TypePtr uint4Type = basic.getUInt4();

			// void irt_ocl_write_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t size, const void* source_ptr);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refBufferType, boolType, uint4Type, basic.getAnyRef()),  basic.getUnit());

			return builder.literal(type, "irt_ocl_write_buffer");
		}

		const core::LiteralPtr getReleaseBuffer(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));

			// void irt_ocl_release_buffer(irt_ocl_buffer* buf);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refBufferType), basic.getUnit());

			return builder.literal(type, "irt_ocl_release_buffer");
		}

		const string WRAP_TYPE_PREFIX = "_ocl_";

		core::TypePtr getWrapperType(const string& name, const core::TypePtr& type) {
			core::ASTBuilder builder(type->getNodeManager());
			return builder.genericType(WRAP_TYPE_PREFIX + name, toVector(type));
		}

		bool isWrapperTypeInternal(const string& name, const core::TypePtr& type) {
			if (type->getNodeType() != core::NT_GenericType) {
				return false;
			}
			return static_pointer_cast<const core::GenericType>(type)->getFamilyName() == WRAP_TYPE_PREFIX + name;
		}

		const core::LiteralPtr getWrapLiteral(core::NodeManager& manager, const string& name) {
			core::ASTBuilder builder(manager);

			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr wrapped = getWrapperType(name, alpha);
			core::TypePtr funType = builder.functionType(toVector(alpha), wrapped, true);
			return builder.literal(funType, WRAP_TYPE_PREFIX + "wrap_" + name);
		}

		const core::LiteralPtr getUnwrapLiteral(core::NodeManager& manager, const string& name) {
			core::ASTBuilder builder(manager);

			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr wrapped = getWrapperType(name, alpha);
			core::TypePtr funType = builder.functionType(toVector(wrapped), alpha, true);
			return builder.literal(funType, WRAP_TYPE_PREFIX + "unwrap_" + name);
		}

		const core::LiteralPtr getGetter(core::NodeManager& manager, const string& name) {
			core::ASTBuilder builder(manager);

			core::TypePtr uint4 = manager.getBasicGenerator().getUInt4();
			core::TypePtr funType = builder.functionType(toVector(uint4), uint4, true);
			return builder.literal(funType, name);
		}

		const core::LiteralPtr getKernelWrapper(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr funType = builder.functionType(toVector(alpha), alpha, true);
			return builder.literal(funType, "_ocl_kernel_wrapper");
		}

	}


	Extensions::Extensions(core::NodeManager& manager)
		  : initDevices(getInitDevices(manager)), getNumDevices(getGetNumDevices(manager)),
			getDevice(getGetDevice(manager)), releaseDevices(getReleaseDevices(manager)),

			createKernel(getCreateKernel(manager)),setKernelNDrange(getSetKernelNDrange(manager)),
			runKernel(getRunKernel(manager)), releaseKernel(getReleaseKernel(manager)),

			createBuffer(getCreateBuffer(manager)), readBuffer(getReadBuffer(manager)),
			writeBuffer(getWriteBuffer(manager)), releaseBuffer(getReleaseBuffer(manager)),

			wrapConst(getWrapLiteral(manager, "const")),
			unwrapConst(getUnwrapLiteral(manager, "const")),

			wrapGlobal(getWrapLiteral(manager, "global")),
			unwrapGlobal(getUnwrapLiteral(manager, "global")),

			wrapLocal(getWrapLiteral(manager, "local")),
			unwrapLocal(getUnwrapLiteral(manager, "local")),

			getLocalID(getGetter(manager, "get_local_id")),
			getGlobalID(getGetter(manager, "get_global_id")),
			getLocalSize(getGetter(manager, "get_local_size")),
			getGlobalSize(getGetter(manager, "get_global_size")),
			getNumGroups(getGetter(manager, "get_num_groups")),

			kernelWrapper(getKernelWrapper(manager)) {}


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
		return wrapGlobal;
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

	core::ExpressionPtr Extensions::wrapExpr(AddressSpace addressSpace, const core::ExpressionPtr& value) const {

		if (addressSpace == PRIVATE) {
			return value;
		}

		core::ASTBuilder builder(value->getNodeManager());

		const core::TypePtr type = getType(addressSpace, value->getType());
		const core::ExpressionPtr& wrapper = getWrapper(addressSpace);

		return builder.callExpr(type, wrapper, toVector(value));
	}

	core::ExpressionPtr Extensions::unWrapExpr(AddressSpace addressSpace, const core::ExpressionPtr& value) const {
		if (addressSpace == PRIVATE) {
			return value;
		}

		assert(isWrapperType(addressSpace, value->getType()));

		core::ASTBuilder builder(value->getNodeManager());

		const core::TypePtr type = static_pointer_cast<const core::GenericType>(value->getType())->getTypeParameter()[0];
		const core::ExpressionPtr& wrapper = getUnWrapper(addressSpace);

		return builder.callExpr(type, wrapper, toVector(value));
	}


} // end namespace ocl_standalone
} // end namespace backend
} // end namespace insieme
