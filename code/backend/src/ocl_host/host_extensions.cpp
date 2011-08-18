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

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"

#include "insieme/backend/ocl_host/host_extensions.h"

namespace insieme {
namespace backend {
namespace ocl_host{

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

	}


	Extensions::Extensions(core::NodeManager& manager)
		  : initDevices(getInitDevices(manager)), getNumDevices(getGetNumDevices(manager)),
			getDevice(getGetDevice(manager)), releaseDevices(getReleaseDevices(manager)),

			createKernel(getCreateKernel(manager)),setKernelNDrange(getSetKernelNDrange(manager)),
			runKernel(getRunKernel(manager)), releaseKernel(getReleaseKernel(manager)),

			createBuffer(getCreateBuffer(manager)), readBuffer(getReadBuffer(manager)),
			writeBuffer(getWriteBuffer(manager)), releaseBuffer(getReleaseBuffer(manager)) {}


} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme
