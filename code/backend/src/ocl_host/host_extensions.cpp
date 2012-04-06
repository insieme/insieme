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

#include "insieme/backend/ocl_host/host_extensions.h"

namespace insieme {
namespace backend {
namespace ocl_host{

	namespace {

		const core::TypePtr getBufferType(core::NodeManager& manager) {
			core::IRBuilder builder(manager);

			// create the irt_ocl_buffer type as a generic type
			return builder.genericType("irt_ocl_buffer");
		}

		const core::TypePtr getRefBufferType(core::NodeManager& manager) {
			core::IRBuilder builder(manager);

			// create the ref<irt_ocl_buffer> type
			return builder.refType(getBufferType(manager));
		}

		// irt_ocl_rt_run_kernel(...)
		const core::LiteralPtr getCallKernel(core::NodeManager& manager) {
			return core::lang::getLiteral(manager, "('a,ref<vector<uint<8>,#l>>,vector<uint<8>,#l>,vector<uint<8>,#l>,var_list)->unit", "call_kernel");
		}

		const core::LiteralPtr getCreateBuffer(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();

			core::TypePtr uint8Type = basic.getUInt8();
			core::TypePtr refBufferType = getRefBufferType(manager);
			//core::TypePtr enumType = builder.genericType("cl_mem_flags");

			// irt_ocl_buffer* irt_ocl_rt_create_buffer(cl_mem_flags flags, size_t size);
			//core::TypePtr type = builder.functionType(toVector<core::TypePtr>(enumType, uint8Type), refBufferType);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(uint8Type, uint8Type), refBufferType);

			return builder.literal(type, "irt_ocl_rt_create_buffer");
		}

		const core::LiteralPtr getReadBuffer(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));
			core::TypePtr uint4Type = basic.getUInt4();
			core::TypePtr uint8Type = basic.getUInt8();

			// void irt_ocl_read_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t offset, size_t size, void* source_ptr);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refBufferType, uint4Type, uint8Type, uint8Type, basic.getAnyRef()),  basic.getUnit());

			return builder.literal(type, "irt_ocl_read_buffer");
		}

		const core::LiteralPtr getWriteBuffer(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));
			core::TypePtr uint4Type = basic.getUInt4();
			core::TypePtr uint8Type = basic.getUInt8();

			// void irt_ocl_write_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t offset, size_t size, const void* source_ptr);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refBufferType, uint4Type, uint8Type, uint8Type, basic.getAnyRef()),  basic.getUnit());

			return builder.literal(type, "irt_ocl_write_buffer");
		}

		const core::LiteralPtr getReleaseBuffer(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();

			core::TypePtr refBufferType = builder.refType(getBufferType(manager));

			// void irt_ocl_release_buffer(irt_ocl_buffer* buf);
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(refBufferType), basic.getUnit());

			return builder.literal(type, "irt_ocl_release_buffer");
		}

	}


	Extensions::Extensions(core::NodeManager& manager)
		  : core::lang::Extension(manager),
		    callKernel(getCallKernel(manager)),
			bufferType(getBufferType(manager)),
			refBufferType(getRefBufferType(manager)),
			createBuffer(getCreateBuffer(manager)), readBuffer(getReadBuffer(manager)),
			writeBuffer(getWriteBuffer(manager)), releaseBuffer(getReleaseBuffer(manager)) {}


} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme
