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

#pragma once

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/frontend/frontend.h"

namespace insieme {

namespace core {
namespace pattern {
class TreePattern;
typedef std::shared_ptr<TreePattern> TreePatternPtr;
}
}

namespace frontend {
namespace ocl {

// enums corresponding to the flags in clCreateBuffer
enum CreateBufferFlags {
		CL_MEM_READ_WRITE = 0,
		CL_MEM_WRITE_ONLY,
		CL_MEM_READ_ONLY,
		CL_MEM_USE_HOST_PTR,
		CL_MEM_ALLOC_HOST_PTR,
		CL_MEM_COPY_HOST_PTR,
		size
};


struct ClMemMetaInfo {
	ClMemMetaInfo() : size(), type(), flags(), initExpr() {}
	ClMemMetaInfo(core::ExpressionPtr& size, core::TypePtr& type, std::set<enum CreateBufferFlags> flags, core::ExpressionPtr& hostPtr)
		: size(size), type(type), flags(flags), initExpr(hostPtr) {}

	core::ExpressionPtr size;
	core::TypePtr type;
	std::set<enum CreateBufferFlags> flags;
	core::ExpressionPtr initExpr;
};

// definitions
typedef insieme::utils::map::PointerMap<core::ExpressionAddress, ClMemMetaInfo > ClMemMetaMap;
typedef std::map<core::NodeAddress, core::NodePtr> NodeAddressMap;
/*
 * Replaces cl_mem/icl_buffer variables with INSPIRE arrays
 */
class BufferMapper: public core::transform::CachedNodeMapping {
	const core::NodePtr resolveElement(const core::NodePtr& ptr);
};


/*
 * Collects cl_mem/icl_buffer expressions, identifies the correct IR type and replaces them with INSPIRE arrays
 */
class BufferReplacer {
public:
	BufferReplacer(core::NodePtr prog);
	virtual ~BufferReplacer() {}
protected:
//	BufferMapper bufferMapper;
	ClMemMetaMap clMemMeta;
	insieme::utils::map::PointerMap<core::ExpressionAddress, core::ExpressionPtr> clMemReplacements;
	insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> generalReplacements;
	insieme::utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> declInitReplacements;
	core::NodePtr prog;

	bool alreadyThereAndCorrect(core::ExpressionAddress& bufferExpr, const core::TypePtr& newType);
	void collectInformation(core::pattern::TreePatternPtr& clCreateBuffer);
	void generateReplacements(core::TypePtr clMemTy);
	void performReplacements();

public:
	virtual core::NodePtr getTransformedProgram();
};

class IclBufferReplacer  : public BufferReplacer {
public:
	IclBufferReplacer(core::NodePtr prog);
public:
	virtual core::NodePtr getTransformedProgram();
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
