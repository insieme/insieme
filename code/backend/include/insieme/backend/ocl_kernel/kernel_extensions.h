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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/extension.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {


	enum AddressSpace {
		PRIVATE,
		LOCAL,
		GLOBAL,
		CONSTANT,
	};

	/**
	 * This class offers a list of IR extensions required to model concepts within the
	 * OpenCL Kernel. 
	 */
	class Extensions : public core::lang::Extension {
	public:

		const core::LiteralPtr wrapConst;
		const core::LiteralPtr unwrapConst;

		const core::LiteralPtr wrapGlobal;
		const core::LiteralPtr unwrapGlobal;

		const core::LiteralPtr wrapLocal;
		const core::LiteralPtr unwrapLocal;

		const core::LiteralPtr getLocalID;

		const core::LiteralPtr getGlobalID;

		const core::LiteralPtr getGroupID;

		const core::LiteralPtr getLocalSize;

		const core::LiteralPtr getGlobalSize;

		const core::LiteralPtr getNumGroups;

		const core::LiteralPtr kernelWrapper;

		const core::LiteralPtr convertBuiltin;


	private:

		friend class core::NodeManager;

		Extensions(core::NodeManager& manager);

	public:

		core::TypePtr getType(AddressSpace space, const core::TypePtr& type) const;
		bool isWrapperType(const core::TypePtr& type) const;
		bool isWrapperType(AddressSpace space, const core::TypePtr& type) const;

		const core::LiteralPtr& getWrapper(AddressSpace space) const;
		const core::LiteralPtr& getUnWrapper(AddressSpace space) const;

		core::TypePtr getGlobalType(const core::TypePtr& type) const;
		core::TypePtr getLocalType(const core::TypePtr& type) const;
		core::TypePtr getConstType(const core::TypePtr& type) const;

		bool isGlobalType(const core::TypePtr& type) const;
		bool isLocalType(const core::TypePtr& type) const;
		bool isConstType(const core::TypePtr& type) const;

		const core::TypePtr getWrappedType(const core::TypePtr& type) const;

		core::ExpressionPtr wrapExpr(AddressSpace addressSpace, const core::ExpressionPtr& value) const;
		core::ExpressionPtr unWrapExpr(AddressSpace addressSpace, const core::ExpressionPtr& value) const;
		core::ExpressionPtr unWrapExpr(const core::ExpressionPtr& value) const;

	};

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
