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

#include "insieme/utils/map_utils.h"

#include "insieme/core/forward_decls.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/converter.h"

namespace insieme {
namespace backend {
namespace ocl_host {

	// ------------------------------------------------------------------------
	//  Within this header file a list of special code fragments used for
	//  creating code to be executed on the Insieme runtime is defined.
	// ------------------------------------------------------------------------

	class KernelCodeTable;
	typedef Ptr<KernelCodeTable> KernelCodeTablePtr;

	struct KernelCode;

	/**
	 * The implementation table fragment realizing a list of indexable kernel
	 * code strings embedded within the resutling target code.
	 */
	class KernelCodeTable : public c_ast::CodeFragment {

		const Converter& converter;

		utils::map::PointerMap<core::ExpressionPtr, unsigned> kernelMap;

		vector<KernelCode> codes;

	public:

		KernelCodeTable(const Converter& converter)
			: converter(converter), kernelMap(), codes() {
			addInclude("irt_all_impls.h");
		}

		static KernelCodeTablePtr get(const Converter& converter);

		const c_ast::ExpressionPtr getTableToken();

		unsigned registerKernel(const core::ExpressionPtr& kernel);

		virtual std::ostream& printTo(std::ostream& out) const;

	};


	struct KernelCode {
		backend::TargetCodePtr code;

		KernelCode(const backend::TargetCodePtr& code) : code(code) {}
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
