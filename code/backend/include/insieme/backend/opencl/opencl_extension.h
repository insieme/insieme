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

#pragma once

#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace backend {
namespace opencl {
	
	class OpenCLExtension : public core::lang::Extension {
		friend class core::NodeManager;
		OpenCLExtension(core::NodeManager& manager);
	  public:
		// import reference extension to utilize aliases
		IMPORT_MODULE(core::lang::ReferenceExtension);
		// used to register a kernel source under the given id (which is an index within the global kernel table)
		// 1st arg: kernel id
		// 2nd arg: kernel source
		LANG_EXT_LITERAL(RegisterKernel, "opencl_register_kernel", "(uint<4>, (ref<array<char,inf>,t,f,plain>,int<8>))->unit");
		// used to register a kernels nd-range
		// 1st arg: kernel id
		// 2nd arg: work_dim
		// 3rd arg: global_work_size
		// 4th arg: local_work_size
		LANG_EXT_LITERAL(RegisterNDRange, "opencl_register_ndrange", "(uint<4>, uint<4>, array<uint<4>, 3>, array<uint<4>, 3>)->unit");

		// represents a data range within a DataRequirement
		LANG_EXT_TYPE(DataRange, "opencl_data_range");
		// constructor for the latter
		LANG_EXT_LITERAL(MakeDataRange, "opencl_make_data_range", "(uint<4>, uint<4>)->opencl_data_range");
		// represents a DataRequirement entity
		LANG_EXT_TYPE(DataRequirement, "opencl_data_requirement");
		// constructor for the latter
		LANG_EXT_LITERAL(MakeDataRequirement, "opencl_make_data_requirement", "(list<uint<4>>, list<opencl_data_range>, uint<4>)->opencl_data_requirement");
		// used to register a kernel requirement (which is in fact the requirement for the ith argument)
		// 1st arg: must be the same as used @opencl_register_kernel!
		// 2nd arg: is a list (must be same size as 2nd arg) which defines the ranges within the dimension to consider
		// 3rd arg: must be 0=RO, 1=WO or 2=RW
		LANG_EXT_LITERAL(RegisterDataRequirement, "opencl_register_data_requirement", "(uint<4>, opencl_data_requirement)->unit");
	};
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
