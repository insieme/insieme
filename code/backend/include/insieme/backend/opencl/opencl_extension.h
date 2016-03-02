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
#include "insieme/core/lang/varargs_extension.h"
#include "insieme/core/forward_decls.h"

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
		LANG_EXT_LITERAL(RegisterKernel, "opencl_register_kernel", "(uint<4>, (ref<array<char,inf>,t,f,plain>,int<8>))->unit");
		// used to run a given kernel
		LANG_EXT_LITERAL(ExecuteKernel, "opencl_execute_kernel", "(uint<4>, (ref<irt_wi>)->opencl_ndrange, list<(ref<irt_wi>, ref<opencl_ndrange>, uint<4>)->opencl_data_requirement>, var_list)->unit");
		// represents an nd-range
		LANG_EXT_TYPE(NDRange, "opencl_ndrange"/*, "struct { work_dim : uint<4>; global_work_size : array<uint<4>, 3>; local_work_size : array<uint<4>, 3>; }"*/);
		// used to make an ndrange for a given kernel
		// 1st arg: work_dim
		// 2nd arg: global_work_size
		// 3rd arg: local_work_size
		LANG_EXT_LITERAL(MakeNDRange, "opencl_make_ndrange", "(uint<4>, list<uint<4>>, list<uint<4>>)->opencl_ndrange");
		// represents a data range within a DataRequirement
		LANG_EXT_TYPE(DataRange, "opencl_data_range"/*, "struct { size : uint<4>; start : uint<4>; end : uint<4>; }"*/);
		// constructor for the latter
		LANG_EXT_LITERAL(MakeDataRange, "opencl_make_data_range", "(uint<4>, uint<4>, uint<4>)->opencl_data_range");
		// represents a DataRequirement entity
		LANG_EXT_TYPE(DataRequirement, "opencl_data_requirement"/*, "struct { mode : uint<4>; num_ranges : uint<4>; ranges : array<opencl_data_range, 's>; }"*/);
		// constructor for the latter
		LANG_EXT_LITERAL(MakeDataRequirement, "opencl_make_data_requirement", "(type<'a>, uint<4>, (ref<irt_wi>, ref<opencl_ndrange>, uint<4>, uint<4>)->opencl_data_range, uint<4>)->opencl_data_requirement");

		// extensions for the opencl kernel code
		LANG_EXT_LITERAL(WorkDim, "opencl_get_work_dim", "()->uint<4>");
		LANG_EXT_LITERAL(GlobalSize, "opencl_get_global_size", "(uint<4>)->uint<4>");
		LANG_EXT_LITERAL(GlobalId, "opencl_get_global_id", "(uint<4>)->uint<4>");
		LANG_EXT_LITERAL(LocalSize, "opencl_get_local_size", "(uint<4>)->uint<4>");
		LANG_EXT_LITERAL(LocalId, "opencl_get_local_id", "(uint<4>)->uint<4>");
		LANG_EXT_LITERAL(NumGroups, "opencl_get_num_groups", "(uint<4>)->uint<4>");
		LANG_EXT_LITERAL(GroupId, "opencl_get_group_id", "(uint<4>)->uint<4>");

		LANG_EXT_TYPE_WITH_NAME(MarkerGlobal, "opencl_global_marker", "opencl_global");
		LANG_EXT_TYPE_WITH_NAME(MarkerConstant, "opencl_constant_marker", "opencl_constant");
		LANG_EXT_TYPE_WITH_NAME(MarkerLocal, "opencl_local_marker", "opencl_local");
		LANG_EXT_TYPE_WITH_NAME(MarkerPrivate, "opencl_private_marker", "opencl_private");

		LANG_EXT_TYPE_WITH_NAME(GenType, "opencl_type_template", "opencl_type<'a, 'loc>");
		TYPE_ALIAS("opencl_type<'a, 'loc>", "'a");

		LANG_EXT_LITERAL(Peel, "opencl_peel", "(ref<opencl_type<'a, 'loc>,'c,'v,plain))->'a");
	};

	class KernelType {
	public:
		enum class AddressSpace { Global, Constant, Local, Private, Undefined };
	private:
		core::TypePtr elementType;
		core::TypePtr locType;

		KernelType(const core::TypePtr& elementType, const core::TypePtr& locType);
	public:
		// tries to parse the given type into KernelType
		KernelType(const core::NodePtr& node);

		KernelType(const KernelType&) = default;
		KernelType(KernelType&&) = default;

		KernelType& operator=(const KernelType&) = default;
		KernelType& operator=(KernelType&&) = default;

		static core::GenericTypePtr create(const core::TypePtr& elementType, AddressSpace loc);
		const core::TypePtr& getElementType() const;
		void setElementType(const core::TypePtr& type);
		AddressSpace getAddressSpace() const;
		operator core::GenericTypePtr() const;
		core::GenericTypePtr toType() const;
	};

	bool isKernelType(const core::NodePtr& node);
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
