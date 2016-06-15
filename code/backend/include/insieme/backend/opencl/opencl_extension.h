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
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/varargs_extension.h"
#include "insieme/core/forward_decls.h"

namespace insieme {
namespace backend {
namespace opencl {
	namespace detail {
		/**
		 * Used to mark nodes which have been been already been checked by isKernelType()
		 */
		class KernelTypeMarker {
			bool valid;
		public:
			KernelTypeMarker(bool valid = false) : valid(valid) {}
			bool isValid() const { return valid; }
			bool operator==(const KernelTypeMarker& other) const { return valid == other.valid;  }
			bool operator!=(const KernelTypeMarker& other) const { return !(*this == other); }
		};
	}
	/**
	 * IR extension which encapsulates all additional constructs required to model OCL kernel code
	 */
	class OpenCLExtension : public core::lang::Extension {
		friend class core::NodeManager;
		OpenCLExtension(core::NodeManager& manager);
	  public:
		IMPORT_MODULE(core::lang::ReferenceExtension);
		/**
		 * Lambda which returns the effective NDRange
		 * @param wi pointer to a irt_work_item
		 */
		TYPE_ALIAS("opencl_ndrange_fun", "(ref<irt_wi>)->opencl_ndrange");
		/**
		 * Lambda which returns the effective data_requirement of a specific argument
		 * @param wi pointer to a irt_work_item
		 * @param ndrange effective ndrange of the current kernel execution
		 * @param argument index of the argument (0...N) for which the data_requirement shall be returned
		 */
		TYPE_ALIAS("opencl_data_requirement_fun", "(ref<irt_wi>, ref<opencl_ndrange>, uint<4>)->opencl_data_requirement");
		/**
		 * Lambda which returns the effective data_range for a specific dimension
		 * @param wi pointer to a irt_work_item
		 * @param ndrange effective ndrange of the current kernel execution
		 * @param argument index of the argument (0...N) for which the data_requirement shall be returned
		 */
		TYPE_ALIAS("opencl_data_range_fun", "(ref<irt_wi>, ref<opencl_ndrange>, uint<4>, uint<4>)->opencl_data_range");
		/**
		 * As of now, a kernel id is represented by an uint32_t type
		 */
		TYPE_ALIAS("opencl_kernel_id", "uint<4>");
		/**
		 * Models a size_t which is not available in inspire_api/basic.def
		 */
		LANG_EXT_TYPE(SizeType, "opencl_size_type");
		TYPE_ALIAS("opencl_size_type", "uint<8>");
		/**
		 * IR construct to register an ocl kernel code which will be picked-up by the op-converter
		 * @param id unique kernel id
		 * @param source string literal which represents the final ocl kernel source code
		 * @param routine string literal which names the entry point of the kernel, e.g. __insieme_fun0
		 */
		LANG_EXT_LITERAL(RegisterKernel, "opencl_register_kernel", "(opencl_kernel_id, 'source, 'routine)->unit");
		/**
		 * Execute the kernel, specified by the given id, synchronously with the given requirements, ndrange and optionals
		 * @param id id of the kernel which shall be executed
		 * @param ndrange_fun lambda which returns the effective ndrange
		 * @param requirements list of lambdas where each one returns a data_requirement for a specific ndrange and argument
		 * @param optionals var_list of optional arguments passed to the kernel at runtime
		 */
		LANG_EXT_LITERAL(ExecuteKernel, "opencl_execute_kernel", "(opencl_kernel_id, opencl_ndrange_fun, list<opencl_data_requirement_fun>, var_list)->unit");
		/**
		 * Models an NDRange which is constructed via opencl_make_ndrange
		 */
		LANG_EXT_TYPE(NDRange, "opencl_ndrange");
		LANG_EXT_LITERAL(MakeNDRange, "opencl_make_ndrange", "(uint<4>, list<'goffset>, list<'gsize>, list<'lsize>)->opencl_ndrange");
		/**
		 * Models a Data Range which is constructed via opencl_make_data_range
		 */
		LANG_EXT_TYPE(DataRange, "opencl_data_range");
		LANG_EXT_LITERAL(MakeDataRange, "opencl_make_data_range", "('size, 'start, 'end)->opencl_data_range");
		/**
		 * Models a Data Requirement which is constructed via opencl_make_data_requirement
		 */
		LANG_EXT_TYPE(DataRequirement, "opencl_data_requirement");
		LANG_EXT_LITERAL(MakeDataRequirement, "opencl_make_data_requirement", "(type<'a>, uint<4>, opencl_data_range_fun, uint<4>)->opencl_data_requirement");
		/**
		 * Models an Optional which is constructed via opencl_make_optional
		 */
		LANG_EXT_TYPE(Optional, "opencl_optional");
		LANG_EXT_LITERAL(MakeOptional, "opencl_make_optional", "('size, 'value, 'modifier)->opencl_optional");

		// extensions for the opencl kernel code
		LANG_EXT_LITERAL(WorkDim, "opencl_get_work_dim", "()->uint<4>");
		LANG_EXT_LITERAL(GlobalSize, "opencl_get_global_size", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(GlobalId, "opencl_get_global_id", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(LocalSize, "opencl_get_local_size", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(LocalId, "opencl_get_local_id", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(NumGroups, "opencl_get_num_groups", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(GroupId, "opencl_get_group_id", "(uint<4>)->opencl_size_type");

		LANG_EXT_TYPE_WITH_NAME(MarkerGlobal, "opencl_global_marker", "opencl_global");
		LANG_EXT_TYPE_WITH_NAME(MarkerConstant, "opencl_constant_marker", "opencl_constant");
		LANG_EXT_TYPE_WITH_NAME(MarkerLocal, "opencl_local_marker", "opencl_local");
		LANG_EXT_TYPE_WITH_NAME(MarkerPrivate, "opencl_private_marker", "opencl_private");

		LANG_EXT_TYPE_WITH_NAME(GenType, "opencl_type_template", "opencl_type<'a, 'loc>");
		LANG_EXT_LITERAL(Peel, "opencl_peel", "(opencl_type<'a, 'loc>)->'a");
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

	core::TypePtr buildKernelType(const core::TypePtr& elementType, KernelType::AddressSpace addressSpace);
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
