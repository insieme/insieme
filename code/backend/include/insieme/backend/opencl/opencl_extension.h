/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
		/**
		 * Construct an opaque NDRange
		 * @param workDim number of dimensions within the range of [1..3]
		 * @param globalOffsets list of globalOffsets
		 * @param globalWorkSize list of globalWorkSizes
		 * @param localWorkSize list of localWorkSizes
		 */
		LANG_EXT_LITERAL(MakeNDRange, "opencl_make_ndrange", "(uint<4>, list<'goffset>, list<'gsize>, list<'lsize>)->opencl_ndrange");
		/**
		 * Models a Data Range which is constructed via opencl_make_data_range
		 */
		LANG_EXT_TYPE(DataRange, "opencl_data_range");
		/**
		 * Construct an opaque DataRange
		 * @param size total number of elements of this dimension
		 * @param start start of the subrange
		 * @param end end of the subrange
		 */
		LANG_EXT_LITERAL(MakeDataRange, "opencl_make_data_range", "('size, 'start, 'end)->opencl_data_range");
		/**
		 * Models a Data Requirement which is constructed via opencl_make_data_requirement
		 */
		LANG_EXT_TYPE(DataRequirement, "opencl_data_requirement");
		/**
		 * Construct an opaque DataRequirement
		 * @param type type_lit of the enclosed expression type
		 * @param numOfRanges total number of DataRanges associated with this requirement
		 * @param rangeLambda lambda which returns a data_range for ranges between [0..numOfRanges[
		 * @param accessMode effective acccessMode as described in opencl_entities.h
		 */
		LANG_EXT_LITERAL(MakeDataRequirement, "opencl_make_data_requirement", "(type<'a>, uint<4>, opencl_data_range_fun, uint<4>)->opencl_data_requirement");
		/**
		 * Models an Optional which is constructed via opencl_make_optional
		 */
		LANG_EXT_TYPE(Optional, "opencl_optional");
		/**
		 * Construct an opaque Optional
		 * @param size
		 * @param value
		 * @param modifier
		 */
		LANG_EXT_LITERAL(MakeOptional, "opencl_make_optional", "('size, 'value, 'modifier)->opencl_optional");

		/**
		 * Represents the fundamental get_xxx functions which are required to obtain information
		 * about a work item at kernel execution time. Please take a look at the OpenCL documentation
		 * for any further explanation (of course non IR related questions)
		 */
		LANG_EXT_LITERAL(WorkDim, "opencl_get_work_dim", "()->uint<4>");
		LANG_EXT_LITERAL(GlobalSize, "opencl_get_global_size", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(GlobalId, "opencl_get_global_id", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(LocalSize, "opencl_get_local_size", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(LocalId, "opencl_get_local_id", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(NumGroups, "opencl_get_num_groups", "(uint<4>)->opencl_size_type");
		LANG_EXT_LITERAL(GroupId, "opencl_get_group_id", "(uint<4>)->opencl_size_type");

		/**
		 * Models the OpenCL address space types as IR constructs
		 */
		LANG_EXT_TYPE_WITH_NAME(MarkerGlobal, "opencl_global_marker", "opencl_global");
		LANG_EXT_TYPE_WITH_NAME(MarkerConstant, "opencl_constant_marker", "opencl_constant");
		LANG_EXT_TYPE_WITH_NAME(MarkerLocal, "opencl_local_marker", "opencl_local");
		LANG_EXT_TYPE_WITH_NAME(MarkerPrivate, "opencl_private_marker", "opencl_private");

		/**
		 * Template type of an OpenCL kernel type
		 * @param 'a element (wrapped) type
		 * @param 'loc address space location e.g. opencl_global
		 */
		LANG_EXT_TYPE_WITH_NAME(GenType, "opencl_type_template", "opencl_type<'a, 'loc>");
		/**
		 * Construct to obtain the element type of an OpenCL wrapped type
		 * @param openc_type opencl_type to peel off
		 */
		LANG_EXT_LITERAL(Peel, "opencl_peel", "(opencl_type<'a, 'loc>)->'a");
	};

	/**
	 * Represents an OpenCL kernel type using IR constructs
	 */
	class KernelType {
	public:
		enum class AddressSpace { Global, Constant, Local, Private, Undefined };
	private:
		core::TypePtr elementType;
		core::TypePtr locType;

		KernelType(const core::TypePtr& elementType, const core::TypePtr& locType);
	public:
		/**
		 * Constructs a kernel type out of the given node
		 */
		KernelType(const core::NodePtr& node);
		/**
		 * Defaulted copy-constructor
		 */
		KernelType(const KernelType&) = default;
		/**
		 * Defaulted move-constructor
		 */
		KernelType(KernelType&&) = default;
		/**
		 * Defaulted copy-assign operator
		 */
		KernelType& operator=(const KernelType&) = default;
		/**
		 * Defaulted move-assign operator
		 */
		KernelType& operator=(KernelType&&) = default;
		/**
		 * Construct an OpenCL kernel type
		 * @param elementType type which is supposed to be qualified by an address space
		 * @param addressSpace address space to use, specify 'unknown' if the location is supposed to be a type variable
		 * @return constructed generic type
		 */
		static core::GenericTypePtr create(const core::TypePtr& elementType, AddressSpace loc);
		/**
		 * Obtain a const reference to the wrapped element type
		 */
		const core::TypePtr& getElementType() const;
		/**
		 * Change the wrapped element type
		 * @param type new element type
		 */
		void setElementType(const core::TypePtr& type);
		/**
		 * Obtain the associated address space
		 * @return 'unknown' in case of a type variable, the actual address space otherwise
		 */
		AddressSpace getAddressSpace() const;
		/**
		 * Implicit conversion operator to a generic IR type
		 */
		operator core::GenericTypePtr() const;
		/**
		 * Obtain the generic IR type which represents this entity
		 */
		core::GenericTypePtr toType() const;
	};

	/**
	 * Determine whether or nor a given node represents an OpenCL kernel type
	 * @param node node to run the test on
	 * @return true iff the type represented by the given node is a kernel type, false otherwise
	 */
	bool isKernelType(const core::NodePtr& node);

	/**
	 * Construct an OpenCL kernel type
	 * @param elementType type which is supposed to be qualified by an address space
	 * @param addressSpace address space to use, specify 'unknown' if the location is supposed to be a type variable
	 * @return constructed generic type
	 */
	core::TypePtr buildKernelType(const core::TypePtr& elementType, KernelType::AddressSpace addressSpace);
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
