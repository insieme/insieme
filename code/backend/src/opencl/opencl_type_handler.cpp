/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/backend/converter.h"

#include "insieme/backend/opencl/opencl_extension.h"
#include "insieme/backend/opencl/opencl_type_handler.h"
#include "insieme/backend/opencl/opencl_analysis.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"

namespace insieme {
namespace backend {
namespace opencl {

	namespace {

		const TypeInfo* handleHostType(ConversionContext& context, const core::TypePtr& type) {
			const Converter& converter = context.getConverter();
			auto& oclExt = converter.getNodeManager().getLangExtension<OpenCLExtension>();

			if (oclExt.isSizeType(type)) {
				return type_info_utils::createInfo(converter.getFragmentManager(), "size_t", "stdio.h");
			} else if(oclExt.isDataRequirement(type)) {
				// use opencl definition of the context
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_opencl_data_requirement", "irt_opencl.h");
			} else if(oclExt.isDataRange(type)) {
				// use opencl definition of the work item type
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_opencl_data_range", "irt_opencl.h");
			} else if(oclExt.isNDRange(type)) {
				// use opencl definition of the work item type
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_opencl_ndrange", "irt_opencl.h");
			}
			// it is not a special opencl type => let somebody else try
			return 0;
		}

		bool isPointerType(const core::TypePtr& type) {
			if (core::lang::isPointer(type)) return true;
			if (core::lang::isFixedSizedArray(type)) return true;
			if (core::lang::isReference(type) && core::lang::isReference(opencl::analysis::getElementType(type))) return true;
			// nope, not of interest
			return false;
		}

		const TypeInfo* handleKrnlType(ConversionContext& context, const core::TypePtr& type) {
			const Converter& converter = context.getConverter();
			static bool inRecursion = false;
			// regardless of implicit wrapping, it a user-supplied KernelType is present it has prio
			if(isKernelType(type)) {
				KernelType kernelType(type);
				// first of all we transform the element type
				inRecursion = true;
				const auto& info = converter.getTypeManager().getTypeInfo(context, kernelType.getElementType());
				inRecursion = false;
				// second step is to wrap the lvalue with an attributed type
				std::string attribute;
				switch (kernelType.getAddressSpace()) {
				case KernelType::AddressSpace::Constant:	attribute = "__constant"; break;
				case KernelType::AddressSpace::Global:		attribute = "__global"; break;
				case KernelType::AddressSpace::Local:		attribute = "__local"; break;
				case KernelType::AddressSpace::Private:
				case KernelType::AddressSpace::Undefined:	/* as private is implicit! */ break;
				}

				TypeInfo* typeInfo = type_info_utils::createInfo(0);
				// lValues of this type are now bound to their corresponding address space
				typeInfo->lValueType = c_ast::attribute(attribute, info.lValueType);
				typeInfo->rValueType = c_ast::attribute(attribute, info.rValueType);
				typeInfo->externalType = info.externalType;
				typeInfo->externalize = info.externalize;
				typeInfo->internalize = info.internalize;
				typeInfo->declaration = info.declaration;
				typeInfo->definition = info.definition;
				return typeInfo;
			} else if (!inRecursion && isPointerType(type)) {
				// first of all we transform the element type
				inRecursion = true;
				const TypeInfo& info = converter.getTypeManager().getTypeInfo(context, type);
				inRecursion = false;

				// if there would exist a dislike button, it would press it myself
				// on the other hand, this is a simple way to inject an implicit address space
				// without altering the existing type_manager with the needs which are only devoted to ocl
				const_cast<TypeInfo&>(info).lValueType = c_ast::attribute("__global", info.lValueType);
				const_cast<TypeInfo&>(info).rValueType = c_ast::attribute("__global", info.rValueType);
				return std::addressof(info);
			}
			// it is not a special opencl type => let somebody else try
			return 0;
		}
	}

	TypeHandler HostTypeHandler = &handleHostType;
	TypeHandler KrnlTypeHandler = &handleKrnlType;

} // end namespace opencl
} // end namespace backend
} // end namespace insieme
