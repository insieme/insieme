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

#include "insieme/backend/opencl/opencl_extension.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/types/match.h"

namespace insieme {
namespace backend {
namespace opencl {
	OpenCLExtension::OpenCLExtension(core::NodeManager& manager) :
		core::lang::Extension(manager)
	{ }

	namespace {
		bool isAddressSpaceMarker(const core::TypePtr& type) {
			if (!type) return false;
			// return true as we do not know it yet ..
			if (type.isa<core::TypeVariablePtr>()) return true;

			core::NodeManager& manager = type->getNodeManager();
			// grab a reference to the extension itself
			auto& oclExt = manager.getLangExtension<OpenCLExtension>();
			// check all possible types
			return oclExt.isMarkerGlobal(type) || oclExt.isMarkerConstant(type) ||
				   oclExt.isMarkerLocal(type)  || oclExt.isMarkerPrivate(type);
		}
	}

	KernelType::KernelType(const core::TypePtr& elementType, const core::TypePtr& locType) :
			elementType(elementType), locType(locType) { }

	KernelType::KernelType(const core::NodePtr& node) {
		// check given node type
		assert_true(node) << "Given node is null!";
		assert_true(isKernelType(node)) << "Given node " << *node << " is not a kernel type!";

		// extract the type
		core::GenericTypePtr type = node.isa<core::GenericTypePtr>();
		if (auto expr = node.isa<core::ExpressionPtr>()) type = expr->getType().as<core::GenericTypePtr>();

		// initialize the local instance
		*this = KernelType(type->getTypeParameter(0), type->getTypeParameter(1));
	}

	core::GenericTypePtr KernelType::create(const core::TypePtr& elementType, AddressSpace loc) {
		core::NodeManager& manager = elementType->getNodeManager();
		// grab a reference to the extension itself
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// convert the loc into a type
		core::TypePtr locType;
		switch (loc) {
		case AddressSpace::Global: locType = oclExt.getMarkerGlobal(); break;
		case AddressSpace::Constant: locType = oclExt.getMarkerConstant(); break;
		case AddressSpace::Local: locType = oclExt.getMarkerLocal(); break;
		case AddressSpace::Private: locType = oclExt.getMarkerPrivate(); break;
		case AddressSpace::Undefined: locType = core::TypeVariable::get(manager, "loc");
		}
		// and create the object itself
		return KernelType(elementType, locType).toType();
	}

	const core::TypePtr& KernelType::getElementType() const {
		return elementType;
	}

	void KernelType::setElementType(const core::TypePtr& type) {
		assert_true(type);
		elementType = type;
	}

	KernelType::AddressSpace KernelType::getAddressSpace() const {
		core::NodeManager& manager = locType->getNodeManager();
		// grab a reference to the extension itself
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		if (oclExt.isMarkerGlobal(locType)) return KernelType::AddressSpace::Global;
		if (oclExt.isMarkerConstant(locType)) return KernelType::AddressSpace::Constant;
		if (oclExt.isMarkerLocal(locType)) return KernelType::AddressSpace::Local;
		if (oclExt.isMarkerPrivate(locType)) return KernelType::AddressSpace::Private;
		if (locType.isa<core::TypeVariablePtr>()) return KernelType::AddressSpace::Undefined;

		assert_fail() << "Unknown address space: " << locType;
		return KernelType::AddressSpace::Undefined;
	}

	KernelType::operator core::GenericTypePtr() const {
		return toType();
	}

	core::GenericTypePtr KernelType::toType() const {
		core::NodeManager& manager = elementType->getNodeManager();
		return core::GenericType::get(manager, "opencl_type", core::ParentList(), toVector(elementType, locType));
	}

	bool isKernelType(const core::NodePtr& node) {
		if (!node) return false;
		// in case we are confronted with an expression, consider the resulting typedef
		if (auto expr = node.isa<core::ExpressionPtr>()) return isKernelType(expr->getType());
		// at this point we need to be sure to hold a type
		auto type = node.isa<core::GenericTypePtr>();
		if (!type) return false;
		// check for a cached annotation
		if (type->hasAttachedValue<detail::KernelTypeMarker>()) {
			return type->getAttachedValue<detail::KernelTypeMarker>().isValid();
		}
		auto result = false;
		core::NodeManager& manager = node->getNodeManager();
		// grab a reference to the extension itself
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// unify given type with template type
		auto pattern = oclExt.getGenType().as<core::GenericTypePtr>();
		auto sub = core::types::match(manager, type, pattern);
		// also check if memloc is the 2nd type parameter
		if (sub) result = isAddressSpaceMarker((*sub).applyTo(manager, pattern->getTypeParameter(1)));
		// attach the result to the type
		type->attachValue(detail::KernelTypeMarker{result});
		return result;
	}

	core::TypePtr buildKernelType(const core::TypePtr& elementType, KernelType::AddressSpace addressSpace) {
		return KernelType::create(elementType, addressSpace);
	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
