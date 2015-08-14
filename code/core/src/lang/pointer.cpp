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

#include "insieme/core/lang/pointer.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/types/match.h"

#include "insieme/core/lang/boolean_marker.h"

namespace insieme {
namespace core {
namespace lang {


	PointerType::PointerType(const NodePtr& node) {
		// check given node type
		assert_true(node) << "Given node is null!";
		assert_true(isPointerType(node)) << "Given node " << *node << " is not a reference type!";

		// process node type
		GenericTypePtr type = node.as<GenericTypePtr>();
		*this = PointerType(type->getTypeParameter(0), isTrueMarker(type->getTypeParameter(1)), isTrueMarker(type->getTypeParameter(2)));
	}

	bool PointerType::isPointerType(const NodePtr& node) {
		auto type = node.isa<GenericTypePtr>();
		if(!type) return false;

		// simple approach: use unification
		NodeManager& mgr = node.getNodeManager();
		const PointerExtension& ext = mgr.getLangExtension<PointerExtension>();

		// unify given type with template type
		auto ref = ext.getGenPtr().as<GenericTypePtr>();
		auto sub = types::match(mgr, type, ref);
		if(!sub) return false;

		// check instantiation
		const types::Substitution& map = *sub;
		return isValidBooleanMarker(map.applyTo(ref->getTypeParameter(1))) && isValidBooleanMarker(map.applyTo(ref->getTypeParameter(2)));
	}

	GenericTypePtr PointerType::create(const TypePtr& elementType, bool _const, bool _volatile) {
		assert_true(elementType);
		return PointerType(elementType, _const, _volatile);
	}

	PointerType::operator GenericTypePtr() const {
		NodeManager& mgr = elementType.getNodeManager();
		auto& ext = mgr.getLangExtension<BooleanMarkerExtension>();

		return GenericType::get(mgr, "ref", ParentList(),
		                        toVector(elementType, (mConst ? ext.getTrue() : ext.getFalse()), (mVolatile ? ext.getTrue() : ext.getFalse())));
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme