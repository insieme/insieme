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
 */

#include "insieme/core/lang/cpp_std.h"

#include "insieme/utils/name_mangling.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/types/match.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"

namespace insieme {
namespace core {
namespace lang {


	bool isStdString(const NodePtr& node) {

		// get type
		auto type = analysis::getType(node);
		if (!type) return false;

		// a standard string has the corresponding annotation
		if (!annotations::hasAttachedName(type)) return false;

		// check the annotated name
		const auto& name = annotations::getAttachedName(type);
		return name == "std::string" || name == "std::__cxx11::basic_string";
	}

	// --------------------- std::pair ----------------------------

	bool isStdPair(const NodePtr& node) {

		// extract the type
		auto type = analysis::getType(node).isa<GenericTypePtr>();
		if (!type) return false;

		// check type properties

		// no parents
		if (!type->getParents()->empty()) return false;

		// two parameters
		if (type->getTypeParameter()->size() != 2) return false;

		// check that both parameters are qualified references
		for(const auto& cur : type->getTypeParameter()) {
			if (!core::lang::isQualifiedReference(cur)) return false;
		}

		// finally, check the name
		if (insieme::utils::demangle(type->getFamilyName()) != "std::pair") return false;

		// everything checks out
		return true;
	}

	TypePtr getStdPairFirstType(const NodePtr& node) {
		// check that it is indeed a pair
		auto type = analysis::getType(node);
		if (!isStdPair(type)) return nullptr;
		// extract the first element type
		return core::analysis::getReferencedType(type.as<GenericTypePtr>()->getTypeParameter(0));
	}

	TypePtr getStdPairSecondType(const NodePtr& node) {
		// check that it is indeed a pair
		auto type = analysis::getType(node);
		if (!isStdPair(type)) return nullptr;
		// extract the second element type
		return core::analysis::getReferencedType(type.as<GenericTypePtr>()->getTypeParameter(1));
	}



	// --------------------- std::array ----------------------------

	bool isStdArray(const NodePtr& node) {

		// extract the type
		auto type = analysis::getType(node).isa<GenericTypePtr>();
		if (!type) return false;

		// check type properties

		// no parents
		if (!type->getParents()->empty()) return false;

		// two parameters
		if (type->getTypeParameter()->size() != 2) return false;

		// check that the first parameter is a qualified reference
		if (!core::lang::isQualifiedReference(type->getTypeParameter(0))) return false;

		// the second must be a numeric type
		if (!type->getTypeParameter(1).isa<NumericTypePtr>()) return false;

		// finally, check the name
		if (insieme::utils::demangle(type->getFamilyName()) != "std::array") return false;

		// everything checks out
		return true;
	}


	TypePtr getStdArrayElementType(const NodePtr& node) {
		// check that it is indeed an array
		auto type = analysis::getType(node);
		if (!isStdArray(type)) return nullptr;
		// extract the element type
		return core::analysis::getReferencedType(type.as<GenericTypePtr>()->getTypeParameter(0));
	}


	// --------------------- std::vector ---------------------------

	bool isStdVector(const NodePtr& node) {

		// extract the type
		auto type = analysis::getType(node).isa<GenericTypePtr>();
		if (!type) return false;

		// check type properties

		// no parents
		if (!type->getParents()->empty()) return false;

		// two parameters
		if (type->getTypeParameter()->size() != 2) return false;

		// check that both parameters are qualified references
		for(const auto& cur : type->getTypeParameter()) {
			if (!core::lang::isQualifiedReference(cur)) return false;
		}

		// finally, check the name
		if (insieme::utils::demangle(type->getFamilyName()) != "std::vector") return false;

		// everything checks out
		return true;
	}

	TypePtr getStdVectorElementType(const NodePtr& node) {
		// check that it is indeed a vector
		auto type = analysis::getType(node);
		if (!isStdVector(type)) return nullptr;
		// extract the element type
		return core::analysis::getReferencedType(type.as<GenericTypePtr>()->getTypeParameter(0));
	}



	// --------------------- std::map ---------------------------

	bool isStdMap(const NodePtr& node) {

		// extract the type
		auto type = analysis::getType(node).isa<GenericTypePtr>();
		if (!type) return false;

		// check type properties

		// no parents
		if (!type->getParents()->empty()) return false;

		// two parameters
		if (type->getTypeParameter()->size() != 4) return false;

		// check that both parameters are qualified references
		for(const auto& cur : type->getTypeParameter()) {
			if (!core::lang::isQualifiedReference(cur)) return false;
		}

		// finally, check the name
		if (insieme::utils::demangle(type->getFamilyName()) != "std::map") return false;

		// everything checks out
		return true;
	}

	TypePtr getStdMapKeyType(const NodePtr& node) {
		// check that it is indeed a vector
		auto type = analysis::getType(node);
		if (!isStdMap(type)) return nullptr;
		// extract the element type
		return core::analysis::getReferencedType(type.as<GenericTypePtr>()->getTypeParameter(0));
	}

	TypePtr getStdMapValueType(const NodePtr& node) {
		// check that it is indeed a vector
		auto type = analysis::getType(node);
		if (!isStdMap(type)) return nullptr;
		// extract the element type
		return core::analysis::getReferencedType(type.as<GenericTypePtr>()->getTypeParameter(1));
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
