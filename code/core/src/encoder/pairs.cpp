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
#include "insieme/core/encoder/pairs.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace core {
namespace encoder {

	namespace {

		/**
		 * A helper function constructing a generic pair type instance.
		 */
		core::GenericTypePtr getPairType(core::NodeManager& manager) {
			core::TypePtr alpha = TypeVariable::get(manager, "a");
			core::TypePtr beta = TypeVariable::get(manager, "b");
			auto ret = GenericType::get(manager, PairExtension::PAIR_TYPE_NAME, toVector(alpha, beta));
			lang::markAsBuiltIn(ret);
			return ret;
		}

		/**
		 * A helper function constructing the pair literal used for encoding pairs.
		 */
		core::LiteralPtr getPairLiteral(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			core::GenericTypePtr pair = getPairType(manager);
			core::TypePtr alpha = pair->getTypeParameter(0);
			core::TypePtr beta = pair->getTypeParameter(1);
			core::TypePtr pairType = builder.functionType(toVector(alpha, beta), pair);
			auto ret = builder.literal(pairType, "pair");
			lang::markAsBuiltIn(ret);
			return ret;
		}
	}

	string PairExtension::PAIR_TYPE_NAME = "pair";

	PairExtension::PairExtension(core::NodeManager& manager) : Extension(manager), pair(getPairLiteral(manager)) {}


	bool isPairType(const core::TypePtr& type) {
		if(type->getNodeType() != NT_GenericType) { return false; }

		// check generic type properties
		core::GenericTypePtr genType = type.as<GenericTypePtr>();
		return genType->getName()->getValue() == PairExtension::PAIR_TYPE_NAME && genType->getTypeParameter().size() == static_cast<std::size_t>(2);
	}

	const core::TypePtr getFirstElementType(const core::TypePtr& pairType) {
		assert_true(isPairType(pairType)) << "Not applicable to non-pair type!";
		return pairType.as<GenericTypePtr>()->getTypeParameter(0);
	}

	const core::TypePtr getSecondElementType(const core::TypePtr& pairType) {
		assert_true(isPairType(pairType)) << "Not applicable to non-pair type!";
		return pairType.as<GenericTypePtr>()->getTypeParameter(1);
	}

	const core::TypePtr getPairType(const core::TypePtr& first, const core::TypePtr& second) {
		IRBuilder builder(first->getNodeManager());
		return builder.genericType(PairExtension::PAIR_TYPE_NAME, toVector(first, second));
	}

} // end namespace lists
} // end namespace core
} // end namespace insieme
