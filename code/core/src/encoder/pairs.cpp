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
			core::TypePtr beta  = TypeVariable::get(manager, "b");
			return GenericType::get(manager, PairExtension::PAIR_TYPE_NAME, toVector(alpha, beta));
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
			return builder.literal(pairType, "pair");
		}

	}

	string PairExtension::PAIR_TYPE_NAME = "pair";

	PairExtension::PairExtension(core::NodeManager& manager)
		: Extension(manager), pair(getPairLiteral(manager)) {}


	bool isPairType(const core::TypePtr& type) {
		if (type->getNodeType() != NT_GenericType) {
			return false;
		}

		// check generic type properties
		core::GenericTypePtr genType = type.as<GenericTypePtr>();
		return genType->getName()->getValue() == PairExtension::PAIR_TYPE_NAME &&
				genType->getTypeParameter().size() == static_cast<std::size_t>(2) &&
				genType->getIntTypeParameter().empty();
	}

	const core::TypePtr getFirstElementType(const core::TypePtr& pairType) {
		assert(isPairType(pairType) && "Not applicable to non-pair type!");
		return pairType.as<GenericTypePtr>()->getTypeParameter(0);
	}

	const core::TypePtr getSecondElementType(const core::TypePtr& pairType) {
		assert(isPairType(pairType) && "Not applicable to non-pair type!");
		return pairType.as<GenericTypePtr>()->getTypeParameter(1);
	}

	const core::TypePtr getPairType(const core::TypePtr& first, const core::TypePtr& second) {
		IRBuilder builder(first->getNodeManager());
		return builder.genericType(PairExtension::PAIR_TYPE_NAME, toVector(first, second));
	}

} // end namespace lists
} // end namespace core
} // end namespace insieme
