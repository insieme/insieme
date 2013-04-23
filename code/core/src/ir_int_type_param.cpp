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

#include "insieme/core/ir_int_type_param.h"
#include "insieme/core/ir_values.h"

#include "insieme/utils/hash_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {


	// ----------------------- Concrete Integer Type Parameter ------------------------

	ConcreteIntTypeParamPtr ConcreteIntTypeParam::get(NodeManager& manager, std::size_t value) {
		return manager.get(ConcreteIntTypeParam(UIntValue::get(manager, value)));
	}

	bool ConcreteIntTypeParam::operator<(const IntTypeParam& param) const {
		NodePtr other(&param);

		// variable int type parameters are smaller than all other parameters
		if (other->getNodeType() != NT_ConcreteIntTypeParam) {
			if (other->getNodeType() == NT_InfiniteIntTypeParam) {
				// it is smaller than an infinite int type param ...
				return true;
			}

			// compare based on parameters type
			return getNodeTypeInternal() < other->getNodeType();
		}

		// compare the symbol
		return getParam()->getValue() < static_cast<const ConcreteIntTypeParam&>(param).getParam()->getValue();
	}



	// ----------------------- Variable Integer Type Parameter ------------------------

	VariableIntTypeParamPtr VariableIntTypeParam::get(NodeManager& manager, char symbol) {
		return manager.get(VariableIntTypeParam(CharValue::get(manager, symbol)));
	}

	bool VariableIntTypeParam::operator<(const IntTypeParam& param) const {
		NodePtr other(&param);

		// variable int type parameters are smaller than all other parameters
		if (other->getNodeType() != NT_VariableIntTypeParam) {
			return false;
		}

		// compare the symbol
		return getSymbol()->getValue() < static_cast<const VariableIntTypeParam&>(other).getSymbol()->getValue();
	}



	// ----------------------- Infinite Integer Type Parameter ------------------------

	InfiniteIntTypeParamPtr InfiniteIntTypeParam::get(NodeManager& manager) {
		return manager.get(InfiniteIntTypeParam());
	}

	bool InfiniteIntTypeParam::operator<(const IntTypeParam& other) const {
		// nothing is larger than infinite
		return false;
	}


} // end namespace core
} // end namespace insieme
