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

#include "insieme/analysis/inductive/memory_location.h"

#include "insieme/core/lang/basic.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/unused.h"

namespace insieme {
namespace analysis {
namespace inductive {

	using namespace core;
	using namespace core::analysis;

	MemoryLocation::MemoryLocation(const core::ExpressionAddress& constructor)
		: constructor(constructor), dataPath(constructor.getNodeManager()) {
		__unused auto& basic = constructor.getNodeManager().getLangBasic();
		assert(
				(isCallOf(constructor.getAddressedNode(), basic.getRefVar()) ||
			     isCallOf(constructor.getAddressedNode(), basic.getRefNew()))
			   && "Memory locations can only be constructed using ref.var(..) and ref.new(..)!"
		);
	}

	MemoryLocation::MemoryLocation(const core::ExpressionAddress& constructor, const core::datapath::DataPath& dataPath)
		: constructor(constructor), dataPath(dataPath) {

		// TODO: implement sanity check!
//		auto& basic = constructor.getNodeManager().getLangBasic();
//		assert(
//				(isCallOf(constructor.getAddressedNode(), basic.getRefVar()) ||
//				 isCallOf(constructor.getAddressedNode(), basic.getRefNew()))
//			   && "Memory locations can only be constructed using ref.var(..) and ref.new(..)!"
//		);
	}



	std::ostream& MemoryLocation::printTo(std::ostream& out) const {
		return out << constructor << "/" << dataPath;
	}

	MemoryLocation MemoryLocation::member(const core::ExpressionPtr& member) const {
		// TODO: check whether this access is allowed
		return MemoryLocation(constructor, dataPath.member(member));
	}

	MemoryLocation MemoryLocation::member(const string& name) const {
		return MemoryLocation(constructor, dataPath.member(name));
	}

	MemoryLocation MemoryLocation::element(const core::ExpressionPtr& element) const {
		return MemoryLocation(constructor, dataPath.element(element));
	}

	MemoryLocation MemoryLocation::element(unsigned index) const {
		return MemoryLocation(constructor, dataPath.element(index));
	}

	MemoryLocation MemoryLocation::component(const core::LiteralPtr& component) const {
		return MemoryLocation(constructor, dataPath.component(component));
	}

	MemoryLocation MemoryLocation::component(unsigned index) const {
		return MemoryLocation(constructor, dataPath.component(index));
	}


} // end namespace inductive
} // end namespace analysis
} // end namespace insieme
