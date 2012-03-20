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

#include "insieme/core/datapath/datapath.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

namespace insieme {
namespace core {
namespace datapath {


	DataPathBuilder::DataPathBuilder(NodeManager& manager)
		: manager(manager), path(manager.getLangBasic().getDataPathRoot()) {}


	DataPathBuilder& DataPathBuilder::member(const ExpressionPtr& member) {
		auto& basic = manager.getLangBasic();
		assert(basic.isIdentifier(member->getType()) && "Member identifier has to be an identifier!");
		path = IRBuilder(manager).callExpr(basic.getDataPath(), basic.getDataPathMember(), path, member);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::member(const string& name) {
		return member(IRBuilder(manager).getIdentifierLiteral(name));
	}

	DataPathBuilder& DataPathBuilder::element(const ExpressionPtr& element) {
		auto& basic = manager.getLangBasic();
		assert(basic.isUnsignedInt(element->getType()) && "Index has to be an unsigned integer!");
		path = IRBuilder(manager).callExpr(basic.getDataPath(), basic.getDataPathElement(), path, element);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::element(unsigned index) {
		return element(IRBuilder(manager).uintLit(index).as<ExpressionPtr>());
	}

	DataPathBuilder& DataPathBuilder::component(const LiteralPtr& component) {
		auto& basic = manager.getLangBasic();
		assert(basic.isUnsignedInt(component->getType()) && "Index has to be an unsigned integer!");
		path = IRBuilder(manager).callExpr(basic.getDataPath(), basic.getDataPathComponent(), path, component);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::component(unsigned index) {
		return component(IRBuilder(manager).uintLit(index));
	}

} // end namespace datapath
} // end namespace core
} // end namespace insieme
