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

#pragma once

#include <memory>

#include "insieme/utils/pointer.h"
#include "insieme/core/ast_visitor.h"

namespace insieme {
namespace backend {

namespace c_ast {

	class Node;
	typedef Ptr<Node> NodePtr;

	class CNodeManager;
	typedef std::shared_ptr<CNodeManager> SharedCNodeManager;

	class CCode;
	typedef std::shared_ptr<CCode> CCodePtr;

}

	// a forward declaration - implementation is hidden
	class ConversionContext;


	class Converter : public core::ASTVisitor<c_ast::NodePtr, core::Pointer, ConversionContext&> {

	public:

		Converter() : core::ASTVisitor<c_ast::NodePtr, core::Pointer, ConversionContext&>(true) {}

		c_ast::CCodePtr convert(const core::NodePtr& node, c_ast::SharedCNodeManager& cNodeManager);

	protected:

		c_ast::NodePtr convert(const core::NodePtr& node, ConversionContext& context) {
			return visit(node, context);
		}

		c_ast::NodePtr visitNode(const core::NodePtr& node, ConversionContext& context);

		c_ast::NodePtr visitProgram(const core::ProgramPtr& node, ConversionContext& context);


	};


} // end namespace backend
} // end namespace insieme
