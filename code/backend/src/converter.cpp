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

#include "insieme/backend/converter.h"

#include <set>

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {

	// --------------- Conversion Context struct ---------------

	struct ConversionContext {
		c_ast::SharedCNodeManager cNodeManager;
		std::set<c_ast::CodeFragmentPtr> dependencies;
		ConversionContext(const c_ast::SharedCNodeManager& manager) : cNodeManager(manager) {}
	};


	namespace {

		c_ast::CodeFragmentPtr toCodeFragment(ConversionContext context, c_ast::NodePtr code) {
			c_ast::CodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(context.cNodeManager, code);
			fragment->addDependencies(context.dependencies);
			return fragment;
		}

	}


	// --------------- conversion operations -------------------

	c_ast::CCodePtr Converter::convert(const core::NodePtr& node, c_ast::SharedCNodeManager& cNodeManager) {

		// convert content
		ConversionContext context(cNodeManager);
		c_ast::NodePtr code = visit(node, context);

		// construct target code
		return std::make_shared<c_ast::CCode>(node, toCodeFragment(context, code));
	}

	c_ast::NodePtr Converter::visitNode(const core::NodePtr& node, ConversionContext& context) {
		// default handling of unsupported nodes => produce comment
		return context.cNodeManager->create<c_ast::Comment>("Unsupported: " + toString(node));
	}


	c_ast::NodePtr Converter::visitProgram(const core::ProgramPtr& node, ConversionContext& context) {

		// get shared C Node Manager reference
		const c_ast::SharedCNodeManager& manager = context.cNodeManager;

		// program is not producing any C code => just dependencies
		for_each(node->getEntryPoints(), [&](const core::ExpressionPtr& entryPoint) {

			// create a new context
			ConversionContext entryContext(manager);

			// create new fragment
			auto fragment = toCodeFragment(entryContext, convert(entryPoint, entryContext));

			// add converted fragment to dependency list
			context.dependencies.insert(fragment);

		});

		// create empty node (program does not represent any code)
		return 0;
	}


} // end namespace backend
} // end namespace insieme
