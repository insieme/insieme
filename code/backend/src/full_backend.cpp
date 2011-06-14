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

#include "insieme/backend/full_backend.h"

#include <sstream>

#include <cstdlib>
#include <iostream>

#include "insieme/core/ast_node.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {


	namespace {
		class TextFragment : public c_ast::CodeFragment {
			string text;
		public:
			TextFragment(const string& text) : text(text) {};
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << text;
			}
		};
	}

	FullBackendPtr FullBackend::getDefault() {
		return std::make_shared<FullBackend>();
	}

	TargetCodePtr FullBackend::convert(const core::NodePtr& code) const {

		// create a C Node Manager
		c_ast::SharedCNodeManager manager = c_ast::CNodeManager::createShared();

		// create Converter instance
		Converter converter;

		// convert IR node target code
		return converter.convert(code, manager);
	}

} // end namespace backend
} // end namespace insieme



