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

#include <set>

#include "insieme/core/forward_decls.h"

#include <iterator>

namespace insieme { namespace analysis { namespace dfa { namespace analyses {

typedef std::set<core::ExpressionAddress> AddressSet;

/** 
 *  This is an utility class which can be used to get the definitions reaching a particular usage of
 *  a variable and uses of a particular definition. This is obtained by performing
 *  reaching_definition dataflow analysis on the given IR sub-graph. 
 *
 *  Given an expression (which represents an access to a variable) an iterator through all the
 *  definitions (if the access is a use) or the uses (if the access is a def) can be obtained. 
 */
class DefUse {

	class DefUseImpl;

	std::shared_ptr<DefUseImpl> pimpl;

public:

	DefUse(const core::NodePtr& root);

	AddressSet getDefinitions(const core::ExpressionAddress& addr);
	
};

} } } } // end insieme::analysis::dfa::analyses

