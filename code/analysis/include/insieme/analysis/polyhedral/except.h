/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <string>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/utils/exception_utils.h"

using namespace insieme::core;

namespace insieme { namespace analysis { namespace polyhedral {

/** Expection which is thrown when a particular tree is defined to be not a static control part. This exception has to
be forwarded until the root containing this node which has to be defined as a non ScopRegion

Because this exception is only used within the implementation of the ScopRegion visitor, it is defined in the anonymous
namespace and therefore not visible outside this translation unit. */
class NotASCoP: public insieme::utils::TraceableException {
	NodePtr root;
	std::string msg;

public:
	template <class SubExTy>
	NotASCoP(const std::string& msg, const char* ex_type,
			 const char* file_name, int line_no, const SubExTy* sub_ex, const NodePtr& root):
		TraceableException(msg, ex_type, file_name, line_no, sub_ex), root(root) {
		if (!sub_ex) {
			std::ostringstream ss;
			ss << "Node: \n" << printer::PrettyPrinter(root)
			   << "\nNot a Static Control Part:\n" << msg;
			setMessage(ss.str());
		}
	}

	const NodePtr& node() const { return root; }

	virtual ~NotASCoP() throw() { }
};

class DiscardSCoPException: public NotASCoP {
	const ExpressionPtr expr;
public:
	DiscardSCoPException(const std::string& msg, const char* ex_type,
						 const char* file_name, int line_no, const std::exception* sub_ex, const NodePtr& root,
						 const ExpressionPtr& expr):
		NotASCoP(msg, ex_type, file_name, line_no, sub_ex, root), expr(expr) {}

	const ExpressionPtr& expression() const { return expr; }

	virtual ~DiscardSCoPException() throw() { }
};

}}}
