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

#include <algorithm>

#include "ast_check.h"
#include "container_utils.h"

namespace insieme {
namespace core {

void CombinedASTCheck::addCheck(std::shared_ptr<ASTCheck>& check) {
	checks.push_back(check);
}

void CombinedASTCheck::remCheck(std::shared_ptr<ASTCheck>& check) {
	remove(checks.begin(), checks.end(), check);
}

MessageList CombinedASTCheck::visitNode(const NodeAddress& node) {
	// aggregate list of all error / warning messages
	MessageList list;
	for_each(checks.begin(), checks.end(), [&list, &node](const std::shared_ptr<ASTCheck>& cur) {
		addAll(list, cur->visit(node));
	});
	return list;
}


MessageList check(NodePtr& node, ASTCheck& check) {
	return check.visit(node);
}

MessageList check(NodeAddress& node, ASTCheck& check) {
	return check.visit(node);
}


bool Message::operator==(const Message& other) const {
	return type == other.type && address == other.address && message == other.message;
}

} // end namespace core
} // end namespace insieme

std::ostream& operator<<(std::ostream& out, const insieme::core::Message& message) {

	// start with type ...
	switch(message.getType()) {
	case insieme::core::Message::Type::WARNING:
		out << "WARNING: "; break;
	case insieme::core::Message::Type::ERROR:
		out << "ERROR:   "; break;
	}

	// .. continue with location ..
	out << "@ (";
	const insieme::core::NodeAddress& address = message.getAddress();
	if (address.isValid()) {
		out << address;
	} else {
		out << "<unknown>";
	}

	// .. and conclude with the message.
	out << ") - MSG: " << message.getMessage();
	return out;
}
