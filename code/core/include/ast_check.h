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

#include "ast_address.h"
#include "ast_visitor.h"
#include "enum.h"

namespace insieme {
namespace core {


class Message;
typedef std::vector<Message> MessageList;

class ASTCheck : public AddressVisitor<MessageList> {};

typedef std::shared_ptr<ASTCheck> SharedCheck;
typedef std::vector<SharedCheck> CheckList;

class CombinedASTCheck : public ASTCheck {

	CheckList checks;

public:

	CombinedASTCheck(const CheckList& checks = CheckList()) : checks(checks) {};

	void addCheck(SharedCheck& check);
	void remCheck(SharedCheck& check);

protected:

	MessageList visitNode(const NodeAddress& node);
};

template<typename N, typename C>
MessageList check(N node, C check) {
	return check.visit(node);
}

// TODO: add recursive check

/**
 * TODO: improve this class - e.g. by supporting messages referencing multiple addresses
 */
class Message {

public:

	/**
	 * An enumeration of the various types of messaged that might occure.
	 */
	enum Type {
		WARNING, 	/* < in case something has been discovered that shouldn't be used but is */
		ERROR		/* < in case a real problem has been discovered */
	};

private:

	/**
	 * The type of this message.
	 *
	 * @see Type
	 */
	Type type;

	/**
	 * The address of the node where this issue has been discovered on.
	 */
	NodeAddress address;

	/**
	 * A string message describing the problem.
	 */
	string message;

public:

	/**
	 * Creates a new message based on the given parameters.
	 *
	 * @param address the address of the node this error has been discovered on - may be the empty address.
	 * @param message a message describing the issue
	 * @param type the type of the new message (ERROR by default)
	 */
	Message(const NodeAddress& address, const string& message, Type type = ERROR)
		: type(type), address(address), message(message) {};

	/**
	 * Obtains the address of the node this message is associated to.
	 *
	 * @return the node address - might be invalid in case the error cannot be associated to a single node.
	 */
	const NodeAddress& getAddress() const {
		return address;
	}

	/**
	 * Obtains the message describing the problem.
	 *
	 * @return the message string
	 */
	const string& getMessage() const {
		return message;
	}

	/**
	 * The type of this warning.
	 */
	Type getType() const {
		return type;
	}

	bool operator==(const Message& other) const;

	bool operator!=(const Message& other) const {
		return !(*this == other);
	}
};

} // end namespace core
} // end namespace insieme

/**
 * Allows messages to be printed to an output stream.
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Message& message);
