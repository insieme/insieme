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

#include <boost/optional/optional.hpp>

#include "insieme/utils/enum.h"

#include "insieme/core/ast_address.h"
#include "insieme/core/ast_visitor.h"

namespace insieme {
namespace core {


class Message;
typedef std::vector<Message> MessageList;
typedef boost::optional<std::vector<Message>> OptionalMessageList;

void addAll(OptionalMessageList& target, const OptionalMessageList& list);
void add(OptionalMessageList& target, const Message& msg);

class ASTCheck : public AddressVisitor<OptionalMessageList> {};

typedef std::shared_ptr<ASTCheck> CheckPtr;
typedef std::vector<CheckPtr> CheckList;

MessageList check(const NodePtr& node, const CheckPtr& check);

CheckPtr makeRecursive(const CheckPtr& check);

CheckPtr makeVisitOnce(const CheckPtr& check);

CheckPtr combine(const CheckPtr& a);

CheckPtr combine(const CheckPtr& a, const CheckPtr& b);

CheckPtr combine(const CheckPtr& a, const CheckPtr& b, const CheckPtr& c);

CheckPtr combine(const CheckList& list);

template<typename C>
inline CheckPtr make_check() {
	return std::make_shared<C>();
}

template<typename C, typename T1>
inline CheckPtr make_check(T1 t1) {
	return std::make_shared<C>(t1);
}

template<typename C, typename T1, typename T2>
inline CheckPtr make_check(T1 t1, T2 t2) {
	return std::make_shared<C>(t1, t2);
}

/**
 * TODO: improve this class - e.g. by supporting messages referencing multiple addresses
 */
class Message {

public:

	/**
	 * An enumeration of the various types of messaged that might occure.
	 */
	enum Type {
		ERROR,		/* < in case a real problem has been discovered */
		WARNING 	/* < in case something has been discovered that shouldn't be used but is */
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
	 * The error code of this message - added since it is easier to automatically process
	 * numbers than strings.
	 */
	unsigned errorCode;

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
	Message(const NodeAddress& address, unsigned errorCode, const string& message, Type type = ERROR)
		: type(type), address(address), errorCode(errorCode), message(message) {};

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
	 * Obtains the error code assigned to this message.
	 *
	 * @return the error code
	 */
	unsigned getErrorCode() const {
		return errorCode;
	}

	/**
	 * The type of this warning.
	 */
	Type getType() const {
		return type;
	}

	/**
	 * Implements the equality operator for this class. Two error messages are equal
	 * if the have the same error code, warning level and are pointing to the same node address.
	 *
	 * @param other the message to be compared to
	 * @return true if equal, false otherwise
	 */
	bool operator==(const Message& other) const;

	/**
	 * Implements the inequality operator for this class. It is simply the negation of the
	 * equality operation.
	 *
	 * @param other the message to be compared to
	 * @return true if not equal, false otherwise
	 *
	 * @see operator==
	 */
	bool operator!=(const Message& other) const {
		return !(*this == other);
	}

	/**
	 * Can be used to sort messages within a list. Messages are ordered according to their
	 * type (ERROR < WARNING) and their address.
	 *
	 * @param other the message to be compared to
	 * @return true if this one is considered smaller, false otherwise
	 */
	bool operator<(const Message& other) const;
};

} // end namespace core
} // end namespace insieme

/**
 * Allows messages to be printed to an output stream.
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Message& message);
