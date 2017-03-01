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

#include <memory>

#include <boost/optional/optional.hpp>

#include "insieme/utils/printable.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/checks/error_codes.h"

namespace insieme {
namespace core {
namespace checks {

	// ----------------- end-user interface -------------------------


	class Message;
	class MessageList;
	typedef boost::optional<MessageList> OptionalMessageList;

	class IRCheck;
	typedef std::shared_ptr<IRCheck> CheckPtr;
	typedef std::vector<CheckPtr> CheckList;

	/**
	 * Requests a check to be conducted. The given check is been applied to
	 * the given given node and a list of warnings and errors is returned.
	 *
	 * NOTE: To run a full-check considering all implemented checks you should
	 * include the full_check.h (which is including this header) and use the
	 * overloaded check(NodePtr) function.
	 *
	 * @param node the node to be tested
	 * @param check the check to be applied
	 * @return a message of identified issues
	 */
	MessageList check(const NodePtr& node, const CheckPtr& check);


	// ----------------- detailed interface -------------------------

	/**
	 * The base class of all IR-Check implementations. Internally, an IRCheck is
	 * just a special case of an IR Visitor producing an error-message list.
	 */
	class IRCheck : public IRVisitor<OptionalMessageList, core::Address> {
	  public:
		IRCheck(bool visitTypes) : IRVisitor<OptionalMessageList, core::Address>(visitTypes) {}
	};

	// ----- Check Combinators -----

	CheckPtr makeRecursive(const CheckPtr& check);

	CheckPtr makeVisitOnce(const CheckPtr& check);

	CheckPtr combine(const CheckList& list);

	template <typename... Checks>
	CheckPtr combine(const Checks&... checks) {
		return combine(toVector(checks...));
	}

	template <typename C, typename... Args>
	inline CheckPtr make_check(const Args&... args) {
		return std::make_shared<C>(args...);
	}

	/**
	 * A class utilized to address the origin of issues within semantic checks.
	 */
	class CodeLocation : public utils::Printable, public boost::equality_comparable<CodeLocation>, public boost::less_than_comparable<CodeLocation> {
	  public:
		/**
		 * The type used to model a path through annotations and nodes.
		 */
		typedef vector<pair<utils::AnnotationKeyPtr, NodeAddress>> annotation_path;

	  private:
		/**
		 * The address of the node where this issue has been discovered on.
		 */
		NodeAddress address;

		/**
		 * The address within the annotation the problem has been discovered.
		 * It is a list of keys and addresses to be followed to reach the origin
		 * of the problem, starting at the node addressed using the address field.
		 *
		 * This list is empty if the problem occurred at the node addressed
		 * by the address field (most typical case).
		 */
		annotation_path annotationPath;

	  public:
		/**
		 * A default constructor for a location based on the given address.
		 */
		CodeLocation(const NodeAddress& address) : address(address), annotationPath(){};

		/**
		 * Obtains the annotation path of the node this message is associated to.
		 *
		 * @return the annotation path - might be empty if the error is associated to a node not accessed
		 * 			via an annotation (most typical case)
		 */
		const annotation_path& getAnnotationPath() const {
			return annotationPath;
		}

		/**
		 * Obtains the address of the node causing the issue. If the annotation path is
		 * empty, the address is equivalent to the address associated to this message.
		 * Otherwise it is the address within the most deeply nested annotation path.
		 *
		 * @return the address of the actual source of the problem
		 */
		const NodeAddress& getOrigin() const {
			if(annotationPath.empty()) { return address; }
			return annotationPath.back().second;
		}

		/**
		 * Updates the origin of this code location.
		 */
		void setOrigin(const NodeAddress& addr) {
			if (annotationPath.empty()) { address = addr; }
			else annotationPath.back().second = addr;
		}

		/**
		 * Shifts the address of this location such that it becomes a location addressing a
		 * problem within a node of an annotation of an outer node.
		 *
		 * @param outer the address of the outer node
		 * @param key the key of the annotation causing this issue
		 * @return the modified message
		 */
		CodeLocation shift(const NodeAddress& outer, const utils::AnnotationKeyPtr& key) const;

		/**
		 * Implements an equality operator for this code location implementation comparing the base address
		 * and the extended annotation path.
		 */
		bool operator==(const CodeLocation& other) const {
			return address == other.address && annotationPath == other.annotationPath;
		}

		/**
		 * Implements a total order on code locations.
		 */
		bool operator<(const CodeLocation& other) const {
			// address is more important ..
			if(address != other.address) { return address < other.address; }
			// if equal, it depends on the path
			return annotationPath < other.annotationPath;
		}

		/**
		 * Allows this location to be printed to an output stream.
		 */
		std::ostream& printTo(std::ostream& out) const;
	};


	/**
	 * The class used to represent Warnings and Errors encountered while running IR checks.
	 */
	class Message : public utils::Printable {
	  public:
		/**
		 * An enumeration of the various types of messaged that might occure.
		 */
		enum Type {
			ERROR,  /* < in case a real problem has been discovered */
			WARNING /* < in case something has been discovered that shouldn't be used but is */
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
		CodeLocation location;

		/**
		 * The error code of this message - added since it is easier to automatically process
		 * numbers than strings.
		 */
		ErrorCode errorCode;

		/**
		 * A string message describing the problem.
		 */
		string message;

	  public:
		/**
		 * Creates a new message based on the given parameters.
		 *
		 * @param location the location of the node this error has been discovered on - may be the empty location.
		 * @param errorCode the error code describing the problem
		 * @param message a message describing the issue
		 * @param type the type of the new message (ERROR by default)
		 */
		Message(const CodeLocation& location, ErrorCode errorCode, const string& message, Type type = ERROR)
		    : type(type), location(location), errorCode(errorCode), message(message){};

		/**
		 * Creates a new message based on the given parameters.
		 *
		 * @param origin the address of the node this error has been discovered on - may be the empty location.
		 * @param errorCode the error code describing the problem
		 * @param message a message describing the issue
		 * @param type the type of the new message (ERROR by default)
		 */
		Message(const NodeAddress& origin, ErrorCode errorCode, const string& message, Type type = ERROR)
		    : type(type), location(origin), errorCode(errorCode), message(message){};

		/**
		 * Shifts the message of this location such that it becomes a message addressing a
		 * problem within a node of an annotation of an outer node.
		 *
		 * @param outer the address of the outer node
		 * @param key the key of the annotation causing this issue
		 * @return the modified message
		 */
		Message shiftAddress(const NodeAddress& outer, const utils::AnnotationKeyPtr& key) const;

		/**
		 * Obtains the location of the node causing this issue.
		 *
		 * @return the code location - might be invalid in case the error cannot be associated to a single node.
		 */
		const CodeLocation& getLocation() const {
			return location;
		}

		/**
		 * Obtains the address of the origin of this message (to the node causing the issue).
		 */
		const NodeAddress& getOrigin() const {
			return location.getOrigin();
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
		ErrorCode getErrorCode() const {
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

		/**
		 * Allows this message to be printed to an output stream.
		 */
		std::ostream& printTo(std::ostream& out) const;
	};


	/**
	 * A container for error and warning messages.
	 */
	class MessageList : public utils::Printable {
		/**
		 * The list of encountered errors.
		 */
		std::vector<Message> errors;

		/**
		 * The list of encountered warnings.
		 */
		std::vector<Message> warnings;

	  public:
		/**
		 * A constructor for a message list allowing to specify an arbitrary
		 * list of messages to be added to the resulting list.
		 */
		template <typename... T>
		explicit MessageList(const T&... mgs) {
			auto msgs = toVector<Message>(mgs...);
			for_each(msgs, [&](const Message& cur) { this->add(cur); });
		}

		/**
		 * Obtains a list containing all encountered errors and warnings included
		 * within this list.
		 */
		const std::vector<Message> getAll() const {
			return concatenate<Message>(errors, warnings);
		}

		/**
		 * Obtains a list containing all errors stored within this message list.
		 */
		const std::vector<Message>& getErrors() const {
			return errors;
		}

		/**
		 * Obtains a list containing all warnings stored within this message list.
		 */
		const std::vector<Message>& getWarnings() const {
			return warnings;
		}

		/**
		 * Append a new message to this message list.
		 *
		 * @param msg the message to be appended
		 */
		const void add(const Message& msg) {
			switch(msg.getType()) {
			case Message::ERROR: errors.push_back(msg); return;
			case Message::WARNING: warnings.push_back(msg); return;
			}
			assert_fail() << "Invalid message type encountered!";
		}

		/**
		 * Adds all messages from the given container to the given
		 */
		const void addAll(const MessageList& list) {
			auto addFun = [&](const Message& cur) { this->add(cur); };
			for_each(list.errors, addFun);
			for_each(list.warnings, addFun);
		}

		/**
		 * Tests whether this message list is empty.
		 */
		bool empty() const {
			return errors.empty() && warnings.empty();
		}

		/**
		 * Obtains the number of messages within this container.
		 */
		std::size_t size() const {
			return errors.size() + warnings.size();
		}

		/**
		 * Obtains direct access to one of the contained messages.
		 */
		const Message& operator[](std::size_t index) const {
			assert_le(0, index);
			assert_lt(index, size());
			if(index < errors.size()) { return errors[index]; }
			return warnings[index - errors.size()];
		}

		/**
		 * Compares this message list with another message list.
		 */
		bool operator==(const MessageList& other) const {
			return errors == other.errors && warnings == other.warnings;
		}

		/**
		 * Enables messages to be printed to some output stream.
		 */
		std::ostream& printTo(std::ostream& out) const;
	};


	/**
	 * A utility function to merge optional message lists.
	 */
	void addAll(OptionalMessageList& target, const OptionalMessageList& list);

	/**
	 * A utility function to append a message to an optional message list.
	 */
	void add(OptionalMessageList& target, const Message& msg);


} // end namspace checks
} // end namespace core
} // end namespace insieme

/**
 * Allows to use Messages in STL map containers
 */
namespace std {
	template <>
	struct hash<insieme::core::checks::Message> {
		size_t operator()(const insieme::core::checks::Message& m) const {
			std::hash<string> hasher;
			auto addr = m.getLocation().getOrigin();
			auto message = m.getMessage();
			std::stringstream s;
			s << addr << message;
			return hasher.operator()(s.str());
		}
	};
}
