/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <string>

#include <boost/optional.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_values.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_node_annotation.h"

#include "insieme/utils/printable.h"

/**
 * A header file for source-location annotations to be considered by
 * IR utilities (mostly debugging). Source locations may be attached
 * to IR nodes to link them to input codes for tracking problems or
 * reporting analysis results. Locations are also preserved by the
 * binary dump.
 */

namespace insieme {
namespace core {
namespace annotations {

	using std::string;

	// forward declaration
	class Location;     // a global localization (file + position in file)
	class TextPosition; // the location within a file (line and character position)

	/**
	 * Checks whether a location is attached to the given node.
	 *
	 * @param node the node to be tested
	 * @return true if a location is attached, false otherwise
	 */
	bool hasAttachedLocation(const NodePtr& node);

	/**
	 * Obtains a reference to the location attached to the given node. If
	 * no location has been attached the result is undefined (an assertion
	 * in debug mode).
	 *
	 * @param node the node to obtain the attached location from
	 * @return the location attached to the given node
	 */
	const Location& getAttachedLocation(const NodePtr& node);

	/**
	 * Updates the location attached to the given node and returns
	 * the handed in node.
	 *
	 * @param node the node to attach a location to
	 * @param location the location to be attached to the node
	 * @return the node handed in (for chaining).
	 */
	const NodePtr& attachLocation(const NodePtr& node, const Location& location);

	/**
	 * Updates the location attached to the given node and returns
	 * the handed in node.
	 *
	 * @param node the node to attach a location to
	 * @param file the source file to be referenced
	 * @param line the line referencing
	 * @param pos the position within the line
	 * @return the node handed in
	 */
	const NodePtr& attachLocation(const NodePtr& node, const string& file, unsigned line, unsigned column = 0);

	/**
	 * Updates the location attached to the given node and returns
	 * the handed in node.
	 *
	 * @param node the node to attach a location to
	 * @param file the source file to be referenced
	 * @param pos the position of the source code location to be covered (will be start and end)
	 * @return the node handed in
	 */
	const NodePtr& attachLocation(const NodePtr& node, const string& file, const TextPosition& pos);

	/**
	 * Updates the location attached to the given node and returns
	 * the handed in node.
	 *
	 * @param node the node to attach a location to
	 * @param file the source file to be referenced
	 * @param startLine start position line number
	 * @param startColumn start position column number
	 * @param endLine end position line number
	 * @param endColumn end position column number
	 * @return the node handed in
	 */
	const NodePtr& attachLocation(const NodePtr& node, const string& file, unsigned startLine, unsigned startColumn, unsigned endLine, unsigned endColumn);

	/**
	 * Updates the location attached to the given node and returns
	 * the handed in node.
	 *
	 * @param node the node to attach a location to
	 * @param file the source file to be referenced
	 * @param start the start of the source code location to be covered
	 * @param end the end of the source code location to be covered
	 * @return the node handed in
	 */
	const NodePtr& attachLocation(const NodePtr& node, const string& file, const TextPosition& start, const TextPosition& end);

	/**
	 * A generic version of the standard attachLocation functions.
	 *
	 * @param node the node to attach a location to
	 * @param args the arguments to be utilized to construct a location
	 * @return the given node
	 */
	template <typename T, typename... Args>
	const T& attachLocationGen(const T& node, const Args&... args) {
		const core::NodePtr& cur = node;
		attachLocation(cur, args...);
		return node;
	}


	// -- Location Data Structure ----------------------------------------------

	/**
	 * A struct utilized for modeling a position within a file.
	 */
	class TextPosition : public utils::Printable, public boost::equality_comparable<TextPosition>, public boost::less_than_comparable<TextPosition> {
		/**
		 * The line number within the file.
		 */
		unsigned line;

		/**
		 * The column within the file.
		 */
		unsigned column;

	  public:
		/**
		 * A constructor for a file location.
		 */
		TextPosition(unsigned line, unsigned column) : line(line), column(column) {}

		/**
		 * A copy constructor
		 */
		TextPosition(const TextPosition& other) = default;

		/**
		 * Obtains the line this file location is referring to.
		 */
		unsigned getLine() const {
			return line;
		}

		/**
		 * Obtains the column this file location is referring to.
		 */
		int getColumn() const {
			return column;
		}

		/**
		 * A equals operator just comparing lines and columns lexicographically.
		 */
		bool operator==(const TextPosition& other) const {
			return line == other.line && column == other.column;
		}

		/**
		 * A comparison operator just comparing lines and columns lexicographically.
		 */
		bool operator<(const TextPosition& other) const {
			return line < other.line || (line == other.line && column < other.column);
		}

		/**
		 * Makes file locations printable.
		 */
		std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * A class modeling the location of some source within a source file.
	 */
	class Location : public utils::Printable,
	                 public boost::equality_comparable<Location>,
	                 public value_annotation::cloneable,
	                 public value_annotation::migratable {
		/**
		 * The source file this location is pointing to. The reference is stored
		 * as a string-value pointer managed by the same node manager as the associated
		 * node to enable sharing of file names (to avoid high overhead since many
		 * nodes origin form the same input file).
		 */
		StringValuePtr file;

		/**
		 * The begin of the covered source location.
		 */
		TextPosition start;

		/**
		 * The end of the covered source location.
		 */
		TextPosition end;

		/**
		 * If this flag is set the location is indicating that the marked node is shared
		 * among multiple locations - no more details on those.
		 */
		bool shared;

		/**
		 * A private default constructor creating a shared location.
		 */
		Location() : file(), start(0, 0), end(0, 0), shared(true) {}

	  public:
		/**
		 * A constructor for a source-code location.
		 */
		Location(const StringValuePtr& file, const TextPosition& start, const TextPosition& end) : file(file), start(start), end(end), shared(false) {
			// assert_le(start, end);
		}

		/**
		 * A factory method for obtaining a location annotation indicating a shared code
		 * fragment.
		 */
		static Location getShared() {
			return Location();
		}

		/**
		 * Determines whether this file location is marking a shared location.
		 */
		bool isShared() const {
			return shared;
		}

		/**
		 * Obtains the file this location is referencing.
		 */
		const string& getFile() const {
			assert_false(isShared());
			return file->getValue();
		}

		/**
		 * Obtains the string value reference stored for referencing the source file.
		 */
		const StringValuePtr& getFileValue() const {
			assert_false(isShared());
			return file;
		}

		/**
		 * Obtains the start position referenced by this location.
		 */
		const TextPosition& getStart() const {
			assert_false(isShared());
			return start;
		}

		/**
		 * Obtains the end position referenced by this location.
		 */
		const TextPosition& getEnd() const {
			assert_false(isShared());
			return end;
		}

		/**
		 * An equals operator.
		 */
		bool operator==(const Location& other) const {
			if(shared && other.shared) { return true; }
			if(shared || other.shared) { return false; }
			return *file == *other.file && start == other.start && end == other.end;
		}

		/**
		 * Makes file locations printable.
		 */
		std::ostream& printTo(std::ostream& out) const;

		/**
		 * Supports the cloning of this annotation to other node managers.
		 */
		void cloneTo(const NodePtr& target) const;

		/**
		 * Supports the migration after transformations.
		 */
		bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
			// migrate annotation using standard implementation
			attachLocation(after, getAttachedLocation(before));
			return true;
		}
	};


	// -- Address Extensions ---------------------------------------------------

	// an optional location
	typedef boost::optional<Location> LocationOpt;

	/**
	 * Tries to obtain a source-code location most precisely describing the location of the given node
	 * within the original source code.
	 *
	 * @param node the node which's location should be determined
	 * @return the obtained location, if any
	 */
	LocationOpt getLocation(const NodePtr& node);

	/**
	 * Tries to obtain a source-code location most precisely describing the location of the given node
	 * within the original source code.
	 *
	 * @param node the node which's location should be determined
	 * @return the obtained location, if any
	 */
	LocationOpt getLocation(const NodeAddress& node);

	/**
	 * A generic version of the function above.
	 */
	template <typename T>
	LocationOpt getLocation(const Address<T>& addr) {
		return getLocation(addr.template as<NodeAddress>());
	}

	/**
	 * Returns a string representation of the most precise location which can be found for "n".
	 */
	template <typename T>
	static inline std::string getLocationString(const T& n) {
		auto loc = core::annotations::getLocation(n);
		return loc ? toString(*loc) : std::string("Location could not be found!");
	}

} // end namespace annotations
} // end namespace core
} // end namespace insieme
