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
#include "insieme/core/annotations/source_location.h"

#include <fstream>

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/tuples.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/color.h"

namespace insieme {
namespace core {
namespace annotations {

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(Location)

		typedef core::value_node_annotation<Location>::type annotation_type;

		typedef std::tuple<string, unsigned, unsigned, unsigned, unsigned, bool> encoded_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only location annotations supported!");
			const Location& loc = static_pointer_cast<annotation_type>(annotation)->getValue();
			if(loc.isShared()) {
				encoded_type value;
				std::get<5>(value) = true;
				return encoder::toIR(manager, value);
			}
			return encoder::toIR(manager, encoded_type(loc.getFile(), loc.getStart().getLine(), loc.getStart().getColumn(), loc.getEnd().getLine(),
													   loc.getEnd().getColumn(), loc.isShared()));
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert(encoder::isEncodingOf<encoded_type>(node.as<ExpressionPtr>()) && "Invalid encoding encountered!");
			encoded_type value = encoder::toValue<encoded_type>(node);
			if(std::get<5>(value)) { return std::make_shared<annotation_type>(Location::getShared()); }

			// re-build the location annotation
			return std::make_shared<annotation_type>(Location(StringValue::get(node->getNodeManager(), std::get<0>(value)),
															  TextPosition(std::get<1>(value), std::get<2>(value)),
															  TextPosition(std::get<3>(value), std::get<4>(value))));
		}

	VALUE_ANNOTATION_CONVERTER_END

// ----------------------------------------------------

bool hasAttachedLocation(const NodePtr& node) {
	return node && node->hasAttachedValue<Location>();
}

const Location& getAttachedLocation(const NodePtr& node) {
	assert_true(hasAttachedLocation(node)) << "No location annotation found at: " << node;
	return node->getAttachedValue<Location>();
}

const NodePtr& attachLocation(const NodePtr& node, const Location& location) {
	// skip null-pointer
	if(!node) { return node; }

	// check whether there is already a location
	if(hasAttachedLocation(node)) {
		// they need to be combined
		if(getAttachedLocation(node) != location) {
			// if they are not equal => it is a shared node
			node->attachValue(Location::getShared());
		}
	} else {
		// attach the new location
		node->attachValue(location);
	}

	// done
	return node;
}

const NodePtr& attachLocation(const NodePtr& node, const string& file, unsigned line, unsigned column) {
	TextPosition pos(line, column);
	return attachLocation(node, file, pos, pos);
}

const NodePtr& attachLocation(const NodePtr& node, const string& file, const TextPosition& pos) {
	return attachLocation(node, file, pos, pos);
}

const NodePtr& attachLocation(const NodePtr& node, const string& file, unsigned startLine, unsigned startColumn, unsigned endLine, unsigned endColumn) {
	return attachLocation(node, file, TextPosition(startLine, startColumn), TextPosition(endLine, endColumn));
}

const NodePtr& attachLocation(const NodePtr& node, const string& file, const TextPosition& start, const TextPosition& end) {
	if(!node) { return node; }
	auto srcFile = StringValue::get(node->getNodeManager(), file);
	return attachLocation(node, Location(srcFile, start, end));
}


// -- Location Data Structure ----------------------------------------------

std::ostream& TextPosition::printTo(std::ostream& out) const {
	return out << line << ":" << column;
}

std::ostream& Location::printTo(std::ostream& out) const {
	// if it is shared, the rest is not important
	if(shared) { return out << "-shared node-"; }

	// print location
	out << *file << "@" << start;
	if(start != end) { out << "-" << end; }
	return out;
}

void Location::cloneTo(const NodePtr& target) const {
	if(isShared()) {
		attachLocation(target, Location::getShared());
	} else {
		auto& trgMgr = target->getNodeManager();
		attachLocation(target, Location(trgMgr.get(file), start, end));
	}
}

namespace {

	LocationOpt getPredecessorLocation(const NodeAddress& node) {
		static const LocationOpt fail;

		// check whether parent is a compound stmt
		if(node.isRoot()) { return fail; }

		CompoundStmtAddress compound = node.getParentAddress().isa<CompoundStmtAddress>();
		if(!compound) { return fail; }

		int index = node.getIndex();
		for(int i = index - 1; i >= 0; i--) {
			// see whether one of the predecessors has a location annotation
			if(auto loc = getLocation(compound[i].getAddressedNode())) { return loc; }
		}

		// check whether there is a location annotation on the parent
		LocationOpt loc = getLocation(compound.getAddressedNode());
		if(loc) { return Location(loc->getFileValue(), loc->getStart(), loc->getStart()); }

		// done
		return fail;
	}

	LocationOpt getSuccessorLocation(const NodeAddress& node) {
		static const LocationOpt fail;

		// check whether parent is a compound stmt
		if(node.isRoot()) { return fail; }

		CompoundStmtAddress compound = node.getParentAddress().isa<CompoundStmtAddress>();
		if(!compound) { return fail; }

		std::size_t index = node.getIndex();
		for(std::size_t i = index + 1; i < compound.size(); i++) {
			// see whether one of the predecessors has a location annotation
			if(auto loc = getLocation(compound[i].getAddressedNode())) { return loc; }
		}

		// check whether there is a location annotation on the parent
		LocationOpt loc = getLocation(compound.getAddressedNode());
		if(loc) { return Location(loc->getFileValue(), loc->getEnd(), loc->getEnd()); }

		// done
		return fail;
	}

	LocationOpt getApproximatedLocation(const NodeAddress& node) {
		LocationOpt before = getPredecessorLocation(node);
		if(!before) { return LocationOpt(); }
		LocationOpt after = getSuccessorLocation(node);
		if(!after) { return LocationOpt(); }
		return Location(before->getFileValue(), before->getEnd(), after->getStart());
	}
}


LocationOpt getLocation(const NodeAddress& node) {
	const NodePtr& cur = node;

	// check whether current node has a location annotation
	if(hasAttachedLocation(cur)) {
		const Location& loc = getAttachedLocation(cur);
		if(!loc.isShared()) {
			return loc; // shared nodes are skipped
		}
	}

	// check whether this is the root node
	if(node.isRoot()) {
		return LocationOpt(); // could not find a location
	}

	// check whether the position could be approximated
	if(auto res = getApproximatedLocation(node)) { return res; }

	// check parent
	return getLocation(node.getParentAddress());
}

LocationOpt getLocation(const NodePtr& node) {
	return getLocation(NodeAddress(node));
}

	void prettyPrintLocation(std::ostream& out, const Location& loc, bool disableColorization /* = false */) {
		utils::Colorize colorize(!disableColorization);

		out << colorize.white() << loc << colorize.reset() << "\n";

		auto fs = std::fstream(loc.getFile());
		if(!fs.good()) {
			return;
		}

		// goto line
		for(unsigned i = 0; i < loc.getStart().getLine() - 1; i++) {
			fs.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		}

		bool singleLineLoc = loc.getStart().getLine() == loc.getEnd().getLine();

		if(!singleLineLoc) {
			out << colorize.light_purple() << ",-->    " << colorize.reset();
		}

		std::string line;
		std::getline(fs, line);
		out << line.substr(0, loc.getStart().getColumn() - 1)
		    << colorize.light_purple();

		if(singleLineLoc) {
			out << line.substr(loc.getStart().getColumn() - 1, loc.getEnd().getColumn() - loc.getStart().getColumn() + 1);
		} else {
			out << line.substr(loc.getStart().getColumn() - 1) << "\n";
			for(unsigned i = loc.getStart().getLine() + 1; i < loc.getEnd().getLine(); i++) {
				std::getline(fs, line);
				out << "|       " << line << "\n";
			}
			std::getline(fs, line);
			out << colorize.light_purple()
			    << "'-->    "
			    << line.substr(0, loc.getEnd().getColumn());
		}

		out << colorize.reset()
		    << line.substr(loc.getEnd().getColumn())
		    << "\n";

		// caret highlight
		if(singleLineLoc) {
			for(int i = 0; i < loc.getStart().getColumn() - 1; i++) {
				out << (line[i] == '\t' ? "\t" : " ");
			}
			out << colorize.light_purple() << "^";
			for(int i = loc.getStart().getColumn(); i < loc.getEnd().getColumn(); i++) {
				out << "~";
			}
			out << colorize.reset() << "\n";
		}
	}

} // end namespace annotations
} // end namespace core
} // end namespace insieme
