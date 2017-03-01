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
/*
 * data_annotations.h
 *
 *  Created on: Dec 6, 2011
 *      Author: klaus
 */


#include "insieme/annotations/data_annotations.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace annotations {

	const string DataRangeAnnotation::NAME = "DataRangeAnnotation";
	const utils::StringKey<DataRangeAnnotation> DataRangeAnnotation::KEY("Range");

	void Range::replace(core::NodeManager& mgr, core::NodeMap& replacements) {
		variable = core::transform::replaceAllGen(mgr, variable, replacements);
		lowerBoundary = core::transform::replaceAllGen(mgr, lowerBoundary, replacements);
		upperBoundary = core::transform::replaceAllGen(mgr, upperBoundary, replacements);
	}

	void Range::replace(core::NodeManager& mgr, core::NodePtr oldNode, core::NodePtr newNode) {
		std::cout << "Variable " << variable << " Replacements " << oldNode << " -> " << newNode << std::endl;
		variable = core::transform::replaceAllGen(mgr, variable, oldNode, newNode);
		std::cout << "new Variable " << variable << std::endl;
		lowerBoundary = core::transform::replaceAllGen(mgr, lowerBoundary, oldNode, newNode);
		upperBoundary = core::transform::replaceAllGen(mgr, upperBoundary, oldNode, newNode);
	}

	ExpressionPtr Range::getUpperBoundary() const {
		TypePtr int4 = upperBoundary->getNodeManager().getLangBasic().getInt4();
		if(upperBoundary->getType() == int4) { return upperBoundary; }
		core::IRBuilder builder(upperBoundary->getNodeManager());
		return builder.castExpr(int4, upperBoundary);
	}

	ExpressionPtr Range::getLowerBoundary() const {
		TypePtr int4 = lowerBoundary->getNodeManager().getLangBasic().getInt4();
		if(lowerBoundary->getType() == upperBoundary->getType()) { return lowerBoundary; }
		core::IRBuilder builder(lowerBoundary->getNodeManager());
		return builder.castExpr(int4, lowerBoundary);
	}

	void DataRangeAnnotation::replace(core::NodeManager& mgr, core::VariableList& oldVars, core::VariableList& newVars) {
		// construct replacement map
		core::NodeMap replacements;
		// TODO add replacement for local and global range
		for(size_t i = 0; i < oldVars.size(); ++i) {
			replacements[oldVars.at(i)] = newVars.at(i);
		}

		for_each(ranges, [&](Range& range) { range.replace(mgr, replacements); });
	}

	void DataRangeAnnotation::replace(core::NodeManager& mgr, core::NodePtr oldNode, core::NodePtr newNode) {
		for_each(ranges, [&](Range& range) { range.replace(mgr, oldNode, newNode); });
	}


	const string DataTransformAnnotation::NAME = "DataTransformAnnotation";
	const utils::StringKey<DataTransformAnnotation> DataTransformAnnotation::KEY("Transform");

} // namespace annotations
} // namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::Range& range) {
		if(range.isSplittable()) {
			out << "+";
		} else {
			out << "-";
		}
		switch(range.getAccessType()) {
		case insieme::ACCESS_TYPE::read: out << "R  "; break;
		case insieme::ACCESS_TYPE::write: out << "W  "; break;
		case insieme::ACCESS_TYPE::readWrite: out << "RW "; break;
		default: out << "X "; break;
		}
		out << *range.getVariable() << " = " << *range.getLowerBoundary() << " : " << *range.getUpperBoundary() << " ";

		return out;
	}

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::DataRangeAnnotation& rAnnot) {
		out << "DatarangeAnnotation:\n";
		for(auto I = rAnnot.getRanges().begin(); I != rAnnot.getRanges().end(); ++I) {
			out << "\t" << *I << std::endl;
		}

		return out;
	}

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::DataTransformAnnotation& tAnnot) {
		out << "DataTransformAnnotation:\n";

		out << "\t" << (tAnnot.isSoa() == 0 ? string("SOA") : (format("Tilesize: %u", tAnnot.getTilesize()))) << std::endl;

		return out;
	}

} // end namespace std
