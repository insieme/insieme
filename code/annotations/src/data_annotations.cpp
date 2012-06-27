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

void Range::replace(core::NodeManager& mgr, core::NodePtr oldNode, core::NodePtr newNode){
	std::cout << "Variable " << variable << " Replacements " << oldNode << " -> " << newNode << std::endl;
	variable = core::transform::replaceAllGen(mgr, variable, oldNode, newNode);
	std::cout << "new Variable " << variable << std::endl;
	lowerBoundary = core::transform::replaceAllGen(mgr, lowerBoundary, oldNode, newNode);
	upperBoundary = core::transform::replaceAllGen(mgr, upperBoundary, oldNode, newNode);
}

ExpressionPtr Range::getUpperBoundary() const {
	TypePtr int4 = upperBoundary->getNodeManager().getLangBasic().getInt4();
	if(upperBoundary->getType() == int4)
		return upperBoundary;
	core::IRBuilder builder(upperBoundary->getNodeManager());
	return builder.castExpr(int4, upperBoundary);
}

ExpressionPtr Range::getLowerBoundary() const {
	TypePtr int4 = lowerBoundary->getNodeManager().getLangBasic().getInt4();
	if(lowerBoundary->getType() == upperBoundary->getType())
		return lowerBoundary;
	core::IRBuilder builder(lowerBoundary->getNodeManager());
	return builder.castExpr(int4, lowerBoundary);
}

void DataRangeAnnotation::replace(core::NodeManager& mgr, core::VariableList& oldVars, core::VariableList& newVars) {
	// construct replacement map
	core::NodeMap replacements;
	//TODO add replacement for local and global range
	for(size_t i = 0; i < oldVars.size(); ++i) {
		replacements[oldVars.at(i)] = newVars.at(i);
	}

	for_each(ranges, [&](Range& range) {
		range.replace(mgr, replacements);
	});
}

void DataRangeAnnotation::replace(core::NodeManager& mgr, core::NodePtr oldNode, core::NodePtr newNode){
	for_each(ranges, [&](Range& range) {
		range.replace(mgr, oldNode, newNode);
	});
}

} // namespace annotations
} // namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::Range& range) {
		if(range.isSplittable())
			out << "+";
		else
			out << "-";
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
			out << "\t" << *I  << std::endl;
		}

		return out;
	}

} // end namespace std
