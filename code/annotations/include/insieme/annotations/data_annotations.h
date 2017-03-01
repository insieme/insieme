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

#pragma once


#include "insieme/utils/annotation.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {

	enum ACCESS_TYPE { null = 0, read = 1, write = 2, readWrite = 3 };

	namespace annotations {

		using namespace insieme::core;

		class Range {
			VariablePtr variable;
			ExpressionPtr lowerBoundary;
			ExpressionPtr upperBoundary;
			ACCESS_TYPE accessType;
			bool splittable;

		  public:
			// creating an empty range only needed to return an empty one on call to DataAnnotation::getRangeOf on variables for which no range is defined
			Range() {}


			Range(VariablePtr variable, ExpressionPtr lowerBoundary, ExpressionPtr upperBoundary, ACCESS_TYPE accessType = ACCESS_TYPE::readWrite,
			      bool splittable = false)
			    : variable(variable), lowerBoundary(lowerBoundary), upperBoundary(upperBoundary), accessType(accessType), splittable(splittable) {}

			VariablePtr getVariable() const {
				return variable;
			};
			ExpressionPtr getLowerBoundary() const;
			ExpressionPtr getUpperBoundary() const;
			ACCESS_TYPE getAccessType() const {
				return accessType;
			}

			void replace(core::NodeManager& mgr, NodeMap& replacements);
			void replace(core::NodeManager& mgr, core::NodePtr oldNode, core::NodePtr newNode);
			bool isSplittable() const {
				return splittable;
			};
		};


		class DataRangeAnnotation : public NodeAnnotation {
			std::vector<Range> ranges;

		  public:
			static const string NAME;
			static const utils::StringKey<DataRangeAnnotation> KEY;

			const utils::AnnotationKeyPtr getKey() const {
				return &KEY;
			}
			const std::string& getAnnotationName() const {
				return NAME;
			}

			DataRangeAnnotation() {}
			DataRangeAnnotation(std::vector<Range>& ranges) : ranges(ranges) {}

			void addRange(const Range& range) {
				ranges.push_back(range);
			}
			const std::vector<Range>& getRanges() const {
				return ranges;
			}
			Range& getRangeOf(VariablePtr var) const;

			void replace(core::NodeManager& mgr, core::VariableList& oldVars, core::VariableList& newVars);
			void replace(core::NodeManager& mgr, core::NodePtr oldNode, core::NodePtr newNode);

			virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
				// always copy the annotation
				assert_true(&*ptr == this) << "Annotation pointer should reference this annotation!";
				after->addAnnotation(ptr);
				return true;
			}
		};

		typedef std::shared_ptr<DataRangeAnnotation> DataRangeAnnotationPtr;

		class DataTransformAnnotation : public NodeAnnotation {
			unsigned tilesize;

		  public:
			static const string NAME;
			static const utils::StringKey<DataTransformAnnotation> KEY;

			const utils::AnnotationKeyPtr getKey() const {
				return &KEY;
			}
			const std::string& getAnnotationName() const {
				return NAME;
			}

			DataTransformAnnotation() : tilesize(1) {}
			DataTransformAnnotation(unsigned tileSize) : tilesize(tileSize) {}

			unsigned getTilesize() const {
				return tilesize;
			}
			unsigned isSoa() const {
				return tilesize == 0;
			}

			virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
				// always copy the annotation
				assert_true(&*ptr == this) << "Annotation pointer should reference this annotation!";
				after->addAnnotation(ptr);
				return true;
			}
		};

		typedef std::shared_ptr<DataTransformAnnotation> DataTransformAnnotationPtr;

	} // end namespace insieme
} // end namespace annotations

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::Range& range);
	std::ostream& operator<<(std::ostream& out, const insieme::annotations::DataRangeAnnotation& rAnnot);

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::DataTransformAnnotation& rAnnot);
} // end namespace std
