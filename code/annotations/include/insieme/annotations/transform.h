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
 */

#pragma once

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_annotation.h"

namespace insieme {
namespace annotations {

	using namespace insieme;

	struct TransformationHint {
		typedef std::vector<unsigned> ValueVect;

		enum Type {
			LOOP_INTERCHANGE,
			LOOP_STRIP,
			LOOP_TILE,
			LOOP_UNROLL,
			LOOP_FUSE,
			LOOP_SPLIT,
			LOOP_STAMP,
			LOOP_RESCHEDULE,
			LOOP_PARALLELIZE,

			REGION_STRIP,

			// for recursive functions
			REC_FUN_UNROLL

			// Add here new transformations
		};

		TransformationHint(const Type& type, const ValueVect& values) : type(type), values(values) {}

		template <class... Args>
		TransformationHint(const Type& type, const Args&... args)
		    : type(type), values({args...}) {}

		const ValueVect& getValues() const {
			return values;
		}
		const Type& getType() const {
			return type;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "Transformation Hint '" << type << "' (" << toString(values) << ")";
		}

	  private:
		Type type;
		ValueVect values;
	};

	class TransformAnnotation : public utils::CompoundAnnotation<TransformationHint, core::NodeAnnotation> {
	  public:
		static const string NAME;
		static const utils::StringKey<TransformAnnotation> KEY;

		TransformAnnotation() : utils::CompoundAnnotation<TransformationHint, core::NodeAnnotation>() {}

		const utils::AnnotationKeyPtr getKey() const {
			return &KEY;
		}
		const std::string& getAnnotationName() const {
			return NAME;
		}

		const std::string toString() const;

		virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
			// always copy the annotation
			assert_true(&*ptr == this) << "Annotation pointer should reference this annotation!";
			after->addAnnotation(ptr);
			return true;
		}

	  private:
		AnnotationList annotationList;
	};

} // end annotations namespace
} // end insieme namespace
