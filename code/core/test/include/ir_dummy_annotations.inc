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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {

	class DummyAnnotation : public NodeAnnotation {
	  public:
		static const string NAME;
		static const utils::StringKey<DummyAnnotation> DummyKey;
		int value;
		DummyAnnotation(int value) : value(value){};

		virtual const utils::AnnotationKeyPtr getKey() const {
			return &DummyKey;
		}

		const std::string& getAnnotationName() const {
			return NAME;
		}

		virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
			return false;
		}
	};

	class DummyAnnotation2 : public NodeAnnotation {
	  public:
		static const string NAME;
		static const utils::StringKey<DummyAnnotation2> DummyKey;
		int value;
		DummyAnnotation2(int value) : value(value){};

		virtual const utils::AnnotationKeyPtr getKey() const {
			return &DummyKey;
		}

		const std::string& getAnnotationName() const {
			return NAME;
		}

		virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
			assert(&*ptr == this && "Annotation pointer should reference this annotation.");
			after->addAnnotation(ptr);
			return false;
		}
	};

	// initalization of the dummy key
	const string DummyAnnotation::NAME = "DummyAnnotation";
	const utils::StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");
	const string DummyAnnotation2::NAME = "DummyAnnotation2";
	const utils::StringKey<DummyAnnotation2> DummyAnnotation2::DummyKey("DummyKey2");

} // end namespace core
} // end namespace insieme
