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

#include <gtest/gtest.h>

#include "insieme/transform/catalog.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace transform {

	// --------- Utilities ----------------------------------

	class DummyTransformation1 : public Transformation {
	  public:
		DummyTransformation1(const parameter::Value& value);

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return true;
		}

		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			return target;
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			return true;
		}

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << "DummyTransform";
		}

		virtual bool operator==(const Transformation& other) const {
			return dynamic_cast<const DummyTransformation1*>(&other);
		}
	};

	TRANSFORMATION_TYPE(DummyTransformation1, "A simple dummy transformation doing nothing!", parameter::no_parameters());

	DummyTransformation1::DummyTransformation1(const parameter::Value& value) : Transformation(DummyTransformation1Type::getInstance(), value) {}

	class DummyTransformation2 : public Transformation {
		int dummyParameter;

	  public:
		DummyTransformation2(int param);
		DummyTransformation2(const parameter::Value& value);

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return true;
		}

		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			auto res = core::IRBuilder(target->getNodeManager()).intLit(dummyParameter);
			return core::transform::replaceAddress(target->getNodeManager(), target, res);
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			return true;
		}

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << "DummyTransform2";
		}

		virtual bool operator==(const Transformation& other) const {
			return dynamic_cast<const DummyTransformation2*>(&other);
		}
	};

	TRANSFORMATION_TYPE(DummyTransformation2, "Another dummy transformation messing everything up!", parameter::atom<int>());

	DummyTransformation2::DummyTransformation2(const parameter::Value& value)
	    : Transformation(DummyTransformation2Type::getInstance(), value), dummyParameter(parameter::getValue<int>(value)) {}

	DummyTransformation2::DummyTransformation2(int param)
	    : Transformation(DummyTransformation2Type::getInstance(), parameter::makeValue(param)), dummyParameter(param) {}

	// -------------------------------------------------------

	// --------- Test cases ----------------------------------


	Catalog getDummyCatalog() {
		Catalog res;
		res.add(DummyTransformation1Type::getInstance());
		res.add(DummyTransformation2Type::getInstance());
		return res;
	}


	TEST(Catalog, Basic) {
		// create the catalog
		Catalog catalog = getDummyCatalog();
		EXPECT_FALSE(catalog.getRegister().empty());

		// list all transformations
		EXPECT_EQ("[DummyTransformation1,DummyTransformation2]", toString(catalog.getAllTransformationNames()));

		// create transformations using the catalog
		TransformationPtr trans1 = catalog.createTransformation("DummyTransformation1");
		EXPECT_TRUE(!!trans1);

		TransformationPtr trans2 = catalog.createTransformation("DummyTransformation2", parameter::makeValue(123));
		EXPECT_TRUE(!!trans2);

		EXPECT_THROW(catalog.createTransformation("DummyTransformation1", parameter::makeValue(123)), std::invalid_argument);
		EXPECT_THROW(catalog.createTransformation("DummyTransformation2", parameter::emptyValue), std::invalid_argument);
	}


} // end namespace transform
} // end namespace insieme
