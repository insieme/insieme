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

#include <gtest/gtest.h>

#include "insieme/transform/catalog.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace transform {

	// --------- Utilities ----------------------------------

	class DummyTransformation : public Transformation {

	public:

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return true;
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const {
			return target;
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			return true;
		}

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << "DummyTransform";
		}

		virtual bool operator==(const Transformation& other) const {
			return dynamic_cast<const DummyTransformation*>(&other);
		}

	};

	class DummyTransformationType : public TransformationType {

	public:

		DummyTransformationType() : TransformationType("DummyTransform1", "A simple dummy transformation doing nothing!") {}

		virtual TransformationPtr buildTransformation(const parameter::Value& value) const {
			return std::make_shared<DummyTransformation>();
		}

	};


	class DummyTransformation2 : public Transformation {

		int dummyParameter;

	public:

		DummyTransformation2(int param) : dummyParameter(param) {}

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return true;
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const {
			return core::IRBuilder(target->getNodeManager()).intLit(dummyParameter);
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

	class DummyTransformation2Type : public TransformationType {

	public:

		DummyTransformation2Type() : TransformationType("DummyTransform2", "Another dummy transformation messing everything up!", parameter::atom<int>()) {}

		virtual TransformationPtr buildTransformation(const parameter::Value& value) const {
			return std::make_shared<DummyTransformation2>(parameter::getValue<int>(value));
		}
	};

	// -------------------------------------------------------

	// --------- Test cases ----------------------------------


	Catalog getDummyCatalog() {
		Catalog res;
		res.add<DummyTransformationType>();
		res.add<DummyTransformation2Type>();
		return res;
	}


	TEST(Catalog, Basic) {

		// create the catalog
		Catalog catalog = getDummyCatalog();
		EXPECT_FALSE(catalog.getRegister().empty());

		// list all transformations
		EXPECT_EQ("[DummyTransform1,DummyTransform2]", toString(catalog.getAllTransformationNames()));

		// create transformations using the catalog
		TransformationPtr trans1 = catalog.createTransformation("DummyTransform1");
		EXPECT_TRUE(!!trans1);

		TransformationPtr trans2 = catalog.createTransformation("DummyTransform2", parameter::makeValue(123));
		EXPECT_TRUE(!!trans2);

		EXPECT_THROW(catalog.createTransformation("DummyTransform1", parameter::makeValue(123)), std::invalid_argument);
		EXPECT_THROW(catalog.createTransformation("DummyTransform2", parameter::emptyValue), std::invalid_argument);
	}








} // end namespace transform
} // end namespace insieme


