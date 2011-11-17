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

#pragma once

#include <map>
#include <string>

#include "insieme/transform/parameter.h"
#include "insieme/transform/transformation.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace transform {

	using std::string;

	class TransformationCatalog;
	typedef std::shared_ptr<TransformationCatalog> TransformationCatalogPtr;

	class TransformationType;
	typedef std::shared_ptr<TransformationType> TransformationTypePtr;


	TransformationCatalogPtr getStandardCatalog();

	class TransformationType {

		string name;

		string description;

		parameter::ParameterPtr parameterInfo;

	public:

		TransformationType(const string& name, const string& desc, const parameter::ParameterPtr& paramInfo = parameter::no_parameters)
			: name(name), description(desc), parameterInfo(paramInfo) {};

		virtual ~TransformationType() {}

		TransformationPtr createTransformation(const parameter::Value& value = parameter::makeComposedValue()) const {
			if (getParameterInfo()->isValid(value)) {
				return buildTransformation(value);
			}
			throw std::invalid_argument("Handed in value not valid for this type of transformation!");
		}

		const string& getName() const {
			return name;
		}

		const string& getDescription() const {
			return description;
		}

		const parameter::ParameterPtr getParameterInfo() const {
			return parameterInfo;
		}

	protected:

		virtual TransformationPtr buildTransformation(const parameter::Value& value) const = 0;

	};


	class TransformationCatalog {

		std::map<string, TransformationTypePtr> catalog;

	public:

		void add(const TransformationTypePtr& newType) {
			catalog.insert(std::make_pair(newType->getName(), newType));
		}

		template<
			typename T, typename ...P,
			typename boost::enable_if<boost::is_base_of<TransformationType,T>,int>::type = 0
		>
		void add(P ... params) { add(std::make_shared<T>(params...)); }


		TransformationTypePtr getTransformationType(const string& name) const {
			auto pos = catalog.find(name);
			if (pos != catalog.end()) {
				return pos->second;
			}
			return TransformationTypePtr();
		}

		TransformationPtr createTransformation(const string& name, const parameter::Value& value = parameter::makeComposedValue()) const {
			TransformationTypePtr type = getTransformationType(name);
			assert(type && "Unknown transformation type requested!");
			return type->createTransformation(value);
		}

		const std::map<string, TransformationTypePtr>& getRegister() const {
			return catalog;
		}

		vector<string> getAllTransformationNames() const {
			vector<string> res;
			projectToFirst(catalog, res);
			return res;
		}

		vector<TransformationTypePtr> getAllTransformations() const {
			return projectToSecond(catalog);
		}

	};








} // end namespace transform
} // end namespace insieme
