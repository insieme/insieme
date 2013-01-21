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

#include "insieme/core/dump/annotations.h"

namespace insieme {
namespace core {
namespace dump {




	// ------------- Annotation Converter Register -----------------

	AnnotationConverterRegister& AnnotationConverterRegister::getDefault() {
		static AnnotationConverterRegister registry;
		return registry;
	}

	bool AnnotationConverterRegister::registerConverter(const AnnotationConverterPtr& converter, const std::type_index& type) {
		name_index[converter->getName()] = converter;
		type_index[type] = converter;
		return true;
	}

	const AnnotationConverterPtr& AnnotationConverterRegister::getConverterFor(const std::string& name) const {
		static AnnotationConverterPtr notFound;

		// look-up converter within name-based index
		auto pos = name_index.find(name);
		if (pos != name_index.end()) {
			return pos->second;
		}
		return notFound;
	}

	const AnnotationConverterPtr& AnnotationConverterRegister::getConverterFor(const NodeAnnotationPtr& annotation) const {
		static AnnotationConverterPtr notFound;

		// look-up converter within index
		auto pos = type_index.find(typeid(*annotation));
		if (pos != type_index.end()) {
			return pos->second;
		}
		return notFound;
	}


} // end namespace dump
} // end namespace core
} // end namespace insieme
