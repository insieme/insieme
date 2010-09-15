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

#include <unordered_map>
#include <boost/functional/hash.hpp>

#include "annotation.h"
#include "identifier.h"

namespace insieme {
namespace c_info {

/** Annotation class intended to keep the name of C types and functions.
 ** Should be used to annotate *pointers* when exactly one name is required,
 ** for example with structs, unions or functions.
 ** */
class CNameAnnotation : public core::Annotation {
	const core::Identifier ident;

public:
	const static core::StringKey<CNameAnnotation> key;

	CNameAnnotation(const core::Identifier& ident) : core::Annotation(), ident(ident) { }
	CNameAnnotation(const std::string& name) : core::Annotation(), ident(name) { }

	const core::Identifier& getIdent() const { return ident; }
	const std::string& getName() const { return ident.getName(); }

	const core::AnnotationKey* getKey() const { return &key; }
};

/** Annotation class intended to keep the name(s) of recursive C types and functions.
 ** Should be used to annotate *pointers* to recursive types, mapping each type variable
 ** identifier to the corresponding C name.
 ** */
class CRecNameAnnotation : public core::Annotation {
public:
	typedef std::unordered_map<core::Identifier, core::Identifier, boost::hash<core::Identifier>> IdentMap;

private:
	IdentMap identMap;

public:
	const static core::StringKey<CRecNameAnnotation> key;

	CRecNameAnnotation() : core::Annotation() { }

	IdentMap& getIdentMap() { return identMap; }

	void addIdent(const core::Identifier& recVarName, const core::Identifier& cName);
	const core::Identifier& getIdent(const core::Identifier& recVarName);
	const std::string& getName(const core::Identifier& recVarName);

	const core::AnnotationKey* getKey() const { return &key; }
};

} // namespace c_info
} // namespace insieme
