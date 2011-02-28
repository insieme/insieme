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

#include <string>
#include <sstream>
#include <vector>
#include <memory>
#include <cassert>

#include <boost/algorithm/string/replace.hpp>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace simple_backend {
	
/** A stream based on std::stringstream that keeps a level of indentation and automatically
 ** applies it at every line break.
 ** */
class CodeStream {
public:
	struct IndR { }; static const IndR indR;
	struct IndL { }; static const IndL indL;

private:
	std::stringstream ss;
	std::string indentString;

	template<typename T>
	friend CodeStream& operator<<(CodeStream& cstr, const T& param);

	template<typename T>
	void append(const T& param) {
		std::stringstream ssTmp;
		ssTmp << param;
		std::string tmp = ssTmp.str();
		boost::replace_all(tmp, "\n", std::string("\n") + indentString);
		ss << tmp;
	}
	void append(IndR) {
		indentString += "\t";
	}
	void append(IndL) {
		assert(indentString.length()>0 && "Trying to indent below level 0");
		indentString = indentString.substr(0, indentString.length()-1);
	}

public:
	CodeStream() : indentString("") { }

	std::string getString();
};

template<typename T>
CodeStream& operator<<(CodeStream& cstr, const T& param) {
	cstr.append(param);
	return cstr;
}

// Forward declarations
class CodeFragment;
typedef std::shared_ptr<CodeFragment> CodePtr;

/** A code fragment encapsulates some generated source code (in the form of a CodeStream) and an 
 ** (optional) list of code fragments it depends on.
 ** */
class CodeFragment {

	CodeStream cStream;
	std::string name;
	std::vector<CodePtr> dependencies;

	// A flag indicating whether this code fragment is only used for grouping other fragments via
	// dependencies (set to true) or to represent actual code.
	bool dummy;

public:
	CodeFragment(const std::string& name = "unnamed", bool dummy = false) : name(name), dummy(dummy) { }

	CodePtr addDependency(const std::string& name = "unnamed");
	void addDependency(const CodePtr& dep);
	const std::vector<CodePtr>& getDependencies() const { return dependencies; };
	const std::string& getName() { return name; }

	CodeStream& getCodeStream() { return cStream; }
	bool isDummy() const { return dummy; }
	void setDummy(bool value = true) { dummy = value; }
};

} // namespace simple_backend
} // namespace insieme

std::ostream& operator<<(std::ostream& os, const insieme::simple_backend::CodePtr& cp);
