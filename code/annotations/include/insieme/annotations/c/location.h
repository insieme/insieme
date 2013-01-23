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

#include "insieme/utils/source_loc.h"
#include "insieme/utils/annotation.h"
#include "insieme/core/ir_node.h"

namespace insieme {
namespace annotations {
namespace c {

using insieme::utils::SourceLocation;

/**
 * Annotation which contains the range within an element in the IR was defined.
 */
class CLocAnnotation : public core::NodeAnnotation {
public:
	typedef std::vector<std::string> ArgumentList;
	static const string NAME;
	static const utils::StringKey<CLocAnnotation> KEY;

	CLocAnnotation(const SourceLocation& begin, 
				   const SourceLocation& end, 
				   bool isFuncDecl=true, 
				   const ArgumentList& args = ArgumentList()
				) :
		core::NodeAnnotation(), begin(begin), end(end), isFunctionDef(isFuncDecl), args(args) {
		assert(begin.getFileName() == end.getFileName() && 
				"Source locations belongs to different files."
			);
	}

	const std::string& getAnnotationName() const {return NAME;}

	std::ostream& printTo(std::ostream& out) const;

	const SourceLocation getStartLoc() const { return begin; }
	const SourceLocation getEndLoc() const { return end; }

	bool isFunctionDefinition() const { return isFunctionDef; }
	const ArgumentList& getArgumentList() const { return args; }

	const utils::AnnotationKeyPtr getKey() const { return &KEY; }

	// Always transfer the source location annotation anytime the node is being copied to a new node
	// manager (for example during replacements)
	bool migrate(const core::NodeAnnotationPtr& ptr, 
				 const core::NodePtr& before, 
				 const core::NodePtr& after) const 
	{ 
		after->addAnnotation( ptr );
		return true; 
	}

private:
	const SourceLocation begin;
	const SourceLocation end;

	bool isFunctionDef;
	ArgumentList args;
};

} // end namespace c_info
} // end namespace annotations
} // end namespace insieme
