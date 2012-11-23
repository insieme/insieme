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

#include <set>

#include "insieme/utils/source_loc.h"
#include "insieme/utils/printable.h"

#include <boost/operators.hpp>

namespace insieme {
namespace utils {

class Rewriter {

public:
	class CodeModification: 
		public boost::less_than_comparable<CodeModification, CodeModification>, 
		public utils::Printable 
	{
	public:
		enum ModificationType { INSERT, REMOVE, REPLACE };
		
		CodeModification(const SourceLocation& locStart, 
						 const SourceLocation& locEnd, 
						 const std::string& code, 
						 const ModificationType& type);
		
		CodeModification(const SourceLocation& locStart, 
						 const std::string& code);


		// this operation is needed by the sort algorithm in order to order the modification hints
		// accordingly with the file name and their location.
		bool operator<(const CodeModification& other) const;

		inline bool operator==(const CodeModification& other) const {
			return fileName == other.fileName && 
				   locStart == other.locStart && 
				   locEnd == other.locEnd && 
				   type == other.type;
		}

		inline const std::string getFileName() const { return fileName; }

		inline const SourceLocation getStartLoc() const { return locStart; }
		
		inline const SourceLocation getEndLoc() const { return locEnd; }

		inline const ModificationType getType() const { return type; }

		inline const std::string getCode() const { return code; }

		static CodeModification createInsertion(const SourceLocation& locStart, 
												const std::string& insertionCode) 
		{
			return CodeModification(locStart, SourceLocation(), insertionCode, INSERT);
		}

		static CodeModification createRemoval(const SourceLocation& locStart, 
											  const SourceLocation& locEnd) 
		{
			return CodeModification(locStart, locEnd, std::string(), REMOVE);
		}

		static CodeModification createReplacement(const SourceLocation& locStart, 
												  const SourceLocation& locEnd, 
												  const std::string& replacementCode) 
		{
			return CodeModification(locStart, locEnd, replacementCode, REPLACE);
		}

		std::ostream& printTo(std::ostream& out) const;

	private:
		const std::string 		fileName;
		const SourceLocation 	locStart;
		const SourceLocation 	locEnd;
		const std::string 		code;
		const ModificationType	type;
	};

	typedef std::set<CodeModification> CodeModificationList;

	static void writeBack( const CodeModificationList& mod, const std::string& insiemeFileName = "insieme.c");
};



} // End utils namespace
} // End insieme namespace
