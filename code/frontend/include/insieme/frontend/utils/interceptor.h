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
#include <boost/regex.hpp>
#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/utils/map_utils.h"

#include "insieme/frontend/utils/header_tagger.h"

// Forward declarations
namespace clang {
class Type;
class FunctionDecl;
} // End clang namespace

namespace insieme {
namespace frontend { 

namespace conversion {
class Converter;
}

namespace utils {

class Interceptor {
public:
	Interceptor(
			insieme::core::NodeManager& mgr,
			const HeaderTagger& headerTagger,
			const std::set<std::string>& interceptSet)
		: builder(mgr), headerTagger(headerTagger),
		
  		  // by default intercept std:: and __gnu_cxx:: namespaces
		  // __gnu_cxx is needed for the iterator of std::vector for example
		  toIntercept(interceptSet), 
	
		  //joins all the strings in the toIntercept-set to one big regEx
		  rx("("+toString(join(")|(", toIntercept))+")")
	{	
	}

	void loadConfigSet(std::set<std::string> toIntercept);	
	
	bool isIntercepted(const string& name) const;
	bool isIntercepted(const clang::Type* type) const;
	bool isIntercepted(const clang::FunctionDecl* decl) const;

	insieme::core::TypePtr intercept(const clang::Type* type, insieme::frontend::conversion::Converter& convFact) const ;
	insieme::core::ExpressionPtr intercept(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& convFact, const bool explicitTemplateArgs=false) const;

	const HeaderTagger& getHeaderTagger () const {
		return headerTagger;
	} 

private:
	insieme::core::IRBuilder builder;

	/**
	 * the header tagger to use when detecting headers
	 */
	const HeaderTagger& headerTagger;

	/**
	 * set of strings representing the regEx to be intercepted
	 */
	std::set<std::string> toIntercept;

	/**
	 * the combined regex from the toIntercept-set
	 */
	boost::regex rx;
};

} // end utils namespace
} // end frontend namespace
} // end insieme namespace
