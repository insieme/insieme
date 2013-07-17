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

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/frontend/program.h"

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include "clang/AST/Decl.h"
#pragma GCC diagnostic pop

#include "insieme/frontend/utils/indexer.h"
#include "insieme/frontend/utils/interceptor.h"

#include <set>
#include <map>
#include <stack>

namespace insieme {
namespace frontend {

namespace conversion {
class ConversionFactory;
}

namespace analysis {

/**
 * traverses the whole clang AST to find every global within code.
 * this code is needed because of 2 reasons
 * 	- we need to diferenciate the global variables declared in some translation unit (TU)
 * 	and used as extern in other TU from the variables which are purelly extern and need to be
 * 	resolved by the backend compiler in the link stage
 *  - for each global, used in any translation unit, we need to identify if is initialized with 
 *  any value, to guaranty that it will have the right initial values
 */
class GlobalVarCollector {

public:
	/** 
	 * this enum specifies the storage of the object
	 */
	enum VarStorage { VS_GLOBAL, VS_STATIC, VS_EXTERN};

	typedef std::list<const clang::VarDecl*> tInitialization;
	typedef std::list<const clang::VarDecl*>::iterator tGlobalInit_Iter;
	typedef std::list<const clang::VarDecl*>::iterator tStaticInit_Iter;

private:

	typedef std::map<std::string, VarStorage> tGlobalsMap;
	tGlobalsMap globalsMap;

	std::map<const clang::VarDecl*, std::string> staticNames;
	int staticCount;

	std::list<const clang::VarDecl*> globalInitializations;
	std::list<const clang::VarDecl*> staticInitializations;

	const insieme::frontend::utils::Interceptor& interceptor;

public:

	// functions

	GlobalVarCollector(const insieme::frontend::utils::Interceptor & interceptor_):
	staticCount(0), interceptor(interceptor_)
	{ }
	/**
	 * the functor overload searches a translation unit for globals
	 * it finds globals and updates global state
	 */
	void operator()(const TranslationUnitPtr& tu);

	/**
	 * given a variable, it finds out if is extern or has being globaly
	 * declared in any translation unit.
	 * @param name: varDecl of the variable, 
	 * @return whenever this one remains extern (to be linked)
	 */
	bool isExtern (const clang::VarDecl* var);

	/**
	 * given a variable, it finds out if is static to a function or has being globaly
	 * declared in any translation unit.
	 * @param name: varDecl of the variable, 
	 * @return whenever this one remains extern (to be linked)
	 */
	bool isStatic (const clang::VarDecl* var);

	/**
	 * usign qualified names should be enough most of the cases, but sometimes static
	 * variables produce aliases, we can have two static variables with the same name 
	 * in two different funtions
	 * @param name: varDecl of the variable, 
	 * @return a generated name for the variable
	 */
	std::string getName (const clang::VarDecl* name);

	/**
	 * incorporates one var declaration to the set, it will be ignored if not global
	 * otherwise declaration, kind of storage and initialization value will be stored
	 */
	void addVar(const clang::VarDecl* var);

	/**
	 * prints on standar output the contents of the global maps
	 */
	void dump();


	/** 
	 * iterator to access the initialization values
	 */
	class init_it{
	private:
		tGlobalsMap::iterator curr;
	public:
		init_it(const tGlobalsMap::iterator& c):
			curr(c){}

		const std::string&    name() const;
		const VarStorage      storage() const;

		init_it operator++(); 
		init_it operator++(int); 

		bool operator!=(const init_it&) const; 
	};

	init_it begin(){
		return init_it(globalsMap.begin());
	}
	init_it end(){
		return init_it(globalsMap.end());
	}


	tGlobalInit_Iter globalsInitialization_begin(){
		return globalInitializations.begin();
	}
	tGlobalInit_Iter globalsInitialization_end(){
		return globalInitializations.end();
	}

	tStaticInit_Iter staticInitialization_begin(){
		return staticInitializations.begin();
	}
	tStaticInit_Iter staticInitialization_end(){
		return staticInitializations.end();
	}

	const insieme::frontend::utils::Interceptor& getInterceptor() const{
		return interceptor;
	}
};

std::ostream& operator<< (std::ostream& out, const GlobalVarCollector::VarStorage storage);

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace

