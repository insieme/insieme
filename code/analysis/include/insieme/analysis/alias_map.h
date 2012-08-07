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
#include <set>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {

// Forward declaration 
class Access;




struct cmp_key {

bool operator()(const insieme::core::ExpressionAddress& lhs, const insieme::core::ExpressionAddress& rhs) const;

};

/**
 * This class stores aliases for variables. 
 */
class AliasMap : public utils::Printable {

public:

	typedef std::map<core::ExpressionAddress, core::VariablePtr, cmp_key> ExprToAliasMap;

	typedef std::set<core::VariablePtr> AliasSet;

	AliasMap() { }

	core::VariablePtr createAliasFor(const core::ExpressionAddress& expr);

	void storeAlias(const core::ExpressionAddress& expr, const core::VariablePtr& var);

	core::VariablePtr lookupImmediateAlias(const core::ExpressionAddress& expr) const;

	AliasSet lookupAliases(const core::ExpressionAddress& expr) const;

	core::ExpressionAddress getMappedExpr(const core::VariablePtr& var) const;

	inline bool empty() const { 
		return aliasMap.empty(); 
	}

	std::ostream& printTo(std::ostream& out) const {
		return out << aliasMap;
	}
private:

	ExprToAliasMap aliasMap;

	void lookupAliasesImpl(const core::ExpressionAddress& expr, AliasSet& aliases) const;

};


//class AliasClass {

	//size_t class_id;
	//typedef std::set<Access> AccessSet;

	//AssessSet accesses; 

//public: 

	//AliasClass( size_t id, const Access& acc ) class_id(id), accesses( { acc } ) { }

	//inline size_t getClassId() const {
		//return class_id;
	//}

	//inline std::pair<AccessSet::iterator, bool> addAccess(const Access& acc) {
		//assert(belongsTo(acc) && "Cannot include an access which is not belonging to the equivalence class");
		//return accesses.insert( acc );
	//}

	//bool contains(const Access& acc) const {
		//return accesses.find( acc ) != accesses.end();
	//}
	
	/** 
	 * Checks whether a particular access belongs to this class 
	 */
	//bool belongsTo(const Access& acc) const {
		
		//if (accesses.empty()) { return false; }

		//bool ret = isConflicting(acc, accesses.begin());
		//for_each(accesses.begin()+1, accesses.end(), [&](const Access& cur) {
			//bool currVal = isClonflicting(acc, cur);
			//assert(currVal == ret);
		//});

		//return ret;
	//}

//};

//class AliasMap2 {

	//typedef std::map<size_t, AliasClass> AliasClassMap;

	//size_t curr_class_id;
	//AliasClassMap aliasMap;

//public:

	//AliasMap2() : curr_class_id(0) { }

	//void addAccess( const Access& acc ) {

		//auto fit = std::find_if(aliasMap.begin(), aliasMap.end(), [&](const AliasClassMap::value_type& cur) { 
				//return cur.second.belongsTo(acc);
			//});

		//if (fit != aliasMap.end()) {
			//fit->second.addAccess(acc);
			//return;
		//}
		
		//aliasMap.insert( { curr_class_id, AliasClass(curr_class_id, acc) } );
		//curr_class_id++;
	//}



//};





} // end analysis namespace 
} // end insieme namespace 
