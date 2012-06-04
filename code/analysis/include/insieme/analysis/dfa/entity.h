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

#include <vector>
#include <map>

#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"
#include "insieme/core/ir_expressions.h"

namespace insieme { 
namespace analysis { 

class CFG;

namespace cfg {
class Block;
} // end cfg namespace 
	
namespace dfa {

// Wrapper class for std::vector so that can be utilized as template template 
// parameter
template <class T>
class Vector : public std::vector<T> { };

// Class wrapper for std::map so that can be utilized as template template
// parameter
template <class Key, class Value>
class Map : public std::map<Key, Value> { };

/** 
 * An entity is the unit of information examined by a dataflow problem. 
 *
 * An entity could be of various form: expressions, variables, control flow blocks, etc... 
 *
 * Now always an entity represent a physical node of the IR, the concept is more abstract. 
 * For this reason each entity should describe the domain of values which can be assumed by 
 * an instance of an entity and how this entity is extracted starting from a generic CFG. 
 */
template <class DomTy, template <typename> class ContainerTy = Vector>
struct EntityMapper {

	typedef DomTy 	DomainType;
	typedef ContainerTy<DomainType> 		EntityVec;

	/**
	 * Extract the list of entities existing in the given CFG. 
	 */
	virtual EntityVec extract(const CFG& cfg) const = 0;
	
	virtual ~EntityMapper() { }
};

template <class DomTy, class CodomTy, 
		 template <typename> class ContainerTy = Vector,
		 template <typename,typename> class MapTy = Map
>
struct EntityAssigner {

	typedef DomTy 	DomainType;
	typedef CodomTy CodomainType;

	typedef MapTy<DomainType,CodomainType> 	ValueVec;

	/**
	 * Extract the value of entities given a block of the CFG.
	 */
	virtual ValueVec extract(const cfg::Block& block) const = 0;

	virtual ~EntityAssigner() { }
};

template <class T, template <class> class Cont>
int getEntityIdx(const Cont<T>& cont, const T& entity) {
	auto&& fit = std::find(cont.begin(), cont.end(), entity);
	if (fit == cont.end()) { return -1; }
	return std::distance(cont.begin(), fit); 
}

struct VariableMapper : public EntityMapper<core::VariablePtr, utils::set::PointerSet> {

	EntityVec extract(const CFG& cfg) const;

	virtual ~VariableMapper() { }
};

/**
 * Variable: extract variable entities
 */
enum Usage { USE, DEF, UNKNOWN };





} } } // end insieme::analysis::dfa
