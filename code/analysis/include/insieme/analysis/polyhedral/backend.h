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

#include "insieme/utils/printable.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace analysis {
namespace poly {

/**************************************************************************************************
 * Generic implementation of a the concept of a set which is natively supported by polyhedral
 * libraries. The class presents a set of operations which are possible on sets (i.e. intersect,
 * union, difference, etc...)
 *************************************************************************************************/
template <class Ctx>
struct Set : public utils::Printable {
	
	typedef Ctx ctx_type;

	std::ostream& printTo(std::ostream& out) const = 0; 

	bool isEmpty() const = 0;

	~Set() { }
private:
	Set();
};

template <class Ctx> 
struct Map : public utils::Printable {

	typedef Ctx ctx_type;

	std::ostream& printTo(std::ostream& out) const = 0; 

	bool isEmpty() const = 0;

	~Map() { }

private:
	Map();
};

template <class Ctx>
std::shared_ptr<Set<Ctx>> set_union(Ctx& ctx, const Set<Ctx>& lhs, const Set<Ctx>& rhs);

template <class Ctx>
std::shared_ptr<Set<Ctx>> set_intersect(Ctx& ctx, const Set<Ctx>& lhs, const Set<Ctx>& rhs);

template <class Ctx>
std::shared_ptr<Map<Ctx>> map_union(Ctx& ctx, const Map<Ctx>& lhs, const Map<Ctx>& rhs);

template <class Ctx>
std::shared_ptr<Map<Ctx>> map_intersect(Ctx& ctx, const Map<Ctx>& lhs, const Map<Ctx>& rhs);

template <class Ctx>
std::shared_ptr<Map<Ctx>> map_intersect_domain(Ctx& ctx, const Map<Ctx>& lhs, const Set<Ctx>& dom);


//===== Dependency analysis =======================================================================

template <class Ctx>
struct DependenceInfo {
	Map<Ctx> mustDep;
	Map<Ctx> mayDep;
	Map<Ctx> mustNoSource;
	Map<Ctx> mayNoSource;
};

template <class Ctx>
void buildDependencies( 
		Ctx&								ctx,
		const std::shared_ptr<Set<Ctx>>& 	domain, 
		const std::shared_ptr<Map<Ctx>>& 	schedule, 
		const std::shared_ptr<Map<Ctx>>& 	sinks, 
		const std::shared_ptr<Map<Ctx>>& 	must_sources = std::shared_ptr<Map<Ctx>>(), 
		const std::shared_ptr<Map<Ctx>>& 	may_sourcs = std::shared_ptr<Map<Ctx>>()
);

//===== Conversion Utilities ======================================================================

enum Backend { ISL };

/**
 * Defines type traits which are used to determine the type of the context for implementing backends 
 */
template <Backend B>
struct BackendTraits;

template <Backend B>
std::shared_ptr<typename BackendTraits<B>::ctx_type>
createContext() { return std::make_shared<typename BackendTraits<B>::ctx_type>(); }

template <Backend B>
std::shared_ptr<Set<typename BackendTraits<B>::ctx_type>> 
makeSet( typename BackendTraits<B>::ctx_type& ctx, 
		 const IterationVector& iterVec,
		 const ConstraintCombinerPtr& constraint,
		 const std::string& tuple_name = std::string())
{
	return std::make_shared<Set<typename BackendTraits<B>::ctx_type>>(ctx, iterVec, constraint, tuple_name);
}

template <Backend B>
std::shared_ptr<Map<typename BackendTraits<B>::ctx_type>>
makeMap( typename BackendTraits<B>::ctx_type& ctx,  
		 const AffineSystem& affSys,
		 const std::string& in_tuple_name = std::string(),
		 const std::string& out_tuple_name = std::string())
{
	return std::make_shared<Map<typename BackendTraits<B>::ctx_type>>(ctx, affSys, in_tuple_name, out_tuple_name);
}

} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 
