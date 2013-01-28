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

#include "insieme/analysis/access/access_mgr.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/analysis/polyhedral/iter_dom.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;

namespace insieme {
namespace analysis {
namespace access {

AccessClassSet AccessManager::findClass(const AccessPtr& access) const {

	AccessClassSet ret;
	for (const auto& accClass : classes) {
		for (const auto& acc : accClass->getAccesses()) {
				if (*acc == *access || equalPath(acc,access)) { 
				ret.insert( accClass );
			}
		}
	}

	return ret;
}


void AccessManager::printDotGraph(std::ostream& out) const { 

	// print dot header
	out << "digraph G {" << std::endl;

	for(unsigned idx=0; idx<size(); ++idx) {

		out << "\t" << idx << " [shape=\"box\",label=\"UID: " << classes[idx]->getUID() << "\\n" << 
		    join("\\l", classes[idx]->getAccesses(), [&](std::ostream& jout, const AccessPtr& cur) {
				jout << "+ " << cur;
			}) << "\\l\"];" << std::endl;

		// check dependencies 
		for (const auto& dep : classes[idx]->getSubClasses()) {
		
			out << "\t" << idx << " -> " << std::get<0>(dep).lock()->getUID() 
				<< " [label=\"" << std::static_pointer_cast<const Access>(std::get<1>(dep)) << "\"];" 
				<< std::endl;

		}
	}
	
	out << "}" << std::endl;
}



AccessClassPtr AccessManager::addClass(
		AccessClassPtr 				parent, 
		const AccessPtr& 			access, 
		const AccessDecoratorPtr& 	dec, 
		bool 						append_to_parent) 
{
	// Creates a new alias class  (can't use make_shared because the constructor is private)
	auto newClass = std::shared_ptr<AccessClass>(new AccessClass(std::cref(*this), classes.size(), parent));
	if (access) {
		newClass->storeAccess(access);
	}
	classes.emplace_back( newClass );

	if (append_to_parent && parent) {
		parent->addSubClass( AccessClass::Dependence( newClass, dec ) );
	}
	return newClass;
}



AccessClassSet AccessManager::getClassFor(const AccessPtr& access, bool subAccess) {

	AccessClassSet retClasses;

    /*
     * Iterate through the existing classes and determine whether this access belongs to one of
     * the exising classes, if not create a new class
     */
	for (auto& cl : classes) {

		const auto& classAccesses = cl->accesses;
		bool found=false, belongs=false;

		for(const auto& cur : classAccesses) {
			if (*cur == *access) {
				found = belongs = true; // the access is already in the class, therefore
										// we mark it as found
				break;
			} else if (!belongs && equalPath(cur, access)) {
				belongs = true;
			}
		}

		if (belongs) {
			// the access is already stored in this class, therefore we simply return it
			if (!found) { cl->storeAccess(access); }
			retClasses.insert(cl);
		}
	}

	if (!retClasses.empty()) { return retClasses; }


	// it might be that this access is an alias for an expression for which we already defined a
	// class.
	if (auto potentialAlias =
			core::dynamic_pointer_cast<const core::Variable>(access->getAddress().getAddressedNode())) {

		UnifiedAddress aliasedExpr(tmpVarMap.getMappedExpr( potentialAlias ));

		if (aliasedExpr.getAbsoluteAddress() && cfg) {
			
			if (aliasedExpr.getAddressedNode()->getNodeType() == core::NT_CastExpr) {
				// If this is a cast expression we consider it as a no-op, therefore 
				// we forward the the alias through the casted expression 
				aliasedExpr = aliasedExpr.getAddressOfChild(1);
			} else {
				aliasedExpr = cfg->find(aliasedExpr.getAbsoluteAddress());
			}
		}

		if (aliasedExpr.getAbsoluteAddress()) {
			try {
				auto aliasAccess = getImmediateAccess(potentialAlias->getNodeManager(), aliasedExpr, TmpVarMap(), subAccess);
				auto clSet = getClassFor(aliasAccess, subAccess);
				for (auto& cl : clSet) { cl->storeAccess(access); }

				return clSet;
			} catch (NotAnAccessException&& e) { }
		}
	}

	/**
	 * This might be an access to a subrange of a class.
	 *
	 * This can happen either when a compound member of a struct is accessed. or when the (N-x)th
	 * dimension of a N dimensional array is accessed
	 */
	AccessClassSet 				parentClasses;
	AccessDecoratorPtr 			subLevel;
	
	AccessPtr skipDeref = access;
	// Skips any derefs present through the access. 
	while (skipDeref->getType() == AccessType::AT_DEREF) {
		skipDeref = cast<Deref>(skipDeref)->getSubAccess();
	}

	// Check whether this access is accessing a sublevel 
	if (skipDeref->getType() == AccessType::AT_MEMBER || skipDeref->getType() == AccessType::AT_SUBSCRIPT) {
		parentClasses = getClassFor( cast<AccessDecorator>(skipDeref)->getSubAccess(), false );
		subLevel = cast<AccessDecorator>(skipDeref)->switchSubAccess(AccessPtr());
	}

	// check if the parent class already has a child to represent this type of access
	if (!parentClasses.empty()) {
		assert(subLevel && "Invalid sublevel");

		typedef std::tuple<AccessClassPtr, AccessClassPtr, AccessDecoratorPtr> AccessInfo;
		std::vector<AccessInfo> toAppend;

		for (auto& parentClass : parentClasses) { 
			
			bool classified = false;

			// This is the access used for comparison in the case we are threating 
			// subscript accesses. 
			auto cmpAccess = skipDeref;

			for(auto& dep : parentClass->getSubClasses()) {
				
				const auto& subClass  = std::get<0>(dep).lock();
				const auto& subAccess = std::get<1>(dep);
				assert(subClass && subAccess);

				//LOG(INFO) << subClass;
				//LOG(INFO) << subAccess;
				//LOG(INFO) << *this;

				// assume that the subclasses are disjoints 
				if (subAccess->getType() == AccessType::AT_MEMBER) {

					assert(skipDeref->getType() == AccessType::AT_MEMBER);

					if (*cast<Member>(skipDeref)->getMember() == *cast<Member>(subAccess)->getMember()) {

						subClass->storeAccess(access);
						retClasses.insert( subClass );
						classified = true;
						break;
					}
					continue;
				}

				// Subscripts 
				if (subAccess->getType() == AccessType::AT_SUBSCRIPT) {
					
					assert(skipDeref->getType() == AccessType::AT_SUBSCRIPT);

					auto classRange = cast<Subscript>(subAccess);
					auto accessRange = cast<Subscript>(cmpAccess);

					if (accessRange->isContextDependent()) {
						// classfy it with the parent 
						assert(parentClass && "parent class must be valid");
						LOG(INFO) << "NOCTX";
						parentClass->storeAccess(access);
						retClasses.insert( parentClass );
						classified = true;
						break;
					}

					// Create a polyhedral context so that we can work with sets 
					auto ctx = polyhedral::makeCtx();
					auto classSet  = polyhedral::makeSet(ctx, 
							classRange->getRange() ?
								polyhedral::IterationDomain(classRange->getRange()) :
								polyhedral::IterationDomain(classRange->getIterationVector(), false)
							);
					auto accessSet = polyhedral::makeSet(ctx, 
							accessRange->getRange() ?
								polyhedral::IterationDomain(accessRange->getRange()) :
								polyhedral::IterationDomain(accessRange->getIterationVector(), false)
							);
					
					// compute the difference, if it is empty then the two ranges are equivalent 
					auto intersection = classSet * accessSet;
					LOG(INFO) << "intersection " << *intersection << " " << ", "; 

					if ( !intersection->empty() ) { 
						
						auto splitAccess = [&]() {
							auto diff = accessSet - intersection;

							polyhedral::IterationVector iv;
							auto diffCons = diff->toConstraint(access->getRoot().getVariable()->getNodeManager(), iv);
								
							// Try to classify the remaining range 
							cmpAccess = std::make_shared<Subscript>(
											cmpAccess->getAddress(),
											cast<Subscript>(cmpAccess)->getSubAccess(),
											cmpAccess->isFinal(),
											cast<Subscript>(cmpAccess)->getContext(),
											iv,
											diffCons
										);

							subLevel = cast<AccessDecorator>(cmpAccess)->switchSubAccess(AccessPtr());
						};

						auto splitClass = [&]() {

							polyhedral::IterationVector iv;
							auto interCons = intersection->toConstraint(
												access->getRoot().getVariable()->getNodeManager(), iv);
	

							// Try to classify the remaining range 
							auto accDecLevel = std::make_shared<Subscript>(
											NodeAddress(),
											AccessPtr(),
											cmpAccess->isFinal(),
											cast<Subscript>(cmpAccess)->getContext(),
											iv,
											interCons
										);
														
							// now change the orginal class by subclassing by the difference 
							auto diff = classSet - intersection;

							polyhedral::IterationVector iv2;
							auto diffCons = diff->toConstraint(
												access->getRoot().getVariable()->getNodeManager(), iv2);

							// Try to classify the remaining range 
							auto diffLevel = std::make_shared<Subscript>(
											NodeAddress(),
											AccessPtr(),
											cmpAccess->isFinal(),
											cast<Subscript>(cmpAccess)->getContext(),
											iv2,
											diffCons
										);
							
							// modify the current access 
							std::get<1>(dep) = diffLevel;

							auto newClass = addClass(parentClass, access, accDecLevel, false);
							
							toAppend.push_back( AccessInfo(newClass, parentClass, accDecLevel) );

							for (auto& acc : subClass->getAccesses()) {
								newClass->storeAccess( acc );
							}

							retClasses.insert( newClass );
						};

						// We hit the same class, add the access to the class  
						if (*intersection == *accessSet && *intersection == *classSet ) 
						{
							subClass->storeAccess(access);
							retClasses.insert( subClass );
							classified = true;
							break;
						}

						// We are in the case where the intersection is equal to the class set. 
						// This means we have to create a new class and append the old class as a subrange 
						if (*intersection == *classSet)  {
							
							// Add a new class representing the intersection
							subClass->storeAccess( access );
							retClasses.insert( subClass );

							splitAccess();
							continue;
						}

						if (*intersection == *accessSet) {

							splitClass();
							classified = true;
							continue;
						}
							
						// We have an intersection, however this is a subset of both the current 
						// access and the subclass selector of the parent class 
						splitClass();
						splitAccess();
					}

				}

			}

			if (!toAppend.empty()) {
				for (const auto& cur : toAppend) 
					std::get<1>(cur)->addSubClass( AccessClass::Dependence( std::get<0>(cur), std::get<2>(cur) ) );
			}

			if (!classified) {
				// the access was not classified 
				retClasses.insert( addClass(parentClass, access, subLevel) );

				if (subLevel->getType() == AccessType::AT_SUBSCRIPT) {

					// if there is an access for a subrange then we have to define a new class
					// which contains the possible remaining accesses 

					auto subAccess = cast<Subscript>(subLevel);

					auto ctx = polyhedral::makeCtx();
					auto universe = polyhedral::makeSet(ctx, 
							polyhedral::IterationDomain(subAccess->getIterationVector())
						);
					auto accessSet = polyhedral::makeSet(ctx, 
							subAccess->getRange() ?
								polyhedral::IterationDomain(subAccess->getRange()) :
								polyhedral::IterationDomain(subAccess->getIterationVector(), false)
							);

					polyhedral::IterationVector iv;
					
					auto diffCons = (universe-accessSet)->toConstraint(
								access->getRoot().getVariable()->getNodeManager(), iv);

					// Try to classify the remaining range 
					auto accDecLevel = std::make_shared<Subscript>(
											NodeAddress(),
											AccessPtr(),
											cmpAccess->isFinal(),
											subAccess->getContext(),
											iv,
											diffCons
										);
														
					addClass(parentClass, AccessPtr(), accDecLevel);
							
				}
			}
		}

		return retClasses;
	}

	return { addClass(nullptr, access, subLevel) };
}

std::ostream& AccessManager::printTo(std::ostream& out) const {
	return out << "AccessManager [" << size() << "]\n{\t" <<
		join("\n\t", classes, [&](std::ostream& jout, const AccessClassPtr& cur) {
				jout << *cur;
				}) << std::endl << "}";
}


} // end access namespace 
} // end analysis namespace 
} // end insieme namespace 
