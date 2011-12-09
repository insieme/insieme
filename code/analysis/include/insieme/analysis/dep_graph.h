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
#include "insieme/core/ir_address.h"

#include <boost/graph/adjacency_list.hpp>

namespace insieme {
namespace analysis {
namespace dep {

class Stmt : public utils::Printable {

	size_t 				m_id;
	core::NodeAddress 	m_addr;

public:
	Stmt(size_t id, const core::NodeAddress& addr);

	const size_t& id() const { return m_id; }
	const core::NodeAddress& addr() const { return m_addr; }

	std::ostream& printTo(std::ostream& out) const {
		return out << "S" << m_id;
	}
};

enum DependenceType { RAW=0x1, TRUE=0x1,   // Read-After-Write dependence (or true-dependence)
					  WAR=0x2, ANTI=0x2,   // Write-After-Read dependence (or anti-dependence)
					  WAW=0x4, OUTPUT=0x4, // Write-After-Write dependence (or output-dependence)
					  RAR=0x8, INPUT=0x8   // Read-After-Read dependence (or input-dependence)
					};

class Dependence : public utils::Printable {
	
	DependenceType m_type;

public:
	
	Dependence( const DependenceType& type) : m_type(type) { }

	const DependenceType& type() const { return m_type; }

	std::ostream& printTo(std::ostream& out) const {
		return out;
	}
};


class DependenceGraph {

	typedef boost::GraphConcept<
		boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, Stmt, Dependence>
	> Graph;

	Graph graph;

public:

	DependenceGraph() { } 
	
};

DependenceGraph extractDependenceGraph( const core::NodePtr& root );


} // end dep namespace
} // end analysis namespace
} // end insieme namespace 
