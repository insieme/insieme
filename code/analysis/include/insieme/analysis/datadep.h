/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <functional>
#include <map>
#include <string>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_values.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace analysis {

/// \brief The DataDependence class gives access to analytics and statistics related to variable use.
/// The statistics are gathered while walking the variable def-use chains which is done from the constructor. After
/// that, the use of the data is pretty cheap.
///
/// These statistics play an important role in analysis, transformation, and optimization.
/// For example:
/// * In one of our test cases, 211 variables were defined, but only 145 were used.
/// * This means that some variables were never used, and others were used only once.
/// * For never used variables, we can use DCE techniques to get rid of them completely.
/// * For write-once-read-once variable, we can substitute the variable by its defining expression (given that
///   INSPIRE is referentially transparent).
///
class DataDependence: public insieme::core::IRVisitor<void, insieme::core::Address> {
	insieme::core::NodeAddress frag;                                  /// < code fragment passed to this compiler pass
	std::map<insieme::core::VariableAddress, std::vector<insieme::core::VariableAddress> > uses; /// uses for each def

	void visitNode(const insieme::core::NodeAddress &node);
	void visitVariable(const insieme::core::VariableAddress &node);
	void recordDef(const insieme::core::VariableAddress &def);
	void recordUse(const insieme::core::VariableAddress &def, const insieme::core::VariableAddress &use);

public:
	DataDependence(const insieme::core::NodeAddress frag);
	static insieme::core::VariableAddress getDef(const insieme::core::VariableAddress& givenaddr);
	std::vector<insieme::core::VariableAddress> getDefs(std::function<bool(const insieme::core::VariableAddress&)>
	    predicate=[](const insieme::core::VariableAddress&) { return true; });
	std::vector<insieme::core::VariableAddress> getDefs(unsigned int id);
	std::vector<insieme::core::VariableAddress> getUse(const insieme::core::VariableAddress& def);

	static unsigned int getVarID(const insieme::core::VariableAddress &var);
	static void printNode(const insieme::core::NodeAddress &node, std::string descr="", unsigned int start=0, int count=-1);
	static void printTypeChain(const insieme::core::NodeAddress &node, std::string descr="", int count=-1);
};

}}
