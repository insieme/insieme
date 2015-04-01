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

#include "insieme/core/parser3/ir_parser.h"

#include <sstream>

#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/parser3/detail/driver.h"

namespace insieme {
namespace core {
namespace parser3 {

using namespace detail;

namespace {

    void checkErrors(inspire_driver& driver, bool onFailThrow){
        if (driver.result) {
            if (onFailThrow ){
                std::stringstream ss;
                driver.print_errors(ss);
                throw IRParserException(ss.str());
            }
            else{
                driver.print_errors();
            }
        }
    }
}


	NodePtr parse(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions){
        inspire_driver driver(code, manager);
        for (const auto& def : definitions) driver.add_symb(def.first, def.second);
        auto x = driver.parseProgram();
        if(!x) checkErrors(driver, onFailThrow);
        return x;
    }

	TypePtr parse_type(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions){
        inspire_driver driver(code, manager);
        for (const auto& def : definitions) driver.add_symb(def.first, def.second);
        auto x = driver.parseType();
        if(!x) checkErrors(driver, onFailThrow);
        return x;
    }

	ExpressionPtr parse_expr(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions){
        inspire_driver driver(code, manager);
        for (const auto& def : definitions) driver.add_symb(def.first, def.second);
        auto x = driver.parseExpression();
        if(!x) checkErrors(driver, onFailThrow);
        return x;
    }

	StatementPtr parse_stmt(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions){
        inspire_driver driver(code, manager);
        for (const auto& def : definitions) driver.add_symb(def.first, def.second);
        auto x =  driver.parseStmt();
        if(!x) checkErrors(driver, onFailThrow);
        return x;
    }

	ProgramPtr parse_program(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions){
        inspire_driver driver(code, manager);
        for (const auto& def : definitions) driver.add_symb(def.first, def.second);
        auto x =  driver.parseProgram();
        if(!x) checkErrors(driver, onFailThrow);
        return x;
    }

	std::vector<NodeAddress> parse_addresses(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions){
        assert_not_implemented() << "some stuff to do";
        auto x =  std::vector<NodeAddress>();
        return x;
    }

} //  parser3
} //  core
} // insime

