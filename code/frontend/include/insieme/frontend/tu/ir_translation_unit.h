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
#include <utility>
#include <vector>

#include "insieme/utils/assert.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/ir.h"

namespace insieme {
namespace frontend {
namespace tu {

	class IRTranslationUnit : public utils::Printable {

	public:

		typedef utils::map::PointerMap<core::GenericTypePtr, core::TypePtr> TypeMap;

		typedef utils::map::PointerMap<core::LiteralPtr, core::LambdaExprPtr> FunctionMap;

		typedef std::pair<core::LiteralPtr, core::ExpressionPtr> Global;
		typedef std::vector<Global> GlobalsList;

	private:

		TypeMap types;

		FunctionMap functions;

		GlobalsList globals;

	public:

		IRTranslationUnit() {}

		IRTranslationUnit(const TypeMap& types, const FunctionMap& functions, const GlobalsList& globals)
			: types(types), functions(functions), globals(globals) {}

		// getter:

		const TypeMap& getTypes() const {
			return types;
		}

		const FunctionMap& getFunctions() const {
			return functions;
		}

		const GlobalsList& getGlobals() const {
			return globals;
		}

		// modifier:

		void addType(const core::GenericTypePtr& symbol, const core::TypePtr& definition) {
			assert(types.find(symbol) == types.end());
			types.insert( { symbol, definition } ).second;
		}

		void addFunction(const core::LiteralPtr& symbol, const core::LambdaExprPtr& definition) {
			assert_eq(symbol->getType(), definition->getType());
			assert(functions.find(symbol) == functions.end());
			functions.insert( { symbol, definition } );
		}

		void addGlobal(const core::LiteralPtr& symbol, const core::ExpressionPtr& definition) {
			addGlobal(Global(symbol, definition));
		}

		void addGlobal(const Global& global);

		// operators:

		bool operator==(const IRTranslationUnit& other) const {
			return types == other.types && functions == other.functions && globals == other.globals;
		}

		core::TypePtr operator[](const core::GenericTypePtr& type) const {
			auto pos = types.find(type);
			return (pos != types.end()) ? pos->second : core::TypePtr();
		}

		core::LambdaExprPtr operator[](const core::LiteralPtr& lit) const {
			auto pos = functions.find(lit);
			return (pos != functions.end()) ? pos->second : core::LambdaExprPtr();
		}

		bool empty() const {
			return types.empty() && functions.empty() && globals.empty();
		}

		core::NodeManager& getNodeManager() const {
			if (!types.empty()) return types.begin()->first->getNodeManager();
			if (!functions.empty()) return functions.begin()->first->getNodeManager();
			if (!globals.empty()) return globals.begin()->first->getNodeManager();
			assert(false && "Must not be called on empty unit.");
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const;

	};


	// ---------------- merge utilities -----------------------

	inline IRTranslationUnit merge(const IRTranslationUnit& a) { return a; }

	IRTranslationUnit merge(const IRTranslationUnit& a, const IRTranslationUnit& b);

	template<typename ... T>
	IRTranslationUnit merge(const IRTranslationUnit& a, const IRTranslationUnit& b, const IRTranslationUnit& c, const T& ... rest) {
		return merge(merge(a,b),c, rest...);
	}

	IRTranslationUnit merge(const vector<IRTranslationUnit>& units);


	// -------------- program conversion ----------------------

	core::ProgramPtr toProgram(const IRTranslationUnit& a);

	inline core::ProgramPtr toProgram(const vector<IRTranslationUnit>& units) {
		return toProgram(merge(units));
	}

	template<typename ... T>
	core::ProgramPtr toProgram(const T& ... units) {
		return toProgram(merge(units ...));
	}

} // end namespace tu
} // end namespace frontend
} // end namespace insieme
