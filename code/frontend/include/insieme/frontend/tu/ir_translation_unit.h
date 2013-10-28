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
#include "insieme/core/analysis/normalize.h"

namespace insieme {
namespace frontend {
namespace tu {

	class IRTranslationUnit : public insieme::utils::Printable {

	public:

		typedef insieme::utils::map::PointerMap<core::GenericTypePtr, core::TypePtr> TypeMap;

		typedef insieme::utils::map::PointerMap<core::LiteralPtr, core::LambdaExprPtr> FunctionMap;

		typedef std::pair<core::LiteralPtr, core::ExpressionPtr> Global;
		typedef std::vector<Global> GlobalsList;

		typedef std::vector<core::ExpressionPtr> Initializer;

		typedef std::vector<core::LiteralPtr> EntryPointList;

	private:

		core::NodeManager* mgr;

		TypeMap types;

		FunctionMap functions;

		GlobalsList globals;

		Initializer initializer;

		EntryPointList entryPoints;

	public:

		IRTranslationUnit(core::NodeManager& mgr) : mgr(&mgr) {}

		IRTranslationUnit(core::NodeManager& mgr, const TypeMap& types, const FunctionMap& functions, const GlobalsList& globals, const Initializer& initializer, const EntryPointList& entryPoints)
			: mgr(&mgr), types(types), functions(functions), globals(globals), initializer(initializer), entryPoints(entryPoints) {}

		IRTranslationUnit(const IRTranslationUnit& other)
			: mgr(other.mgr), types(other.types), functions(other.functions), globals(other.globals), initializer(other.initializer), entryPoints(other.entryPoints) {}

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

		const Initializer& getInitializer() const {
			return initializer;
		}

		const EntryPointList& getEntryPoints() const {
			return entryPoints;
		}

		// mutable getter:

	//	TypeMap& getTypes() {
	//		return types;
	//	}

		FunctionMap& getFunctions() {
			return functions;
		}

		GlobalsList& getGlobals() {
			return globals;
		}

		Initializer& getInitializer() {
			return initializer;
		}

		EntryPointList& getEntryPoints() {
			return entryPoints;
		}

		// modifier:

		void addType(const core::GenericTypePtr& symbol, const core::TypePtr& definition) {
			assert(symbol );
			assert(definition);
			types.insert( { mgr->get(symbol), mgr->get(definition) } ).second;
		}

		void replaceType(const core::GenericTypePtr& symbol, const core::TypePtr& definition) {
			assert(symbol );
			assert(definition);
			assert(types.find(symbol) != types.end());
			types[symbol] = definition;
		}

		void addFunction(const core::LiteralPtr& symbol, const core::LambdaExprPtr& definition) {
			assert_eq(*symbol->getType(), *definition->getType());
			//check if function exists, if it exists we
			//have to check if they are really the same.
			//assert((functions.find(symbol) == functions.end()) || core::analysis::equalNormalize ( definition, functions[symbol] ));
			if(functions.find(symbol) != functions.end()) {
                assert(core::analysis::equalNormalize ( definition, functions[symbol] ));
			}
			functions.insert( { mgr->get(symbol), mgr->get(definition) } );
		}

		/**
		 * replaces a previous definition by a new one
		 */
		void replaceFunction(const core::LiteralPtr& symbol, const core::LambdaExprPtr& definition){
			assert_eq(*symbol->getType(), *definition->getType());
			assert(functions.find(symbol) != functions.end());
			functions[symbol] = definition;
		}

		void addGlobal(const core::LiteralPtr& symbol, const core::ExpressionPtr& definition = core::ExpressionPtr()) {
			addGlobal(Global(symbol, definition));
		}

		void addGlobal(const Global& global);

		void addInitializer(const core::ExpressionPtr& expr) {
			initializer.push_back(mgr->get(expr));
		}

		void addEntryPoints(const core::LiteralPtr& literal) {
			assert(functions.find(literal) != functions.end());
			entryPoints.push_back(mgr->get(literal));
		}

		// operators:

		bool operator==(const IRTranslationUnit& other) const {
			return types == other.types && functions == other.functions && globals == other.globals && entryPoints == other.entryPoints;
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
			return types.empty() && functions.empty() && globals.empty() && initializer.empty();
		}

		core::NodeManager& getNodeManager() const {
			return *mgr;
		}

		IRTranslationUnit toManager(core::NodeManager& manager) const;

		core::NodePtr resolve(const core::NodePtr& fragment) const;

	protected:

		virtual std::ostream& printTo(std::ostream& out) const;

	};


	// ---------------- merge utilities -----------------------

	IRTranslationUnit merge(core::NodeManager& mgr, const IRTranslationUnit& a, const IRTranslationUnit& b);

	template<typename ... T>
	IRTranslationUnit merge(core::NodeManager& mgr, const IRTranslationUnit& a, const IRTranslationUnit& b, const T& ... rest) {
		return merge(mgr, merge(mgr, a,b), rest...);
	}

	IRTranslationUnit merge(core::NodeManager& mgr, const vector<IRTranslationUnit>& units);


	// -------------- program conversion ----------------------

	core::ProgramPtr toProgram(core::NodeManager& mgr, const IRTranslationUnit& a, const string& entryPoint = "main");

	inline core::ProgramPtr toProgram(core::NodeManager& mgr, const vector<IRTranslationUnit>& units, const string& entryPoint = "main") {
		return toProgram(mgr, merge(mgr, units));
	}

	core::ProgramPtr resolveEntryPoints(core::NodeManager& mgr, const IRTranslationUnit& a);

} // end namespace tu
} // end namespace frontend
} // end namespace insieme
