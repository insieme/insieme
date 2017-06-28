/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace tu {

	class IRTranslationUnit : public insieme::utils::Printable {
	  public:
		typedef insieme::utils::map::PointerMap<core::GenericTypePtr, core::TagTypePtr> TypeMap;

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

		bool isCppCode;

	  public:
		IRTranslationUnit(core::NodeManager& mgr) : mgr(&mgr), isCppCode(false) {}

		IRTranslationUnit(core::NodeManager& mgr, const TypeMap& types, const FunctionMap& functions, const GlobalsList& globals,
		                  const Initializer& initializer, const EntryPointList& entryPoints, bool cppCode)
		    : mgr(&mgr), types(types), functions(functions), globals(globals), initializer(initializer), entryPoints(entryPoints), isCppCode(cppCode) {}

		IRTranslationUnit(const IRTranslationUnit& other)
		    : mgr(other.mgr), types(other.types), functions(other.functions), globals(other.globals), initializer(other.initializer),
		      entryPoints(other.entryPoints), isCppCode(other.isCppCode) {}

		// getter:
		bool isEmpty() {
			return types.empty() && functions.empty() && globals.empty();
		}

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

		bool hasType(const core::GenericTypePtr& genTy) const {
			return types.find(genTy) != types.end();
		}

		// mutable getter:

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

		void addType(const core::GenericTypePtr& symbol, const core::TagTypePtr& definition) {
			assert_true(symbol);
			assert_true(definition);
			types.insert({mgr->get(symbol), mgr->get(definition)});
		}

		void replaceType(const core::GenericTypePtr& symbol, const core::TagTypePtr& definition) {
			assert_true(symbol);
			assert_true(definition);
			assert(types.find(symbol) != types.end());
			types[symbol] = definition;
		}
		void substituteType(const core::GenericTypePtr& oldSymbol, const core::GenericTypePtr& newSymbol, const core::TagTypePtr& definition) {
			assert_true(oldSymbol);
			assert_true(newSymbol);
			assert_true(definition);
			assert(types.find(oldSymbol) != types.end());
			types.erase(oldSymbol);
			types[newSymbol] = definition;
		}

		void addFunction(const core::LiteralPtr& symbol, const core::LambdaExprPtr& definition) {
			assert_eq(*symbol->getType(), *definition->getType());
			functions[symbol] = definition;
		}

		/**
		 * removes a function from the function map
		 */
		void removeFunction(const core::LiteralPtr& symbol) {
			if(functions.find(symbol) != functions.end()) functions.erase(symbol);
		}

		/**
		 * replaces a previous definition by a new one
		 */
		void replaceFunction(const core::LiteralPtr& symbol, const core::LambdaExprPtr& definition) {
			assert_eq(*symbol->getType(), *definition->getType());
			assert(functions.find(symbol) != functions.end());
			functions[symbol] = definition;
		}
		/**
		 * replaces previous definition and changes the symbol that points to it
		 */
		void substituteFunction(const core::LiteralPtr& oldSymbol, const core::LiteralPtr& newSymbol, const core::LambdaExprPtr& definition) {
			assert_eq(*newSymbol->getType(), *definition->getType());
			assert(functions.find(oldSymbol) != functions.end());
			functions.erase(oldSymbol);
			functions[newSymbol] = definition;
		}

		void addGlobal(const core::LiteralPtr& symbol, const core::ExpressionPtr& definition = core::ExpressionPtr()) {
			addGlobal(Global(symbol, definition));
		}

		void addGlobal(const Global& global);

		void replaceGlobal(const Global& old, const Global& replacement) {
			std::replace(globals.begin(), globals.end(), old, replacement);
		}

		void addInitializer(const core::ExpressionPtr& expr) {
			initializer.push_back(mgr->get(expr));
		}

		void addEntryPoints(const core::LiteralPtr& literal) {
			assert(functions.find(literal) != functions.end());
			entryPoints.push_back(mgr->get(literal));
		}

		void removeType(const core::GenericTypePtr& type) {
			types.erase(type);
		}

		// operators:

		IRTranslationUnit operator=(const IRTranslationUnit& other) {
			mgr = other.mgr;
			initializer = other.initializer;

			types.clear();
			std::copy(other.types.begin(), other.types.end(), std::inserter(types, types.end()));
			functions.clear();
			std::copy(other.functions.begin(), other.functions.end(), std::inserter(functions, functions.end()));
			globals.clear();
			std::copy(other.globals.begin(), other.globals.end(), std::back_inserter(globals));
			entryPoints.clear();
			std::copy(other.entryPoints.begin(), other.entryPoints.end(), std::back_inserter(entryPoints));
			isCppCode = other.isCppCode;

			return *this;
		}
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

		template <typename Visitor>
		void visitAll(Visitor& visit) const {
			for(const auto& cur : types) {
				visit(cur.first);
				visit(cur.second);
			}

			for(const auto& cur : functions) {
				visit(cur.first);
				visit(cur.second);
			}

			for(const auto& cur : globals) {
				visit(cur.first);
				visit(cur.second);
			}

			for(const auto& cur : initializer) {
				visit(cur);
			}

			for(const auto& cur : entryPoints) {
				visit(cur);
			}
		}

		bool isCXX() const {
			return isCppCode;
		}

		void setCXX(bool flag = true) {
			isCppCode = flag;
		}

		std::ostream& printTo(std::ostream& out) const;

	};


	// ---------------- merge utilities -----------------------

	IRTranslationUnit merge(core::NodeManager& mgr, const IRTranslationUnit& a, const IRTranslationUnit& b);

	template <typename... T>
	IRTranslationUnit merge(core::NodeManager& mgr, const IRTranslationUnit& a, const IRTranslationUnit& b, const T&... rest) {
		return merge(mgr, merge(mgr, a, b), rest...);
	}

	IRTranslationUnit merge(core::NodeManager& mgr, const vector<IRTranslationUnit>& units);


	// -------------- program conversion ----------------------

	core::ProgramPtr toProgram(core::NodeManager& mgr, const IRTranslationUnit& a, const string& entryPoint = "main");

	inline core::ProgramPtr toProgram(core::NodeManager& mgr, const vector<IRTranslationUnit>& units, const string& entryPoint = "main") {
		return toProgram(mgr, merge(mgr, units));
	}

	core::ProgramPtr resolveEntryPoints(core::NodeManager& mgr, const IRTranslationUnit& a);

} // end namespace tu
} // end namespace core
} // end namespace insieme
