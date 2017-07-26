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
 */

#include "insieme/core/lang/basic.h"

#include <map>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/lang/lang.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace lang {

	using namespace boost;

	LiteralNotFoundException::LiteralNotFoundException(const string& lit) throw() : msg(string("Literal not found: ") + lit) {}

	// Work around GCC unimplemented features
	template <class... All>
	struct GroupChecker;
	template <class Head, class... Tail>
	struct GroupChecker<Head, Tail...> {
		bool operator()(const BasicGenerator& bg, const NodePtr& p) {
			return Head::isInstance(bg, p) || GroupChecker<Tail...>()(bg, p);
		}
	};
	template <>
	struct GroupChecker<> {
		bool operator()(const BasicGenerator&, const NodePtr&) {
			return false;
		}
	};

	template <class... All>
	struct GroupFiller;
	template <class Head, class... Tail>
	struct GroupFiller<Head, Tail...> {
		void operator()(const BasicGenerator& bg, vector<NodePtr>& list) {
			Head::append(bg, list);
			GroupFiller<Tail...>()(bg, list);
		}
	};
	template <>
	struct GroupFiller<> {
		void operator()(const BasicGenerator& bg, vector<NodePtr>& list) {}
	};

	struct BasicGenerator::BasicGeneratorImpl : boost::noncopyable {
		NodeManager& nm;
		IRBuilder build;

		typedef LiteralPtr (BasicGenerator::*litFunPtr)() const;
		std::map<std::string, litFunPtr> literalMap;

		typedef ExpressionPtr (BasicGenerator::*derivedFunPtr)() const;
		std::map<std::string, derivedFunPtr> derivedMap;

		typedef bool (BasicGenerator::*groupCheckFuncPtr)(const NodePtr&) const;
		typedef std::multimap<BasicGenerator::Operator, std::pair<groupCheckFuncPtr, litFunPtr>> LiteralOperationMap;
		LiteralOperationMap literalOperationMap;

		typedef std::multimap<BasicGenerator::Operator, std::pair<groupCheckFuncPtr, derivedFunPtr>> DerivedOperationMap;
		DerivedOperationMap derivedOperationMap;

		#define ADD_IS_AND_GET(_id)                                                                                                                            \
			struct _id {                                                                                                                                       \
				static bool isInstance(const BasicGenerator& bg, const NodePtr& p) {                                                                           \
					return bg.is##_id(p);                                                                                                                      \
				}                                                                                                                                              \
				static void append(const BasicGenerator& bg, vector<NodePtr>& list) {                                                                          \
					list.push_back(bg.get##_id());                                                                                                             \
				}                                                                                                                                              \
			};

		#define TYPE(_id, _spec)                                                                                                                               \
			TypePtr ptr##_id;                                                                                                                                  \
			ADD_IS_AND_GET(_id)
		#define LITERAL(_id, _name, _spec)                                                                                                                     \
			LiteralPtr ptr##_id;                                                                                                                               \
			ADD_IS_AND_GET(_id)
		#define DERIVED(_id, _name, _spec)                                                                                                                     \
			ExpressionPtr ptr##_id;                                                                                                                            \
			ADD_IS_AND_GET(_id)
		#define OPERATION(_type, _op, _name, _spec)                                                                                                            \
			LiteralPtr ptr##_type##_op;                                                                                                                        \
			ADD_IS_AND_GET(_type##_op)
		#define DERIVED_OP(_type, _op, _name, _spec)                                                                                                           \
			ExpressionPtr ptr##_type##_op;                                                                                                                     \
			ADD_IS_AND_GET(_type##_op)
		#define GROUP(_id, ...)                                                                                                                                \
			struct _id {                                                                                                                                       \
				static bool isInstance(const BasicGenerator& bg, const NodePtr& p) {                                                                           \
					return bg.is##_id(p);                                                                                                                      \
				}                                                                                                                                              \
				static void append(const BasicGenerator& bg, vector<NodePtr>& list) {                                                                          \
					const vector<NodePtr>& tmp = bg.get##_id##Group();                                                                                         \
					list.insert(list.end(), tmp.begin(), tmp.end());                                                                                           \
				}                                                                                                                                              \
			};
		#include "insieme/core/lang/inspire_api/lang.def"

		BasicGeneratorImpl(NodeManager& nm) : nm(nm), build(nm) {
		#define LITERAL(_id, _name, _spec) literalMap.insert(std::make_pair(_name, &BasicGenerator::get##_id));
		#define DERIVED(_id, _name, _spec) derivedMap.insert(std::make_pair(_name, &BasicGenerator::get##_id));
		#define OPERATION(_type, _op, _name, _spec)                                                                                                            \
			literalMap.insert(std::make_pair(_name, &BasicGenerator::get##_type##_op));                                                                        \
			literalOperationMap.insert(std::make_pair(BasicGenerator::_op, std::make_pair(&BasicGenerator::is##_type, &BasicGenerator::get##_type##_op)));
		#define DERIVED_OP(_type, _op, _name, _spec)                                                                                                           \
			derivedMap.insert(std::make_pair(_name, &BasicGenerator::get##_type##_op));                                                                        \
			derivedOperationMap.insert(std::make_pair(BasicGenerator::_op, std::make_pair(&BasicGenerator::is##_type, &BasicGenerator::get##_type##_op)));
		#include "insieme/core/lang/inspire_api/lang.def"
		}

		#define GROUP(_id, ...)                                                                                                                                \
			mutable vector<NodePtr> group_##_id##_list;                                                                                                        \
			bool is##_id(const NodePtr& p) {                                                                                                                   \
				return GroupChecker<__VA_ARGS__>()(nm.getLangBasic(), p);                                                                                      \
			};                                                                                                                                                 \
			const vector<NodePtr>& get##_id##Group() const {                                                                                                   \
				if(group_##_id##_list.empty()) { GroupFiller<__VA_ARGS__>()(nm.getLangBasic(), group_##_id##_list); }                                          \
				return group_##_id##_list;                                                                                                                     \
			}
		#include "insieme/core/lang/inspire_api/lang.def"

		// ----- extra material ---

		StatementPtr ptrNoOp;
	};

	class BasicGenerator::SubTypeLattice : boost::noncopyable {
		typedef utils::map::PointerMultiMap<TypePtr, TypePtr> RelationMap;
		RelationMap subTypes;
		RelationMap superTypes;

	  public:
		void addRelation(const TypePtr& subType, const TypePtr& superType) {
			subTypes.insert(std::make_pair(superType, subType));
			superTypes.insert(std::make_pair(subType, superType));
		}

		TypeSet getSubTypesOf(const TypePtr& type) const {
			return getAllOf(type, subTypes);
		}

		TypeSet getSuperTypesOf(const TypePtr& type) const {
			return getAllOf(type, superTypes);
		}

	  private:
		TypeSet getAllOf(const TypePtr& type, const RelationMap& map) const {
			TypeSet res;
			auto types = map.equal_range(type);
			auto cur = types.first;
			while(cur != types.second) {
				res.insert(cur->second);
				++cur;
			}
			return res;
		}
	};

	BasicGenerator::BasicGenerator(NodeManager& nm) : nm(nm), pimpl(new BasicGeneratorImpl(nm)), subTypeLattice(0) {}

	BasicGenerator::~BasicGenerator() {
		delete pimpl;
		if(subTypeLattice) { delete subTypeLattice; }
	}

	#define TYPE(_id, _spec)                                                                                                                                   \
		TypePtr BasicGenerator::get##_id() const {                                                                                                             \
			if(!pimpl->ptr##_id) {                                                                                                                             \
				pimpl->ptr##_id = parser::parseType(nm, _spec, false);                                                                                         \
				markAsBuiltIn(pimpl->ptr##_id);                                                                                                                \
			}                                                                                                                                                  \
			return pimpl->ptr##_id;                                                                                                                            \
		};                                                                                                                                                     \
		bool BasicGenerator::is##_id(const NodePtr& p) const {                                                                                                 \
			return *p == *get##_id();                                                                                                                          \
		};

	#define LITERAL(_id, _name, _spec)                                                                                                                         \
		LiteralPtr BasicGenerator::get##_id() const {                                                                                                          \
			if(!pimpl->ptr##_id) {                                                                                                                             \
				pimpl->ptr##_id = pimpl->build.literal(parser::parseType(nm, _spec, false), _name);                                                            \
				markAsBuiltIn(pimpl->ptr##_id);                                                                                                                \
			}                                                                                                                                                  \
			return pimpl->ptr##_id;                                                                                                                            \
		};                                                                                                                                                     \
		bool BasicGenerator::is##_id(const NodePtr& p) const {                                                                                                 \
			return *p == *get##_id();                                                                                                                          \
		};

	#define DERIVED(_id, _name, _spec)                                                                                                                         \
		ExpressionPtr BasicGenerator::get##_id() const {                                                                                                       \
			if(!pimpl->ptr##_id) {                                                                                                                             \
				pimpl->ptr##_id = analysis::normalize(parser::parseExpr(nm, _spec, false));                                                                    \
				if(pimpl->ptr##_id.isa<insieme::core::LambdaExprPtr>()) {                                                                                      \
					pimpl->ptr##_id = insieme::core::LambdaExpr::get(nm, pimpl->ptr##_id.as<insieme::core::LambdaExprPtr>()->getLambda(), _name);              \
				}                                                                                                                                              \
				markAsBuiltIn(pimpl->ptr##_id);                                                                                                                \
				markAsDerived(pimpl->ptr##_id, _name);                                                                                                         \
			}                                                                                                                                                  \
			return pimpl->ptr##_id;                                                                                                                            \
		};                                                                                                                                                     \
		bool BasicGenerator::is##_id(const NodePtr& p) const {                                                                                                 \
			return *p == *get##_id();                                                                                                                          \
		};


	#define OPERATION(_type, _op, _name, _spec)                                                                                                                \
		LiteralPtr BasicGenerator::get##_type##_op() const {                                                                                                   \
			if(!pimpl->ptr##_type##_op) {                                                                                                                      \
				pimpl->ptr##_type##_op = pimpl->build.literal(parser::parseType(nm, _spec, false), _name);                                                     \
				markAsBuiltIn(pimpl->ptr##_type##_op);                                                                                                         \
			}                                                                                                                                                  \
			return pimpl->ptr##_type##_op;                                                                                                                     \
		};                                                                                                                                                     \
		bool BasicGenerator::is##_type##_op(const NodePtr& p) const {                                                                                          \
			return *p == *get##_type##_op();                                                                                                                   \
		};

	#define DERIVED_OP(_type, _op, _name, _spec)                                                                                                               \
		ExpressionPtr BasicGenerator::get##_type##_op() const {                                                                                                \
			if(!pimpl->ptr##_type##_op) {                                                                                                                      \
				pimpl->ptr##_type##_op = analysis::normalize(parser::parseExpr(nm, _spec, false));                                                             \
				if(pimpl->ptr##_type##_op.isa<insieme::core::LambdaExprPtr>()) {                                                                               \
					pimpl->ptr##_type##_op = insieme::core::LambdaExpr::get(nm, pimpl->ptr##_type##_op.as<insieme::core::LambdaExprPtr>()->getLambda(), _name);\
				}                                                                                                                                              \
				markAsBuiltIn(pimpl->ptr##_type##_op);                                                                                                         \
				markAsDerived(pimpl->ptr##_type##_op, _name);                                                                                                  \
			}                                                                                                                                                  \
			return pimpl->ptr##_type##_op;                                                                                                                     \
		};                                                                                                                                                     \
		bool BasicGenerator::is##_type##_op(const NodePtr& p) const {                                                                                          \
			return *p == *get##_type##_op();                                                                                                                   \
		};

	#define GROUP(_id, ...)                                                                                                                                    \
		bool BasicGenerator::is##_id(const NodePtr& p) const {                                                                                                 \
			return pimpl->is##_id(p);                                                                                                                          \
		}                                                                                                                                                      \
		const vector<NodePtr>& BasicGenerator::get##_id##Group() const {                                                                                       \
			return pimpl->get##_id##Group();                                                                                                                   \
		}

	#include "insieme/core/lang/inspire_api/lang.def"

	LiteralPtr BasicGenerator::getLiteral(const string& name) const {
		// check literals
		auto lIt = pimpl->literalMap.find(name);
		if(lIt != pimpl->literalMap.end()) { return ((*this).*lIt->second)(); }
		throw LiteralNotFoundException(name);
	}

	ExpressionPtr BasicGenerator::getBuiltIn(const string& name) const {
		// check derived
		auto dIt = pimpl->derivedMap.find(name);
		if(dIt != pimpl->derivedMap.end()) { return ((*this).*dIt->second)(); }
		// check literals
		return getLiteral(name);
	}


	bool BasicGenerator::isType(const NodePtr& type) const {
		return type->getNodeCategory() == NC_Type && analysis::isTypeLiteralType(type.as<TypePtr>());
	}

	bool BasicGenerator::isGen(const NodePtr& type) const {
		return type->getNodeCategory() == NC_Type && !isBool(type) && !isChar(type) && !isSignedInt(type) && !isUnsignedInt(type) && !isReal(type)
		       && !isType(type);
	}


	ExpressionPtr BasicGenerator::getOperator(const TypePtr& type, const BasicGenerator::Operator& op) const {
		{
			// try literals
			auto fit = pimpl->literalOperationMap.equal_range(op);
			for(auto it = fit.first, end = fit.second; it != end; ++it) {
				const auto& curr = *it;
				if(((*this).*curr.second.first)(type)) { return ((*this).*curr.second.second)(); }
			}
		}

		{
			// try derived
			auto fit = pimpl->derivedOperationMap.equal_range(op);
			for(auto it = fit.first, end = fit.second; it != end; ++it) {
				const auto& curr = *it;
				if(((*this).*curr.second.first)(type)) { return ((*this).*curr.second.second)(); }
			}
		}

		LOG(ERROR) << "Operation " << op << " of type " << type << " is not declared" << std::endl;
		assert_fail() << "Required combination of operator and type not declared";
		return 0;
	}

	BasicGenerator::Operator BasicGenerator::getOperator(const ExpressionPtr& lit) const {
		// We have to scan the multimap operationMap and find the operation which
		// has this literal as second argument.
		{
			// try literals
			auto& opMap = pimpl->literalOperationMap;
			auto fit = std::find_if(opMap.begin(), opMap.end(),
			                        [&](const BasicGeneratorImpl::LiteralOperationMap::value_type& cur) { return *((*this).*cur.second.second)() == *lit; });
			if(fit != opMap.end()) { return fit->first; }
		}
		{
			// try derived
			auto& opMap = pimpl->derivedOperationMap;
			auto fit = std::find_if(opMap.begin(), opMap.end(),
			                        [&](const BasicGeneratorImpl::DerivedOperationMap::value_type& cur) { return *((*this).*cur.second.second)() == *lit; });
			if(fit != opMap.end()) { return fit->first; }
		}
		assert_fail() << "Literal not found within the OperationMap, therefore not a valid IR literal expression";
		// should never happen, but eliminates compiler warning in release mode
		return BasicGenerator::Operator::Not;
	}
	// ----- extra material ---


	const BasicGenerator::SubTypeLattice* BasicGenerator::getSubTypeLattice() const {
		if(!subTypeLattice) {
			SubTypeLattice* res = new SubTypeLattice();

			// initialize lattice with generic relations from lang.def
			#define SUB_TYPE(_typeA, _typeB) res->addRelation(get##_typeA(), get##_typeB());
			#include "insieme/core/lang/inspire_api/lang.def"

			subTypeLattice = res;
		}
		return subTypeLattice;
	}


	TypeSet BasicGenerator::getDirectSuperTypesOf(const TypePtr& type) const {
		return getSubTypeLattice()->getSuperTypesOf(type);
	}

	TypeSet BasicGenerator::getDirectSubTypesOf(const TypePtr& type) const {
		return getSubTypeLattice()->getSubTypesOf(type);
	}


	std::ostream& operator<<(std::ostream& out, const BasicGenerator::Operator& op) {
		// clang-format off
		switch(op) {
		#define OPERATOR(_id, _str)                                                                                                                            \
			case BasicGenerator::Operator::_id: return out << #_str; 
		#include "insieme/core/lang/inspire_api/lang.def"
		}
		// clang-format on
		return out << "- unknown operator -";
	}

} // namespace lang
} // namespace core
} // namespace insieme
