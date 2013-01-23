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

#include "insieme/core/datapath/datapath.h"

#include "insieme/utils/logging.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace datapath {


	DataPath::DataPath(NodeManager& manager)
		: path(manager.getLangBasic().getDataPathRoot()) { }

	DataPath DataPath::member(const ExpressionPtr& member) const {
		auto& mgr = path.getNodeManager(); auto& basic = mgr.getLangBasic();
		assert(basic.isIdentifier(member->getType()) && "Member identifier has to be an identifier!");
		return DataPath(IRBuilder(mgr).callExpr(basic.getDataPath(), basic.getDataPathMember(), path, member));
	}

	DataPath DataPath::member(const string& name) const {
		return member(IRBuilder(path.getNodeManager()).getIdentifierLiteral(name));
	}

	DataPath DataPath::element(const ExpressionPtr& element) const {
		auto& mgr = path.getNodeManager(); auto& basic = mgr.getLangBasic();
		assert(basic.isUnsignedInt(element->getType()) && "Index has to be an unsigned integer!");
		return DataPath(IRBuilder(mgr).callExpr(basic.getDataPath(), basic.getDataPathElement(), path, element));
	}

	DataPath DataPath::element(unsigned index) const {
		return element(IRBuilder(path.getNodeManager()).uintLit(index).as<ExpressionPtr>());
	}

	DataPath DataPath::component(const LiteralPtr& component) const {
		auto& mgr = path.getNodeManager(); auto& basic = mgr.getLangBasic();
		assert(basic.isUnsignedInt(component->getType()) && "Index has to be an unsigned integer!");
		return DataPath(IRBuilder(mgr).callExpr(basic.getDataPath(), basic.getDataPathComponent(), path, component));
	}

	DataPath DataPath::component(unsigned index) const {
		return component(IRBuilder(path.getNodeManager()).uintLit(index));
	}

	DataPath DataPath::parent(const TypePtr& type) const {
		auto& mgr = path.getNodeManager();
		auto& basic = mgr.getLangBasic();
		IRBuilder builder(mgr);
		return DataPath(builder.callExpr(basic.getDataPath(), basic.getDataPathParent(), path, builder.getTypeLiteral(type)));
	}

	namespace {

		/**
		 * The printer used to present data paths in a nice representation.
		 */
		class DataPathPrinter : public core::IRVisitor<void,Pointer,std::ostream&> {

		public:

			/**
			 * Handle the literal forming the root node.
			 */
			void visitLiteral(const LiteralPtr& literal, std::ostream& out) {
				assert(literal.getNodeManager().getLangBasic().isDataPathRoot(literal)
						&& "Invalid literal encountered within data path!");
				out << "<>";
			}

			/**
			 * Handle steps along the data path.
			 */
			void visitCallExpr(const CallExprPtr& call, std::ostream& out) {
				auto& basic = call.getNodeManager().getLangBasic();
				const auto& fun = call->getFunctionExpr();


				// visit in post-fix order
				visit(call->getArgument(0), out);

				// handle member accesses
				if (basic.isDataPathMember(fun)) {
					out << "." << core::printer::PrettyPrinter(call->getArgument(1));
					return;
				}

				// handle element accesses
				if (basic.isDataPathElement(fun)) {
					out << "[" << core::printer::PrettyPrinter(call->getArgument(1)) << "]";
					return;
				}

				// handle component accesses
				if (basic.isDataPathComponent(fun)) {
					out << ".c" << core::printer::PrettyPrinter(call->getArgument(1));
					return;
				}

				// handle pre-fix notation of parent access
				if (basic.isDataPathParent(fun)) {
					out << ".as<" << core::printer::PrettyPrinter(call->getArgument(1)->getType().as<GenericTypePtr>().getTypeParameter(0)) << ">";
					return;
				}

				LOG(FATAL) << "Invalid data path encountered: " << core::printer::PrettyPrinter(call);
				assert(false && "Invalid Data Path encountered!");

			}

		};

	}



	std::ostream& DataPath::printTo(std::ostream& out) const {
		static DataPathPrinter printer;
		printer.visit(path, out);
		return out;
	}



	DataPathBuilder& DataPathBuilder::member(const ExpressionPtr& member) {
		path = path.member(member);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::member(const string& name) {
		path = path.member(name);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::element(const ExpressionPtr& element) {
		path = path.element(element);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::element(unsigned index) {
		path = path.element(index);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::component(const LiteralPtr& component) {
		path = path.component(component);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::component(unsigned index) {
		path = path.component(index);
		return *this;
	}

	DataPathBuilder& DataPathBuilder::parent(const TypePtr& type) {
		path = path.parent(type);
		return *this;
	}


} // end namespace datapath
} // end namespace core
} // end namespace insieme
