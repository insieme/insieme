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
#include "insieme/core/datapath/datapath.h"

#include "insieme/utils/logging.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace datapath {


	DataPath::DataPath(const TypePtr& type) {
		auto& mgr = type.getNodeManager();
		auto& ext = mgr.getLangExtension<lang::DatapathExtension>();
		IRBuilder builder(mgr);
		path = builder.callExpr(ext.getDataPathRoot(), builder.getTypeLiteral(type));
	}

	DataPath DataPath::member(const string& name) const {
		auto& mgr = path.getNodeManager();
		auto& ext = mgr.getLangExtension<lang::DatapathExtension>();
		assert_true(getTargetType().isa<TagTypePtr>()) << "Current target must be a tag type!";
		auto elementType = getTargetType().as<TagTypePtr>()->getFieldType(name);
		assert_true(elementType) << "No member " << name << " in type " << *getTargetType() << "\n";
		IRBuilder builder(mgr);
		return DataPath(builder.callExpr(ext.getDataPathMember(), path, builder.getIdentifierLiteral(name), builder.getTypeLiteral(elementType)));
	}

	DataPath DataPath::element(const ExpressionPtr& element) const {
		auto& mgr = path.getNodeManager();
		auto& ext = mgr.getLangExtension<lang::DatapathExtension>();
		assert_true(mgr.getLangBasic().isSignedInt(element->getType())) << "Index has to be a signed integer!";
		return DataPath(IRBuilder(mgr).callExpr(ext.getDataPathElement(), path, element));
	}

	DataPath DataPath::element(unsigned index) const {
		return element(IRBuilder(path.getNodeManager()).intLit(index).as<ExpressionPtr>());
	}

	DataPath DataPath::component(unsigned index) const {
		auto& mgr = path.getNodeManager();
		auto& ext = mgr.getLangExtension<lang::DatapathExtension>();
		assert_true(getTargetType().isa<TupleTypePtr>()) << "Current target must be a tuple type!";
		assert_lt(index, getTargetType().as<TupleTypePtr>().size());
		auto elementType = getTargetType().as<TupleTypePtr>()[index];
		IRBuilder builder(mgr);
		return DataPath(builder.callExpr(ext.getDataPathComponent(), path, builder.uintLit(index), builder.getTypeLiteral(elementType)));
	}

	DataPath DataPath::parent(const TypePtr& type) const {
		auto& mgr = path.getNodeManager();
		auto& ext = mgr.getLangExtension<lang::DatapathExtension>();
		IRBuilder builder(mgr);
		return DataPath(builder.callExpr(ext.getDataPathParent(), path, builder.getTypeLiteral(type)));
	}

	TypePtr DataPath::getSourceType() const {
		return path->getType().as<GenericTypePtr>().getTypeParameter(0);
	}

	TypePtr DataPath::getTargetType() const {
		return path->getType().as<GenericTypePtr>().getTypeParameter(1);
	}


	namespace {

		/**
		 * The printer used to present data paths in a nice representation.
		 */
		class DataPathPrinter : public core::IRVisitor<void, Pointer, std::ostream&> {
		  public:
			/**
			 * Handle steps along the data path.
			 */
			void visitCallExpr(const CallExprPtr& call, std::ostream& out) {
				auto& ext = call.getNodeManager().getLangExtension<lang::DatapathExtension>();
				const auto& fun = call->getFunctionExpr();

				// handle root step
				if(ext.isDataPathRoot(fun)) {
					out << "<" << *(call->getType().as<GenericTypePtr>()->getTypeParameter(0)) << ">";
					return;
				}

				// visit in post-fix order
				visit(call->getArgument(0), out);

				// handle member accesses
				if(ext.isDataPathMember(fun)) {
					out << "." << core::printer::PrettyPrinter(call->getArgument(1));
					return;
				}

				// handle element accesses
				if(ext.isDataPathElement(fun)) {
					out << "[" << core::printer::PrettyPrinter(call->getArgument(1)) << "]";
					return;
				}

				// handle component accesses
				if(ext.isDataPathComponent(fun)) {
					out << ".c" << core::printer::PrettyPrinter(call->getArgument(1));
					return;
				}

				// handle pre-fix notation of parent access
				if(ext.isDataPathParent(fun)) {
					out << ".as<" << core::printer::PrettyPrinter(call->getArgument(1)->getType().as<GenericTypePtr>().getTypeParameter(0)) << ">";
					return;
				}

				LOG(FATAL) << "Invalid data path encountered: " << core::printer::PrettyPrinter(call);
				assert_fail() << "Invalid Data Path encountered!";
			}
		};
	}


	std::ostream& DataPath::printTo(std::ostream& out) const {
		static DataPathPrinter printer;
		printer.visit(path, out);
		return out;
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
