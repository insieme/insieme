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

#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/framework/constraint_generator.h"

#include "insieme/utils/int_type.h"

namespace insieme {
namespace analysis {
namespace cba {


	// -------------------- Constraint Generator ---------------------------

	// declaration of utility function
	template<typename Context> bool isValidContext(CBA& cba, const Context& context);

	/**
	 * A base class for constraint generator implementations computing values of expressions.
	 *
	 * Essentially, this kind of constraint generator is an IR visitor generating for a given node (addressed by its
	 * address) and a call / thread context constraints. In general, every resolver is only supposed to
	 * generate in-constraints for the requested target set specified by the address and context parameter.
	 */
	template<typename Context, typename ... ExtraParams>
	class DataValueConstraintGenerator :
			public ConstraintGenerator,
			public core::IRVisitor<void, core::Address, const Context&, const ExtraParams& ..., Constraints&> {

		// a short-cut for the base class
		typedef core::IRVisitor<void, core::Address, const Context&, const ExtraParams& ..., Constraints&> super;

		/**
		 * The base-implementation is preventing the same arguments to be processed multiple times.
		 */
		std::set<ValueID> processed;

	protected:

		/**
		 * The analysis context this resolver is working for. Every instance may only be utilized by
		 * a single CBA instance.
		 */
		CBA& cba;

	public:

		DataValueConstraintGenerator(CBA& cba)
			: processed(), cba(cba) {}

	private:

		template<int i, typename Tuple, typename ... Args>
		void resolve(Constraints& constraints, const utils::int_type<i>& c, const Tuple& t, const Args& ... args) {
			resolve(constraints, utils::int_type<i+1>(), t, args..., std::get<i>(t));
		}

		template<typename Tuple, typename ... Args>
		void resolve(Constraints& constraints, const utils::int_type<sizeof...(ExtraParams)+3>& c, const Tuple& t, const Args& ... args) {
			visit(args..., constraints);
		}

		template<int i, typename Tuple>
		void printParams(std::ostream& out, const utils::int_type<i>& c, const Tuple& t) const {
			out << std::get<i>(t) << ",";
			printParams(out, utils::int_type<i+1>(), t);
		}

		template<typename Tuple>
		void printParams(std::ostream& out, const utils::int_type<sizeof...(ExtraParams)+2>& c, const Tuple& t) const {
			out << std::get<sizeof...(ExtraParams)+2>(t);
		}

	public:

		/**
		 * The main entry point for the constraint resolution. The function is resolving the given
		 * value id into the represented address and context value and initiates the constraint resolution
		 * process by invoking the visit function handling it.
		 *
		 * @param cba the analysis context to be utilized for the processing
		 * @param value the value for which constraints shell be generated
		 * @param constraints the set to which the resulting constraints should be added
		 */
		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {

			// resolve the targeted node address, program context string and extra values
			auto& data = cba.getValueParameters<int, Context, ExtraParams...>(value);

			// resolve node address (need to convert label to stmt)
			const core::NodeAddress& node = cba.getStmt(std::get<1>(data));

			// resolve the rest recursively and trigger the visit function
			resolve(constraints, utils::int_type<2>(), data, node);
		}

		/**
		 * Produces a human-readable representation of the value represented by the given value ID.
		 */
		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {

			auto& data = cba.getValueParameters<int,Context,ExtraParams...>(value);
			int label = std::get<1>(data);
			const core::NodeAddress& node = cba.getStmt(label);

			out << value << " = " << getAnalysisName(std::get<0>(data)) <<
					"[l" << label << " = " << node->getNodeType() << " : "
						 << node;// << " = " << core::printer::PrettyPrinter(node, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE) << " : ";

			// print remaining set parameters (including context)
			printParams(out, utils::int_type<2>(), data);

			// done
			out << "]";
		}


		/**
		 * Overrides the standard visit function of the super type and realizes the guard avoiding the
		 * repeated evaluation of identical argument types.
		 */
		virtual void visit(const core::NodeAddress& node, const Context& ctxt, const ExtraParams& ... args, Constraints& constraints) {

			// filter out invalid contexts
			if (!isValidContext(cba, ctxt)) return;

			// for valid content => std procedure
			visitInternal(node, ctxt, args..., constraints);
		}

	protected:

		/**
		 * An entry point to be intersected in case sub-classes would like to customize the entry point of the
		 * constraint resolution process (after the cache and ctxt has been checked).
		 */
		virtual void visitInternal(const core::NodeAddress& node, const Context& ctxt, const ExtraParams& ... args, Constraints& constraints) {
			// by default, just forward call to visit
			super::visit(node, ctxt, args..., constraints);
		}

		/**
		 * Provides access to the context
		 */
		CBA& getCBA() {
			return cba;
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
