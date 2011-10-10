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

#include <memory>
#include <vector>
#include "insieme/core/ast_address.h"
#include "insieme/core/forward_decls.h"

namespace insieme {
namespace transform {

	/**
	 * NOTES:
	 * This header file defines the basic interface for transformations and
	 * some of the routines to be used for applying those. Further header files
	 * are defining concrete transformations or transformation connectors.
	 *
	 * Ideas:
	 * 	- prohibit direct creation of transformation instances => use factories
	 */



	/**
	 * The common abstract base class / interface for all transformations handled
	 * within the Insieme Transformation Framework.
	 */
	class Transformation {

	public:

		/**
		 * Tests whether this transformation can be applied to the given target. If
		 * the given node is a valid target for the transformation, true will be
		 * returned, false otherwise.
		 *
		 * TODO: figure out how this is best used => adjust semantic
		 *
		 * @param target the target to be tested
		 * @return true if it is a valid target, false otherwise
		 */
		virtual bool checkPreCondition(const core::NodePtr& target) const =0;

		/**
		 * Requests this transformation to be applied on the given target. The result
		 * should be the transformed version of the node. During the transformation, the
		 * node manager of the given node will be used to create new instances of IR nodes.
		 *
		 * The transformation conducted during this step is requested to be deterministic and pure.
		 * Hence, given the same input, the same result is produced and no global state will be able to
		 * effect the transformations behavior.
		 *
		 * @param target the node to be transformed
		 * @return the transformed node
		 * @throws InvalidTargetException if this transformation can not be applied to the given target
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const =0;

		/**
		 * Tests whether the transformation has been successful by converting the given before into the
		 * given after state.
		 *
		 * @param before the state of the program before the transformation
		 * @param after the state after the transformation
		 * @return true if the transformation was carried out successfully, false otherwise
		 */
		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const =0;

	};

	/**
	 * A pointer type used to address transformations uniformely.
	 */
	typedef std::shared_ptr<Transformation> TransformationPtr;


	/**
	 * An exception which will be raised in case a transformation can not be applied to
	 * a certain target node.
	 */
	class InvalidTargetException : public std::exception {

		/**
		 * A message describing the cause of the problem.
		 */
		std::string msg;

	public:
		InvalidTargetException(const string& cause) : msg(cause) {};
		InvalidTargetException(const core::NodePtr& node);
		virtual ~InvalidTargetException() throw() { }
		virtual const char* what() const throw() { return msg.c_str(); }
	};


//	class Transformation {
//	public:
//		virtual core::NodeAddress apply(const core::NodeAddress& target) =0;
//		virtual bool checkPrecondition(const core::NodeAddress& target) =0;
//		virtual bool checkPostcondition(const core::NodeAddress& before, const core::NodeAddress& after) =0;
//	};
//
//	typedef std::shared_ptr<Transformation> TransformationPtr;
//
//	// ------------------------------------------------------------
//	class RepresentationType {
//	protected:
//		std::string name;
//
//	public:
//		virtual std::string getName() { return name; }
//		};
//
//	class ParameterType {
//	protected:
//		std::string name;
//		ParameterType* supertype;
//		RepresentationType* representationtype;
//
//	public:
//		virtual std::string getName() { return name; }
//		virtual ParameterType* getSuperType() { return supertype; }
//		virtual RepresentationType* getRepresentationType() { return representationtype; }
//		};
//	// ------------------------------------------------------------
//
//	// ------------------------------------------------------------
//	class Representation {
//	public:
//		virtual RepresentationType* getRepresentationType() =0;
//		};
//
//	class Parameter {
//	public:
//		virtual ParameterType* getType() =0;
//		};
//	// ------------------------------------------------------------
//
//	// ------------------------------------------------------------
//	class IntegerRepresentationType: public RepresentationType {
//		public: IntegerRepresentationType();
//		};
//
//	class IntegerParameterType: public ParameterType {
//		public: IntegerParameterType();
//		};
//
//	class UnrollingDepthParameterType: public ParameterType {
//		public: UnrollingDepthParameterType();
//		};
//	// ------------------------------------------------------------
//
//	// ------------------------------------------------------------
//	class Types {
//	public:
//		static IntegerRepresentationType integerRepresentationType;
//		static IntegerParameterType integerParameterType;
//		static UnrollingDepthParameterType unrollingDepthParameterType;
//		};
//	// ------------------------------------------------------------
//
//	// ------------------------------------------------------------
//	class IntegerRepresentation: public Representation {
//	// ------------------------------------------------------------
//		protected:
//		IntegerRepresentation(int i):i(i) { }
//
//		virtual RepresentationType* getRepresentationType() {
//			return &Types::integerRepresentationType;
//			}
//		int i;
//		};
//	// ------------------------------------------------------------
//	class IntegerParameter: public IntegerRepresentation, public Parameter {
//	// ------------------------------------------------------------
//		protected:
//		IntegerParameter(int i):IntegerRepresentation(i) { }
//
//		virtual ParameterType* getType() {
//			return &Types::integerParameterType;
//			}
//		};
//	// ------------------------------------------------------------
//	class UnrollingDepthParameter: public IntegerParameter {
//	// ------------------------------------------------------------
//		public:
//		UnrollingDepthParameter(int i):IntegerParameter(i) { }
//
//		virtual ParameterType* getType() {
//			return &Types::unrollingDepthParameterType;
//			}
//		};
//
//	// ------------------------------------------------------------
//	class LoopUnrolling: public Transformation {
//
//		UnrollingDepthParameter* d;
//
//		public:
//		LoopUnrolling(UnrollingDepthParameter* d):d(d) { }
//
// 		std::vector<Parameter*> getParameters() {
//			return std::vector<Parameter*>({d});
//			}
//
//		virtual core::NodeAddress apply(const core::NodeAddress& target) { return core::NodeAddress(); };
//		virtual bool checkPrecondition(const core::NodeAddress& target) { return false; };
//		virtual bool checkPostcondition(const core::NodeAddress& before) { return false; };
//
//		};
//
//	// ------------------------------------------------------------

} // end namespace transform
} // end namespace insieme
