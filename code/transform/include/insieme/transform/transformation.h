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

	class Transformation {
	public:
		virtual core::NodeAddress apply(const core::NodeAddress& target) =0;
		virtual bool checkPrecondition(const core::NodeAddress& target) =0;
		virtual bool checkPostcondition(const core::NodeAddress& before, const core::NodeAddress& after) =0;
	};

	typedef std::shared_ptr<Transformation> TransformationPtr;

	// ------------------------------------------------------------
	class RepresentationType {
	protected: 
		std::string name;

	public:
		virtual std::string getName() { return name; }
		};

	class ParameterType {
	protected: 
		std::string name;
		ParameterType* supertype;
		RepresentationType* representationtype;

	public:
		virtual std::string getName() { return name; }
		virtual ParameterType* getSuperType() { return supertype; }
		virtual RepresentationType* getRepresentationType() { return representationtype; }
		};
	// ------------------------------------------------------------

	// ------------------------------------------------------------
	class Representation {
	public:
		virtual RepresentationType* getRepresentationType() =0;
		};

	class Parameter {
	public:
		virtual ParameterType* getType() =0;
		};
	// ------------------------------------------------------------

	// ------------------------------------------------------------
	class IntegerRepresentationType: public RepresentationType { 
		public: IntegerRepresentationType(); 
		};

	class IntegerParameterType: public ParameterType { 
		public: IntegerParameterType(); 
		};

	class UnrollingDepthParameterType: public ParameterType { 
		public: UnrollingDepthParameterType(); 
		};
	// ------------------------------------------------------------

	// ------------------------------------------------------------
	class Types {
	public:
		static IntegerRepresentationType* integerRepresentationType;
		static IntegerParameterType* integerParameterType;
		static UnrollingDepthParameterType* unrollingDepthParameterType;
		};
	// ------------------------------------------------------------

	// ------------------------------------------------------------
	class IntegerRepresentation: public Representation {
	// ------------------------------------------------------------
		protected:
		IntegerRepresentation(int i):i(i) { }

		virtual RepresentationType* getRepresentationType() {
			return Types::integerRepresentationType;
			}
		int i;
		};
	// ------------------------------------------------------------
	class IntegerParameter: public IntegerRepresentation, public Parameter {
	// ------------------------------------------------------------
		protected:
		IntegerParameter(int i):IntegerRepresentation(i) { }

		virtual ParameterType* getType() {
			return Types::integerParameterType;
			}
		};
	// ------------------------------------------------------------
	class UnrollingDepthParameter: public IntegerParameter {
	// ------------------------------------------------------------
		public:
		UnrollingDepthParameter(int i):IntegerParameter(i) { }

		virtual ParameterType* getType() {
			return Types::unrollingDepthParameterType;
			}
		};

	// ------------------------------------------------------------
	class LoopUnrolling: public Transformation {

		UnrollingDepthParameter* d; 

		public:
		LoopUnrolling(UnrollingDepthParameter* d):d(d) { } 

 		std::vector<Parameter*> getParameters() { 
			return std::vector<Parameter*>({d});
			}

		virtual core::NodeAddress apply(const core::NodeAddress& target) { return core::NodeAddress(); };
		virtual bool checkPrecondition(const core::NodeAddress& target) { return false; };
		virtual bool checkPostcondition(const core::NodeAddress& before) { return false; };

		};

	// ------------------------------------------------------------
}

}
