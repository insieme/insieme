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

/** Shark error functions do not provide any infomration about themself. Therefore a warapper around all error functions is created, to provide functions such
 * as getName()
 */
#pragma once

#include "ReClaM/BFGS.h"
#include "ReClaM/CG.h"
#include "ReClaM/Rprop.h"
#include "ReClaM/Quickprop.h"

#include "ReClaM/Svm.h"

namespace insieme {
namespace ml {

/**
 * Baseclass for all wrapper classes around Shark optimizer classes
 */
class MyOptimizer: public Optimizer {
public:
	virtual const std::string getName() =0;
};

class MyBFGS : public MyOptimizer, public BFGS {
public:
	const std::string getName() { return "BFGS"; }
	void init(Model& model) { BFGS::init(model); }
	double optimize(Model& model, ErrorFunction& errorfunction, const Array<double>& input, const Array<double>& target) {
		return BFGS::optimize(model, errorfunction, input, target);
	}
};

class MyCG : public MyOptimizer, public CG {
public:
	const std::string getName() { return "CG"; }
	void init(Model& model) { CG::init(model); }
	double optimize(Model& model, ErrorFunction& errorfunction, const Array<double>& input, const Array<double>& target) {
		return CG::optimize(model, errorfunction, input, target);
	}
};

class MyRpropMinus : public MyOptimizer, public RpropMinus {
public:
	const std::string getName() { return "Rprop-"; }
	void init(Model& model) { RpropMinus::init(model); }
	double optimize(Model& model, ErrorFunction& errorfunction, const Array<double>& input, const Array<double>& target) {
		return RpropMinus::optimize(model, errorfunction, input, target);
	}
};

class MyRpropPlus : public MyOptimizer, public RpropPlus {
public:
	const std::string getName() { return "Rprop+"; }
	void init(Model& model) { RpropPlus::init(model); }
	double optimize(Model& model, ErrorFunction& errorfunction, const Array<double>& input, const Array<double>& target) {
		return RpropPlus::optimize(model, errorfunction, input, target);
	}
};

class MyQuickprop : public MyOptimizer, public Quickprop {
public:
	const std::string getName() { return "Quickprop"; }
	void init(Model& model) { Quickprop::init(model); }
	double optimize(Model& model, ErrorFunction& errorfunction, const Array<double>& input, const Array<double>& target) {
		return Quickprop::optimize(model, errorfunction, input, target);
	}
};

class MySVM_Optimizer : public MyOptimizer, public SVM_Optimizer {
public:
	const std::string getName() { return "SVM Optimizer"; }
	void init(Model& model) { SVM_Optimizer::init(model); }
	double optimize(Model& model, ErrorFunction& errorfunction, const Array<double>& input, const Array<double>& target) {
		return SVM_Optimizer::optimize(model, errorfunction, input, target);
	}
};


} // end namespace ml
} // end namespace insieme
