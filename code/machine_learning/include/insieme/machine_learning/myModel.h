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

/*
 * myModel.h
 *
 *  Created on: Feb 16, 2012
 *      Author: klois
 */

#pragma once

#include "ReClaM/FFNet.h"
#include "ReClaM/Svm.h"

namespace insieme {
namespace ml {

/**
 * Baseclass for all wrapper classes around Shark model classes
 */
class MyModel : public Model {
	// additional information prowided -----------------------------------------------
	virtual const Array<int> getConnections() =0;

	virtual const std::pair<double, double> getInitInterval() =0;
};


class MyFFNet : public MyModel, public FFNet {
private:
	std::pair<double, double> initInterval;

public:

	//! Creates a feed-forward network by reading the necessary
	//! information from a file named "filename".
	MyFFNet(const std::string &filename) : FFNet(filename) {}

	//! Creates an empty feed-forward network with "in" input neurons and
	//! "out" output neurons.
	MyFFNet(const unsigned in = 0, const unsigned out = 0) : FFNet(in, out) {}

	//! Creates a feed-forward network with "in" input neurons and
	//! "out" output neurons. Additionally, the array "cmat" determines
	//! the topology (i.e., number of neurons and their connections).
	MyFFNet(const unsigned in, const unsigned out, const Array<int>& cmat) : FFNet(in, out, cmat) {}

	//! Creates a feed-forward network with "in" input neurons and
	//! "out" output neurons. Additionally, the arrays "cmat" and
	//! "wmat" determine the topology (i.e., number of neurons and their
	//! connections) as well as the connection weights.
	MyFFNet(const unsigned in, const unsigned out, const Array<int>& cmat, const Array<double>& wmat) : FFNet(in, out, cmat, wmat) {}


	// Model virtual methods -------------------------------------------------

	void model(const Array<double>& input, Array<double>& output) {
		FFNet::model(input, output);
	}

	void modelDerivative(const Array<double>& input, Array<double>& derivative) {
		FFNet::modelDerivative(input, derivative);
	}

	void modelDerivative(const Array<double>& input, Array<double>& output, Array<double>& derivative) {
		FFNet::modelDerivative(input, output, derivative);
	}

	void generalDerivative(const Array<double>& input, const Array<double>& coefficient, Array<double>& derivative) {
		FFNet::generalDerivative(input, coefficient, derivative);
	}

	bool isFeasible() {
		return FFNet::isFeasible();
	}

	double getParameter(unsigned int index) const {
		return FFNet::getParameter(index);
	}

	void setParameter(unsigned int index, double value) {
		FFNet::setParameter(index, value);
	}

	Model* CloneI() {
		return FFNet::CloneI();
	}

	void read(std::istream& is) {
		FFNet::read(is);
	}

	void write(std::ostream& os) const {
		FFNet::write(os);
	}

	// FFNet virtual methods --------------------------------------------------------------------

	void initWeights(long seed = 42, double l = -.5, double h = .5) {
		initInterval = std::make_pair(l,h);
		FFNet::initWeights(seed, l, h);
	}

	void initWeights(double l = -.5, double h = .5) {
		initInterval = std::make_pair(l,h);
		FFNet::initWeights(l, h);
	}

	double  g(double a) {
		return FFNet::g(a);
	}

	double  dg(double a) {
		return FFNet::dg(a);
	}

	double  gOutput(double a) {
		return FFNet::gOutput(a);
	}

	double  dgOutput(double a) {
		return FFNet::dgOutput(a);
	}

	void resize() {
		FFNet::resize();
	}

	// additional information prowided -----------------------------------------------
	const Array<int> getConnections() {
		return FFNet::getConnections();
	}

	const std::pair<double, double> getInitInterval() {
		return initInterval;
	}
};

class MySVM : public MyModel, public SVM {
public:
	//! Constructor
	//!
	//! \param  pKernel	  kernel function to use for training and prediction
	//! \param  bSignOutput  true if the SVM should output binary labels, false if it should output real valued function evaluations
	MySVM(KernelFunction* pKernel, bool bSignOutput = false) : SVM(pKernel, bSignOutput) {
		SVM::parameter[0] = 0;
	}

	//! Constructor
	//!
	//! \param  pKernel	  kernel function to use for training and prediction
	//! \param  input		training data points
	//! \param  bSignOutput  true if the SVM should output binary labels, false if it should output real valued function evaluations
	MySVM(KernelFunction* pKernel, const Array<double>& input, bool bSignOutput = false) : SVM(pKernel, input, bSignOutput) {
		SVM::parameter[0] = 0;
	}

	// Model virtual methods -------------------------------------------------

	void model(const Array<double>& input, Array<double>& output) {
		SVM::model(input, output);
	}

	void modelDerivative(const Array<double>& input, Array<double>& derivative) {
		SVM::modelDerivative(input, derivative);
	}

	void modelDerivative(const Array<double>& input, Array<double>& output, Array<double>& derivative) {
		SVM::modelDerivative(input, output, derivative);
	}

	void generalDerivative(const Array<double>& input, const Array<double>& coefficient, Array<double>& derivative) {
		SVM::generalDerivative(input, coefficient, derivative);
	}

	bool isFeasible() {
		return SVM::isFeasible();
	}

	double getParameter(unsigned int index) const {
		return SVM::getParameter(index);
	}

	void setParameter(unsigned int index, double value) {
		setParameter(index, value);
	}

	Model* CloneI() {
		return SVM::CloneI();
	}

	void read(std::istream& is) {
		SVM::read(is);
	}

	void write(std::ostream& os) const {
		SVM::write(os);
	}

	// additional information prowided -----------------------------------------------
	const Array<int> getConnections() {
		Array<int> ret(1);
		ret[0] = -1;
		return ret;
	}

	const std::pair<double, double> getInitInterval() {
		return std::make_pair(SVM::getAlpha(0), SVM::getAlpha(1));
	}


};

} // end namespace ml
} // end namespcae insieme
