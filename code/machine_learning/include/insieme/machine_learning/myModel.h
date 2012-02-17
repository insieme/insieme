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

#include <cassert>

#include "ReClaM/FFNet.h"
#include "ReClaM/Svm.h"

namespace insieme {
namespace ml {

/**
 * Baseclass for all wrapper classes around Shark model classes
 */
class MyModel {
public:
	virtual void model(const Array<double>& input, Array<double>& output) =0;

	virtual void modelDerivative(const Array<double>& input, Array<double>& derivative) =0;

	virtual void modelDerivative(const Array<double>& input, Array<double>& output, Array<double>& derivative) =0;

	virtual void generalDerivative(const Array<double>& input, const Array<double>& coefficient, Array<double>& derivative) =0;

	virtual bool isFeasible() =0;

	virtual double getParameter(unsigned int index) const =0;

	virtual void setParameter(unsigned int index, double value) =0;
/* protected
	Model* CloneI() {
		return shark.CloneI();
	}
*/
	virtual void read(std::istream& is) =0;
	virtual void load(const char* path) =0;

	virtual void write(std::ostream& os) const =0;
	virtual void save(const char* path) =0;

	//! Returns the dimension of the model input.
	virtual const unsigned int getInputDimension() const =0;

	//! Returns the dimension of the model output.
	virtual const unsigned int getOutputDimension() const =0;

	//! Returns the number of optimizable model parameters,
	//! i.e. the dimension of the parameter array.
	virtual const unsigned int getParameterDimension() const =0;

	// additional information provided -----------------------------------------------
	virtual const Array<int> getConnections() =0;

	virtual Model& getModel() =0;

	virtual const std::pair<double, double> getInitInterval() =0;
};


class MyFFNet : public MyModel {
private:
	std::pair<double, double> initInterval;
	FFNet shark;

public:

	//! Creates a feed-forward network by reading the necessary
	//! information from a file named "filename".
	MyFFNet(const std::string &filename) : shark(filename) {}

	//! Creates an empty feed-forward network with "in" input neurons and
	//! "out" output neurons.
	MyFFNet(const unsigned in = 0, const unsigned out = 0) : shark(in, out) {}

	//! Creates a feed-forward network with "in" input neurons and
	//! "out" output neurons. Additionally, the array "cmat" determines
	//! the topology (i.e., number of neurons and their connections).
	MyFFNet(const unsigned in, const unsigned out, const Array<int>& cmat) : shark(in, out, cmat) {}

	//! Creates a feed-forward network with "in" input neurons and
	//! "out" output neurons. Additionally, the arrays "cmat" and
	//! "wmat" determine the topology (i.e., number of neurons and their
	//! connections) as well as the connection weights.
	MyFFNet(const unsigned in, const unsigned out, const Array<int>& cmat, const Array<double>& wmat) : shark(in, out, cmat, wmat) {}

	Model& getModel() { return shark; }

	// Model virtual methods -------------------------------------------------

	void model(const Array<double>& input, Array<double>& output) {
		shark.model(input, output);
	}

	void modelDerivative(const Array<double>& input, Array<double>& derivative) {
		shark.modelDerivative(input, derivative);
	}

	void modelDerivative(const Array<double>& input, Array<double>& output, Array<double>& derivative) {
		shark.modelDerivative(input, output, derivative);
	}

	void generalDerivative(const Array<double>& input, const Array<double>& coefficient, Array<double>& derivative) {
		shark.generalDerivative(input, coefficient, derivative);
	}

	bool isFeasible() {
		return shark.isFeasible();
	}

	double getParameter(unsigned int index) const {
		return shark.getParameter(index);
	}

	void setParameter(unsigned int index, double value) {
		shark.setParameter(index, value);
	}
	//! Returns the dimension of the model input.
	const inline unsigned int getInputDimension() const
	{
		return shark.getInputDimension();
	}

	//! Returns the dimension of the model output.
	const inline unsigned int getOutputDimension() const
	{
		return shark.getOutputDimension();
	}

	//! Returns the number of optimizable model parameters,
	//! i.e. the dimension of the parameter array.
	const inline unsigned int getParameterDimension() const
	{
		return shark.getParameterDimension();
	}

	inline void setEpsilon(double eps)
	{
		shark.setEpsilon(eps);
	};

/* private
	Model* CloneI() {
		return shark.CloneI();
	}
*/
	void read(std::istream& is) {
		shark.read(is);
	}
	void load(const char* path) {
		shark.load(path);
	}


	void write(std::ostream& os) const {
		shark.write(os);
	}
	void save(const char* path) {
		shark.save(path);
	}

	// FFNet virtual methods --------------------------------------------------------------------

	void initWeights(long seed = 42, double l = -.5, double h = .5) {
		initInterval = std::make_pair(l,h);
		shark.initWeights(seed, l, h);
	}

	void initWeights(double l = -.5, double h = .5) {
		initInterval = std::make_pair(l,h);
		shark.initWeights(l, h);
	}
/* private
	double  g(double a) {
		return shark.g(a);
	}

	double  dg(double a) {
		return shark.dg(a);
	}

	double  gOutput(double a) {
		return shark.gOutput(a);
	}

	double  dgOutput(double a) {
		return shark.dgOutput(a);
	}

	void resize() {
		shark.resize();
	}
*/
	// additional information provided -----------------------------------------------
	const Array<int> getConnections() {
		return shark.getConnections();
	}

	const std::pair<double, double> getInitInterval() {
		return initInterval;
	}
};

class MySVM : public MyModel {
private:
	SVM shark;
public:
	//! Constructor
	//!
	//! \param  pKernel	  kernel function to use for training and prediction
	//! \param  bSignOutput  true if the SVM should output binary labels, false if it should output real valued function evaluations
	MySVM(KernelFunction* pKernel, bool bSignOutput = false) : shark(pKernel, bSignOutput) {}

	//! Constructor
	//!
	//! \param  pKernel	  kernel function to use for training and prediction
	//! \param  input		training data points
	//! \param  bSignOutput  true if the SVM should output binary labels, false if it should output real valued function evaluations
	MySVM(KernelFunction* pKernel, const Array<double>& input, bool bSignOutput = false) : shark(pKernel, input, bSignOutput) { }

	Model& getModel() { return shark; }

	// Model virtual methods -------------------------------------------------

	void model(const Array<double>& input, Array<double>& output) {
		shark.model(input, output);
	}

	void modelDerivative(const Array<double>& input, Array<double>& derivative) {
		shark.modelDerivative(input, derivative);
	}

	void modelDerivative(const Array<double>& input, Array<double>& output, Array<double>& derivative) {
		shark.modelDerivative(input, output, derivative);
	}

	void generalDerivative(const Array<double>& input, const Array<double>& coefficient, Array<double>& derivative) {
		shark.generalDerivative(input, coefficient, derivative);
	}

	bool isFeasible() {
		return shark.isFeasible();
	}

	double getParameter(unsigned int index) const {
		return shark.getParameter(index);
	}

	void setParameter(unsigned int index, double value) {
		setParameter(index, value);
	}
/* protected
	Model* CloneI() {
		return shark.CloneI();
	}
*/
	void read(std::istream& is) {
		shark.read(is);
	}
	virtual void load(const char* path) {
		shark.load(path);
	}


	void write(std::ostream& os) const {
		shark.write(os);
	}
	void save(const char* path) {
		shark.save(path);
	}

	// additional information prowided -----------------------------------------------
	const Array<int> getConnections() {
		Array<int> ret(1);
		ret[0] = -1;
		return ret;
	}

	const std::pair<double, double> getInitInterval() {
		return std::make_pair(shark.getAlpha(0), shark.getAlpha(1));
	}


};

} // end namespace ml
} // end namespcae insieme
