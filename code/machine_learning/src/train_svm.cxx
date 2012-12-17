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

#include "ReClaM/Svm.h"
#include "ReClaM/MeanSquaredError.h"
#include "ReClaM/GaussKernel.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/machine_learning/myModel.h"

#define SVM
#include "insieme/machine_learning/cmd_line_utils.h"
#undef SVM
#include "insieme/machine_learning/trainer.h"

using namespace insieme::ml;

void writeModel(Trainer* trainer) {
	if(TrainCmdOptions::OutputPath.size() == 0)
		TrainCmdOptions::OutputPath = ".";

	if(TrainCmdOptions::OutputModel.size() == 0){
		std::stringstream str;
		str << "model_" << + trainer->getModel().getInputDimension() << "_" << trainer->getModel().getOutputDimension();
		TrainCmdOptions::OutputModel = str.str();
	}

	trainer->saveModel(TrainCmdOptions::OutputModel, TrainCmdOptions::OutputPath);
}

size_t numberOfFeatures() {
	return TrainCmdOptions::SFeatureNames.size() + TrainCmdOptions::SFeatures.size()
		 + TrainCmdOptions::DFeatureNames.size() + TrainCmdOptions::DFeatures.size()
		 + TrainCmdOptions::PFeatureNames.size() + TrainCmdOptions::PFeatures.size();
}

typedef std::shared_ptr<KernelFunction> KernelFunctionPtr;
typedef std::shared_ptr<MyModel> MyModelPtr;

template<typename T>
T convertTo(std::string str, std::string start, std::string end) {
	return insieme::utils::numeric_cast<T>(str.substr(str.find(start)+1, str.find(end)-str.find(start)-1));
}

KernelFunctionPtr strToKernel(std::string argString) {
	if(argString.empty())	return std::make_shared<RBFKernel>(1.0);
	if(argString.find("LinearKernel") == 0)	return std::make_shared<LinearKernel>();
	if(argString.find("PolynimialKernel") == 0)  return std::make_shared<PolynomialKernel>
			(convertTo<int>(argString, "[", ","), convertTo<double>(argString, ",", "]"));
	if(argString.find("RBFKernel") == 0) {
		std::cout << "setting " << argString << " to " << convertTo<double>(argString, "[", "]") << std::endl;
		return std::make_shared<RBFKernel>(convertTo<double>(argString, "[", "]"));
	}
	if(argString.find("DiagGaussKernel") == 0)  return std::make_shared<DiagGaussKernel>
			(convertTo<int>(argString, "[", ","), convertTo<double>(argString, ",", "]"));
	if(argString.find("GeneralGaussKernel") == 0)  return std::make_shared<GeneralGaussKernel>
			(convertTo<int>(argString, "[", ","), convertTo<double>(argString, ",", "]"));
//	if(argString == "NormalizedKernel")  return std::make_shared<NormalizedKernel>(RBFKernel(1.0));
//	if(argString == "WeightedSumKernel")  return std::make_shared<WeightedSumKernel>(const std::vector< KernelFunction * > &base);

	LOG(WARNING) << "Kernel '" << argString <<
		"' not valid. Available optimizers are: 'LinearKernel', 'PolynimialKernel[int degree,double offset]', 'RBFKernel[double gamma]', "
		"'DiagGaussKernel[int dim,double gamma]', 'GeneralGaussKernel[int dim,double gamma]'\n"
		"defaulting to RBFKernel[1.0]"
	   << std::endl;
	return std::make_shared<RBFKernel>(1.0);
}


int main(int argc, char* argv[]) {
	TrainCmdOptions::Parse(argc, argv);

	const std::string dbPath(TrainCmdOptions::DataBase != std::string() ? TrainCmdOptions::DataBase : std::string("linear.db"));
	Logger::get(std::cerr, ((insieme::utils::log::Level)TrainCmdOptions::Verbosity));

	size_t nIn = numberOfFeatures(), nOut = TrainCmdOptions::NumClasses;

	if(nIn == 0) {
		LOG(ERROR) << "No features set. Use -h to see help";
		return -1;
	}


	KernelFunctionPtr kernel = strToKernel(TrainCmdOptions::Kernel);
//	GeneralGaussKernel a(3,1.0);

	// declare Machine
	MyModelPtr svm;
	if(TrainCmdOptions::Y < 0) {
		std::cerr << "TRaining a multi Class\n";
		svm = std::make_shared<MyMultiClassSVM>(&*kernel, nOut, TrainCmdOptions::C);
	}
	else {
		std::cerr << "TRaining an Epsilon\n";
		svm = std::make_shared<MyEpsilon_SVM>(&*kernel, nOut, TrainCmdOptions::C, TrainCmdOptions::Y);
	}
	SVM_Optimizer optimizer;
	MeanSquaredError err;

	Trainer* svmTrainer;
	// create trainer
	try {
		svmTrainer = new Trainer(dbPath, *svm, strToGenNNoutput(TrainCmdOptions::TargetGen));
	} catch(Kompex::SQLiteException& sle) {
		LOG(ERROR) << "Cannot create trainer: \n";
		sle.Show();
	}

	if(TrainCmdOptions::SFeatureNames.size() > 0)
		svmTrainer->setStaticFeaturesByName(TrainCmdOptions::SFeatureNames);
	if(TrainCmdOptions::SFeatures.size() > 0)
		svmTrainer->setStaticFeaturesByIndex(TrainCmdOptions::SFeatures);

	if(TrainCmdOptions::DFeatureNames.size() > 0)
		svmTrainer->setDynamicFeaturesByName(TrainCmdOptions::DFeatureNames);
	if(TrainCmdOptions::DFeatures.size() > 0)
		svmTrainer->setDynamicFeaturesByIndex(TrainCmdOptions::DFeatures);

	if(TrainCmdOptions::PFeatureNames.size() > 0)
		svmTrainer->setPcaFeaturesByName(TrainCmdOptions::PFeatureNames);
	if(TrainCmdOptions::PFeatures.size() > 0)
		svmTrainer->setPcaFeaturesByIndex(TrainCmdOptions::PFeatures);

	if(TrainCmdOptions::ExcludeCids.size() > 0)
		svmTrainer->setExcludeCodes(TrainCmdOptions::ExcludeCids);

	if(TrainCmdOptions::FilterCids.size() > 0)
		svmTrainer->setFilterCodes(TrainCmdOptions::FilterCids);

	if(TrainCmdOptions::TargetName.size() == 0) {
		LOG(ERROR) << "No target set. Use -t to set the desired target";
		delete svmTrainer;
		return -1;
	}
	svmTrainer->setTargetByName(TrainCmdOptions::TargetName);


	LOG(INFO)<< "Error: " << svmTrainer->train(optimizer, err, 0) << std::endl;

	if(TrainCmdOptions::OutputModel.size() > 0 || TrainCmdOptions::OutputPath.size() > 0)
		writeModel(svmTrainer);

	return 0;
}

