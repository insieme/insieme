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

#include "ReClaM/createConnectionMatrix.h"
#include "ReClaM/MeanSquaredError.h"
#include "ReClaM/Quickprop.h"
#include "ReClaM/BFGS.h"

#include "insieme/utils/string_utils.h"
#include "insieme/machine_learning/cmd_line_utils.h"
#include "insieme/machine_learning/trainer.h"

using namespace insieme::ml;

int main(int argc, char* argv[]) {
	CommandLineOptions::Parse(argc, argv);
	const std::string dbPath(CommandLineOptions::DataBase != std::string() ? CommandLineOptions::DataBase : std::string("small.db"));

	// Create a connection matrix with 2 inputs, 1 output
	// and a single, fully connected hidden layer with
	// 8 neurons:
	Array<int> con;
	createConnectionMatrix(con, 4, 8, 5);
	// declare Machine
	FFNet net = FFNet(4, 5, con);
	net.initWeights(-0.1, 0.1);
	MeanSquaredError err;
	Array<double> in, target;
	Quickprop qprop;
	BFGS bfgs;
	bfgs.initBfgs(net);

	// create trainer
	Trainer qpnn(dbPath, net, GenNNoutput::ML_MAP_FLOAT_HYBRID);

	if(CommandLineOptions::FeatureNames.size() > 0)
		qpnn.setFeaturesByName(CommandLineOptions::FeatureNames);

	if(CommandLineOptions::Features.size() == 0) {
		for(size_t i = 0u; i < 4u; ++i)
			CommandLineOptions::Features.push_back(toString(i+1));
	}

	qpnn.setFeaturesByIndex(CommandLineOptions::Features);

	std::cout << "Error: " << qpnn.train(bfgs, err, 10) << std::endl;

	return 0;
}

