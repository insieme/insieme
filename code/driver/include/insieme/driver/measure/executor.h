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
 */

#pragma once

#include <map>
#include <boost/uuid/random_generator.hpp>

#include "insieme/driver/measure/hardware.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/map_utils.h"

/**
 * Within this header file some utilities enabling the execution of
 * code fragments using the insieme runtime and its instrumentation
 * capabilities.
 */


namespace insieme {
namespace driver {
namespace measure {

	using std::string;

	class Executor;
	typedef std::shared_ptr<Executor> ExecutorPtr;

	/**
	* A utility struct that holds execution environment, parameters, and other execution-related properties
	*/
	struct ExecutionSetup : public insieme::utils::Printable {
		Machine machine;
		std::map<string, string> env;
		std::vector<string> params;
		string outputDirectory;
		bool requiresRawIOCapabilities;
		bool requiresBinaryCopying;
		bool silent;

		ExecutionSetup() : machine(), env(), params(), outputDirectory("."), requiresRawIOCapabilities(false), requiresBinaryCopying(true), silent(false) {}

		ExecutionSetup withMachine(const Machine newMachine) const {
			ExecutionSetup retVal = *this;
			retVal.machine = newMachine;
			return retVal;
		}

		ExecutionSetup withEnvironment(const std::map<string, string> newEnvironment) const {
			ExecutionSetup retVal = *this;
			retVal.env = newEnvironment;
			return retVal;
		}

		ExecutionSetup withParameters(const std::vector<string> newParameters) const {
			ExecutionSetup retVal = *this;
			retVal.params = newParameters;
			return retVal;
		}

		ExecutionSetup withOutputDirectory(const string newDirectory) const {
			ExecutionSetup retVal = *this;
			retVal.outputDirectory = newDirectory;
			return retVal;
		}

		ExecutionSetup withCapabilities(bool newRequiresRawIOCapabilities) const {
			ExecutionSetup retVal = *this;
			retVal.requiresRawIOCapabilities = newRequiresRawIOCapabilities;
			return retVal;
		}

		ExecutionSetup withBinaryCopying(bool newRequiresBinaryCopying) const {
			ExecutionSetup retVal = *this;
			retVal.requiresBinaryCopying = newRequiresBinaryCopying;
			return retVal;
		}

		ExecutionSetup withSilent(bool newSilent) const {
			ExecutionSetup retVal = *this;
			retVal.silent = newSilent;
			return retVal;
		}

		std::ostream& printTo(std::ostream& out) const {
			out << "ExecutionSetup:\n";
			out << "\tmachine: " << machine << "\n";
			out << "\tenv: " << env << "\n";
			out << "\tparams: " << params << "\n";
			out << "\toutputDirectory: " << outputDirectory << "\n";
			out << "\trequiresRawIOCapabilities: " << toString(requiresRawIOCapabilities?"true":"false") << "\n";
			out << "\tsilent: " << toString(silent ? "true" : "false") << "\n";
			return out;
		}

	};

	/**
	 * The basic interface of an executor.
	 */
	class Executor : utils::Printable {
	  protected:
		/**
		* Universally unique ID generator used in working directory name generation.
		*/
		mutable boost::uuids::random_generator uuidGen;

	  public:
		/**
		* Holds a wrapper and its arguments to be prepended in the execution string (i.e. affinity tools, etc...)
		*/
		const string wrapper;

		/**
		* Creates an executor with a given wrapper if present
		* @param wrapper a wrapper program (and its arguments, if any) to be prepended in the execution string
		*/
		Executor(const string& wrapper = "") : wrapper(wrapper) { }

		virtual ~Executor(){};

		/**
		 * An invocation to this function will execute the given binary once.
		 * The conditions of the execution are defined by the actual implementations.
		 *
		 * @param binary the name of the binary to be executed
		 * @param executionSetup the execution setup with which to run the specified binary
		 * @return the return code of the binaries execution
		 */
		virtual int run(const string& binary, const ExecutionSetup& executionSetup) const = 0;

		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};

	/**
	 * This straight-forward implementation is executing the given binary within
	 * the current working directory.
	 */
	class LocalExecutor : public Executor {
	  public:
		  /**
		   * Creates an executor with a given wrapper if present
		   * @param wrapper a wrapper program (and its arguments, if any) to be prepended in the execution string
		   */
		  LocalExecutor(const string& wrapper = "") : Executor(wrapper) { }

		/**
		 * Runs the given binary within the current working directory.
		 */
		virtual int run(const string& binary, const ExecutionSetup& executionSetup) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "LocalExecutor(" << wrapper << ")";
		}
	};

	/**
	 * A factory function for a local executor.
	 */
	ExecutorPtr makeLocalExecutor();

	/**
	 * This executor runs binaries on a remote machine. The binary will be copied to
	 * a remote working directory (using scp), executed on the remote machine and the
	 * resulting files will be moved back to the local working directory (using rsync).
	 */
	class RemoteExecutor : public Executor {
	  private:
		/**
		 * The remote hostname on which the binary should be executed.
		 */
		string hostname;

		/**
		 * The user name to be used to login on the remote machine.
		 */
		string username;

		/**
		 * The working directory on the remote machine.
		 */
		string workdir;

	  public:
		/**
		 * Creates a new instance of a remote executor using the given parameters.
		 *
		 * @param hostname the host name or the IP of the machine to run the binary on (the name has to be resolvable)
		 * @param username the user to be used to log in to the remote machine (the authentication should use a certificate)
		 * 					if empty, the current users user name will be used
		 * @param remoteWorkDir the working directory to be used on the remote system.
		 */
		RemoteExecutor(const string& hostname, const string& username = "", const string& remoteWorkDir = "/tmp", const string& wrapper = "")
		    : Executor(wrapper), hostname(hostname), username(username), workdir(remoteWorkDir) {}

		/**
		 * Copies the given binary to the remote machine and executes it.
		 * @param binary the name of the binary to be executed
		 * @param executionSetup the execution setup with which to run the specified binary
		 */
		virtual int run(const string& binary, const ExecutionSetup& executionSetup) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "RemoteExecutor(" << wrapper << ")";
		}
	};
	/**
	* This executor runs binaries on a remote machine using mpi. The binary will be copied
	* to each participating compute node (using scp), executed using mpirun and the
	* resulting files will be moved back to the local working directory (using rsync).
	*/
	class MPIExecutor : public Executor {
	  private:
		/**
		* Additional parameters for mpirun
		*/
		string options;
	  public:
		/**
		* Creates a new instance of an MPI executor.
		*
		* @param wrapper a wrapper program (and its arguments, if any) to be prepended in the execution string
		*/
		MPIExecutor(const string& options = "") : Executor(""), options(options) {}

		/**
		* Copies the given binary to each participating compute node and executes it using mpirun.
		* @param binary the name of the binary to be executed
		* @param executionSetup the execution setup with which to run the specified binary
		*/
		virtual int run(const string& binary, const ExecutionSetup& executionSetup) const;

		virtual std::ostream& printTo(std::ostream& out) const { return out << "MPIExecutor(" << options << ")"; }
	};

	/**
	 * A factory function for a remote executor.
	 */
	ExecutorPtr makeRemoteExecutor(const string& hostname, const string& username = "", const string& remoteWorkDir = "/tmp");

	/**
	 * A factory function for an MPI executor.
	 */
	ExecutorPtr makeMPIExecutor(const string& options = "");


} // end namespace measure
} // end namespace driver
} // end namespace insieme
