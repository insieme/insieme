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

#include <map>

/**
 * Within this header file some utilities enabling the execution of
 * code fragments using the insieme runtime and its instrumentation
 * capabilities.
 */


namespace insieme {
namespace driver {
namespace measure {

	class Executor;
	typedef std::shared_ptr<Executor> ExecutorPtr;

	/**
	 * The basic interface of an executor.
	 */
	class Executor {
	public:
		virtual ~Executor() {};

		/**
		 * An invocation to this function will execute the given binary once.
		 * The conditions of the execution are defined by the actual implementations.
		 *
		 * @param binary the name of the binary to be executed
		 * @param env some environment variables to be set
		 * @param outputDirectory the directory the resulting logs will be written to
		 * @return the return code of the binaries execution
		 */
		virtual int run(const std::string& binary, const std::map<string, string>& env = std::map<string, string>(),
				const std::string& outputDirectory = ".") const =0;
	};

	/**
	 * This straight-forward implementation is executing the given binary within
	 * the current working directory.
	 */
	class LocalExecutor : public Executor {
	public:

		/**
		 * Runs the given binary within the current working directory.
		 */
		virtual int run(const std::string& binary, const std::map<string, string>& env, const string& dir) const;
	};

	/**
	 * A factory function for a local executor.
	 */
	ExecutorPtr makeLocalExecutor();

	/**
	 * This executor is running binaries on a remote machine. The binary will be copied
	 * to a remote working directory (using scp), executed on the remote machine and
	 * result files will be moved back into the local working directory.
	 */
	class RemoteExecutor : public Executor {

		/**
		 * The remote hostname on which the binary should be executed.
		 */
		std::string hostname;

		/**
		 * The user name to be used to login on the remote machine.
		 */
		std::string username;

		/**
		 * The working directory on the remote machine.
		 */
		std::string workdir;

	public:

		/**
		 * Creates a new instance of a remote executor using the given parameters.
		 *
		 * @param hostname the host name or the IP of the machine to run the binary on (the name has to be resolvable)
		 * @param username the user to be used to log in to the remote machine (the authentication should use a certificate)
		 * 					if empty, the current users user name will be used
		 * @param remoteWorkDir the working directory to be used on the remote system.
		 */
		RemoteExecutor(const std::string& hostname, const std::string& username = "", const std::string& remoteWorkDir = "/tmp")
			: hostname(hostname), username(username), workdir(remoteWorkDir) {}

		/**
		 * Runs the given binary on the specified remote machine.
		 */
		virtual int run(const std::string& binary, const std::map<string, string>& env, const string& dir) const;

		const std::string& getHostname() const {
			return hostname;
		}

		const std::string& getUsername() const {
			return username;
		}

		const std::string& getWorkDir() const {
			return workdir;
		}
	};


	/**
	 * A factory function for a remote executor.
	 */
	ExecutorPtr makeRemoteExecutor(const std::string& hostname, const std::string& username = "", const std::string& remoteWorkDir = "/tmp");


	/**
	 * This executor is running binaries on a remote machine using a job queuing system.
	 * The binary will be copied to a remote working directory (using scp), executed on the remote
	 * machine using its local queuing system and the resulting files files will be moved back into
	 * the local working directory.
	 */
	class RemoteQueuingExecutor : public RemoteExecutor {
	public:
		/**
		 * Creates a new instance of a remote executor using the given parameters.
		 *
		 * @param hostname the host name or the IP of the machine to run the binary on (the name has to be resolvable)
		 * @param username the user to be used to log in to the remote machine (the authentication should use a certificate)
		 * 					if empty, the current users user name will be used
		 * @param remoteWorkDir the working directory to be used on the remote system.
		 */
		RemoteQueuingExecutor(const std::string& hostname, const std::string& username = "", const std::string& remoteWorkDir = "/tmp")
			: RemoteExecutor(hostname, username, remoteWorkDir) {}

		/**
		 * Runs the given binary on the specified remote machine.
		 */
		virtual int run(const std::string& binary, const std::map<string, string>& env, const string& dir) const;
	};


	/**
	 * A factory function for a remote executor.
	 */
	ExecutorPtr makeRemoteQueuingExecutor(const std::string& hostname, const std::string& username = "", const std::string& remoteWorkDir = "/tmp");


} // end namespace measure
} // end namespace driver
} // end namespace insieme
