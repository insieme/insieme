/*
 * IFParser.h
 *
 *  Created on: 9.08.2010
 *      Author: clemens
 */

#ifndef IFPARSER_H_
#define IFPARSER_H_

//----------------------------------------
#define MAX_INCLUDE_DEPTH 20	//number of allowed include-recursion-depth in inputfiles
#define CYLINDRICAL 1
#define SPHERICAL 2

//----------------------------------------
class IFParser {
public:
	//constructors
	IFParser();

	IFParser( std::string arg);
	virtual ~IFParser();

	//public variables and objects
//	static const std::string knownKeywords[];

	//get + set - methods
	std::string getInputFileName();
	std::string getLogFileName();
	std::string getTmpInputFileName();

	//public methods
	bool doesFileExist();
	bool createLogFile();
	int generateTmpInputFile();
	void appendToLogFile( std::string message);
	int readTmpInputFile( FETask::FETask &feJob);

	//static methods
	static std::string intToStr( int nr);
	static std::string doubleToStr( double nr);
	static std::string& toUpCase( std::string &lowCaseStr);
	static std::vector<std::string> splitStrToStrs( std::string inputStr, char* sep);
	static std::vector<int> splitStrToInt( std::string inputStr, char* sep);

private:
	bool errors;

	std::string ifPathName;		//original argument
	std::string ifName;			//inputfilename
	std::string ifPath;			//inputfilepath

	std::string lfPathName;
	std::string lfName;			//logfilename

	std::string ifPathNameTmp;	//temporary inputfilename + path
	std::string ifNameTmp;		//temporary inputfilename

	time_t rawTimeStart;
	time_t rawTimeInterim;
	//time_t rawTimeEnd;

	//private methods - no better idea in the moment
	void getHeading( std::ifstream &file, FETask::FETask &feJob, int &lCount);
	std::string readNodes( std::ifstream &file, FETask::FETask &feJob, std::vector<std::string> &strParts, int &lCount);
	std::string readNset( std::ifstream &file, FETask::FETask &feJob, std::vector<std::string> &strParts, int &lCount);
	std::string readElements( std::ifstream &file, FETask::FETask &feJob, std::vector<std::string> &strParts, int &lCount);
	std::string readMaterial( std::ifstream &file, FETask::FETask &feJob, std::vector<std::string> &strParts, int &lCount);
	std::string readElset( std::ifstream &file, FETask::FETask &feJob, std::vector<std::string> &strParts, int &lCount);
	void readSolidSection( std::ifstream &file, FETask::FETask &feJob, std::vector<std::string> &strParts, int &lCount);
	void readStatic( std::ifstream &file, Step::Step &analysisStep, std::vector<std::string> &strParts, int &lCount);
	std::string readCLoads( std::ifstream &file, FETask::FETask &feJob, Step::Step &analysisStep, std::vector<std::string> &strParts, int &lCount);
	std::string readBoundaries( std::ifstream &file, FETask::FETask &feJob, Step::Step &analysisStep, std::vector<std::string> &strParts, int &lCount);
	std::string getInitialTemperature( std::ifstream &file, FETask::FETask &feJob, int &lCount);
};

#endif /* IFPARSER_H_ */
