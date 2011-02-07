/*
 * IFParser.cpp
 *
 * InputFileParser for reading  Abaqus-.inp-files
 * read abq-inp-keywords.odp
 *
 *  Created on: 9.08.2010
 *      Author: clemens
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <cstring>

#include <stdlib.h>
#include <time.h>

using namespace std;

#include "config.h"
#include "Node.h"
#include "Element.h"
#include "Material.h"
#include "Step.h"
#include "FETask.h"
#include "IFParser.h"

//constructors + destructor
IFParser::IFParser() {
}

IFParser::IFParser( string arg) {
	errors=false;

	size_t pos;

	ifPathName=arg;

	pos=ifPathName.find_last_of( "/\\");

	if( (int) pos==(-1)) {
		ifPath="./";
		ifName=ifPathName;
	}
	else {
		ifPath=ifPathName.substr( 0, pos);
		ifPath.append( "/");

		ifName=ifPathName.substr( pos+1);
	}

	//create temporary inputfile
	ifPathNameTmp=ifPathName;
	ifPathNameTmp.append( ".tmp");
	ifNameTmp=ifName;
	ifNameTmp.append( ".tmp");
}

IFParser::~IFParser() {
}

//static methods
string IFParser::intToStr( int nr) {
	stringstream ssOut;

	ssOut << nr;
	return ssOut.str();
}

string IFParser::doubleToStr( double nr) {
	stringstream ssOut;

	ssOut << nr;
	return ssOut.str();
}

string& IFParser::toUpCase( string &lowCaseStr) {
	int i;
	int len=lowCaseStr.length();

	for ( i=0; i<len; ++i) {
		lowCaseStr[i]=toupper( lowCaseStr[i]);
	}

	return lowCaseStr;
}

vector<string> IFParser::splitStrToStrs( string inputStr, char* sep) {
	char* token;
	char* line;
	vector<string> tokens;

	line = new char [inputStr.size()+1];
	strcpy( line, inputStr.c_str());		//convert string into char (not const char!)

	token=strtok( line, sep);

	while ( token!=NULL) {
		tokens.push_back( token);

		token=strtok( NULL, sep);
	}

	delete[] line;

	return tokens;
}

vector<int> IFParser::splitStrToInt( string inputStr, char* sep) {
	char* token;
	char* line;
	vector<int> tokens;

	line = new char[inputStr.size()+1];
	strcpy( line, inputStr.c_str());		//convert string into char (not const char!)

	token=strtok( line, sep);

	while ( token!=NULL) {
		tokens.push_back( atoi( token));

		token=strtok( NULL, sep);
	}

	delete[] line;

	return tokens;
}

//------------------------------------------------------------
//public methods
// get + set -methods
string IFParser::getInputFileName() {
	return ifName;
}

string IFParser::getLogFileName() {
	return lfName;
}

string IFParser::getTmpInputFileName() {
	return ifNameTmp;
}

//public methods
void IFParser::appendToLogFile( string message) {
	ofstream file;

	file.open( lfPathName.c_str(), ios::app);

	file << message;

	file.close();
}

bool IFParser::doesFileExist() {
	bool fileExists=false;
	int len=ifPathName.length();
	ifstream file;

	if( len<5) {
		return false;
	}

	if( ifPathName.substr( len-4, len).compare( ".inp")!=0) {
		return false;
	}

	file.open( ifPathName.c_str(), ios::in);

	if( file.good()) {
		fileExists=true;

		//create log-file-name
		lfPathName=ifPath;
		lfName=ifName.substr( 0, len-4);
		lfName.append( ".log");
		lfPathName.append( lfName);
	}

	file.close();

	return fileExists;
}


bool IFParser::createLogFile() {
	bool success;

	time( &rawTimeStart);
	struct tm *timeinfo=localtime( &rawTimeStart);

	ofstream file;

	file.open( lfPathName.c_str());

	file << "icona v" << VERSION_ALL << "-" << VERSION_PARSER << "-" << VERSION_VIEWER << endl << endl;
	file << "log-file\n--------------------------------------------------------------------------------" << endl << endl;
	file << "start reading inputfile --> " << ifPathName << " <--" << endl;
	file << asctime( timeinfo) << endl;

	success=!( file.fail());

	file.close();

	return success;
}

int IFParser::generateTmpInputFile() {
	int errorNr=0;
	int includeCount=0;
	int lineCount[MAX_INCLUDE_DEPTH]={ 0};
	int lineCountAll=0;
	int len;
	bool done=false;

	string line, firstEight, logMessage;
	string ifNames[MAX_INCLUDE_DEPTH];

	logMessage="start generating temporary inputfile --> ";
	logMessage.append( ifPathNameTmp);
	logMessage.append( " <--\n\n");
	appendToLogFile( logMessage);

	ifstream inputFile[MAX_INCLUDE_DEPTH];
	ofstream tmpInputFile;

	tmpInputFile.open( ifPathNameTmp.c_str());

	inputFile[includeCount].open( ifPathName.c_str(), ios::in);
	ifNames[0]=ifPathName;

	while(( !done) && ( errorNr==0)) {
		getline( inputFile[includeCount], line);

		lineCount[includeCount]++;
		lineCountAll++;

		if( inputFile[includeCount].eof()) {
			inputFile[includeCount].close();

			lineCount[includeCount]=0;
			includeCount--;

			if( includeCount<0) {
				done=true;
			}
		}
		else {
			len=line.length();

			if( len>0) {		//line is not empty
				if( (line.compare( "\n")!=0) && ( line.compare( "\r")!=0) && ( line.compare( "\r\n")!=0)) {	//ignore CR, LF and CR LF
					if( line.substr(0,2).compare( "**")!=0) {	//ignore comments
						firstEight=line.substr(1,7);
						firstEight=toUpCase( firstEight);

						if( firstEight.compare( "INCLUDE")==0) {
							vector<string> strParts=splitStrToStrs( line, (char*) " ,=\t");

							includeCount++;

							if( includeCount<MAX_INCLUDE_DEPTH) {
								lineCount[includeCount]=0;
								ifNames[includeCount]=ifPath;
								ifNames[includeCount].append( strParts[2]);

								inputFile[includeCount].open( ifNames[includeCount].data(), ios::in);

								if( !( inputFile[includeCount].good())) {
									inputFile[includeCount].close();
									includeCount--;

									logMessage="error! - include-inputfile --> ";
									logMessage.append( ifNames[includeCount+1]);
									logMessage.append( " <-- not found.\n");
									logMessage.append( ifNames[includeCount]);
									logMessage.append( ", line ");
									logMessage.append( intToStr( lineCount[includeCount]));
									logMessage.append( "\n");

									appendToLogFile( logMessage);
									errorNr=ERR_IN_INPUTFILE;
								}
							}
							else {
								includeCount--;

								logMessage="error! - more than ";
								logMessage.append( intToStr( MAX_INCLUDE_DEPTH));
								logMessage.append( " recursive includes; maximum reached in\n");
								logMessage.append( ifNames[includeCount]);
								logMessage.append( ", line ");
								logMessage.append( intToStr( lineCount[includeCount]));
								logMessage.append( "\n");

								appendToLogFile( logMessage);
								errorNr=ERR_IN_INPUTFILE;
							}
						}
						else {
							line=toUpCase( line);

							tmpInputFile << line << endl;
						}
					}
				}
			}
		}
	}

	tmpInputFile.close();

	return errorNr;
}

//private methods for parsing keywords - no better idea
//--------------------HEADING--------------------
void IFParser::getHeading( ifstream &file, FETask &feJob, int &lCount) {
	string line;

	getline( file, line);
	lCount++;

	feJob.setHeading( line);
}

//--------------------NODE-----SYSTEM-----NSET--------------------
string IFParser::readNodes( ifstream &file, FETask &feJob, vector<string> &strParts, int &lCount) {
	string line, nsetName, logMessage;
	vector<int> nodesOfNset;

	bool done=false, intoNset=false;

	int nodeNr;
	int convertFrom=0, errorNr=0;

	double x, y, z;

	if( strParts.size()>2) {
		if( strParts[1].compare( "SYSTEM")==0) {
			if( strParts[1].compare( "C")==0) {
				convertFrom=CYLINDRICAL;
			}
			else if( strParts[1].compare( "S")==0) {
				convertFrom=SPHERICAL;
			}
		}
		if( strParts[1].compare( "NSET")==0) {
			nsetName=strParts[2];
			intoNset=true;
		}
	}
	if( strParts.size()>4) {
		if( strParts[3].compare( "SYSTEM")==0) {
			if( strParts[4].compare( "C")==0) {
				convertFrom=CYLINDRICAL;
			}
			else if( strParts[4].compare( "S")==0) {
				convertFrom=SPHERICAL;
			}
		}
		if( strParts[3].compare( "NSET")==0) {
			nsetName=strParts[4];
			intoNset=true;
		}
	}

	getline( file, line);
	lCount++;

	while( !file.eof() && !done) {
		if( line.at(0)=='*') {
			done=true;
		}
		else  {
			strParts.clear();
			strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

			nodeNr=atoi( strParts[0].c_str());			//node number

			if( intoNset) {
				nodesOfNset.push_back( nodeNr);
			}

			x=atof( strParts[1].c_str());
			y=atof( strParts[2].c_str());
			z=atof( strParts[3].c_str());

			errorNr=feJob.addNode( nodeNr, x, y, z);
		}

		if( errorNr==NODE_EXISTS) {
			logMessage="error! - Node ";
			logMessage.append( IFParser::intToStr(nodeNr));
			logMessage.append( " already exists; last one will be skipped\nline ");
			logMessage.append( intToStr( lCount));
			logMessage.append( "\n");
			appendToLogFile( logMessage);

			errorNr=0;
			errors=true;
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	if( intoNset) {
		errorNr=feJob.addNset( nsetName, nodesOfNset);
	}

	if( errorNr==NSET_EXISTS) {
		logMessage="Nodeset ";
		logMessage.append( nsetName);
		logMessage.append( " already exists; last one will be used\nline ");
		logMessage.append( intToStr( lCount-1));
		logMessage.append( "\n");
		appendToLogFile( logMessage);

		errors=true;
	}

	return line;
}

//--------------------NSET-----NAME-----GENERATE--------------------
string IFParser::readNset( ifstream &file, FETask &feJob, vector<string> &strParts, int &lCount) {
	string line, nsetName, logMessage;
	vector<int> nodesOfNset;
	vector<int> intParts;
	vector<int>::iterator intPartsIT;

	int nrOfNodes;
	int errorNr=0;

	bool done=false, generate=false;

	if( strParts.size()>2) {
		if( strParts[1].compare( "GENERATE")==0) {
			generate=true;
			nsetName=strParts[3];
		}
		else if( strParts[3].compare( "GENERATE")==0) {
			generate=true;
			nsetName=strParts[2];
		}
		else {
			nsetName=strParts[2];
		}
	}

	getline( file, line);
	lCount++;

	while( !file.eof() && !done) {
		if( line.at(0)=='*') {
			done=true;
		}
		else {
			intParts.clear();
			intParts=IFParser::splitStrToInt( line, (char*) " ,\t");

			nrOfNodes=intParts.size();

			if( generate) {
				if( nrOfNodes<2) {
					logMessage="error! - too few arguments for generateing Nset\nline ";
					logMessage.append( intToStr( lCount));
					logMessage.append( " ignored\n ");
					appendToLogFile( logMessage);

					done=true;
					errors=true;
				}
				else {
					int i, from, to;
					int step=1;

					from=intParts[0];			//start-node-nr
					to=intParts[1];				//end-node-nr

					if( nrOfNodes>2) {
						step=intParts[2];		//step
					}

					cout << endl;
					for( i=from; i<=to; i+=step) {
						nodesOfNset.push_back( i);
					}
					cout << endl;
				}
			}
			else {
				for( intPartsIT=intParts.begin(); intPartsIT!=intParts.end(); intPartsIT++) {
					nodesOfNset.push_back( *intPartsIT);
				}
			}
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	errorNr=feJob.addNset( nsetName, nodesOfNset);

	if( errorNr==NSET_EXISTS) {
		logMessage="Nodeset ";
		logMessage.append( nsetName);
		logMessage.append( " already exists; last one will be used\nline ");
		logMessage.append( intToStr( lCount-1));
		logMessage.append( "\n");
		appendToLogFile( logMessage);

		errors=true;
	}

	return line;
}

//--------------------ELEMENT-----TYPE--------------------
string IFParser::readElements( ifstream &file, FETask &feJob, vector<string> &strParts, int &lCount) {
	string line, elementType, logMessage;
	vector<int> nodeNrsOfElement;
	vector<int> intParts;
	vector<int>::iterator intPartsIT;
	int nodesPerElement, nrOfNodes, nodeNr;
	int errorNr=0;

	bool done=false;

	elementType=strParts[2];

	if( elementType.compare( "HC8A8E9")==0) {
		feJob.setDOFPerNode( 3);
		feJob.setNrOfNodesPerElement( 8);
		nodesPerElement=8;
	}
	else if( elementType.compare( "C3D8")==0) {
		feJob.setDOFPerNode( 3);
		feJob.setNrOfNodesPerElement( 8);
		nodesPerElement=8;
	}
	else {
		errorNr=UNKNOWN_ELEMENTTYPE;
		errors=true;
	}

	getline( file, line);
	lCount++;

	if( errorNr==0) {
		while( !file.eof() && !done) {
			if( line.at(0)=='*') {
				done=true;
			}
			else {
				intParts.clear();
				intParts=IFParser::splitStrToInt( line, (char*) " ,\t");

				nrOfNodes=intParts.size();

				if( nrOfNodes!=(nodesPerElement+1)) {
					logMessage="error! - wrong number of arguments for element\nline ";
					logMessage.append( intToStr( lCount));
					logMessage.append( " ignored\n");
					appendToLogFile( logMessage);

					errors=true;
				}
				else {
					nodeNr=intParts[0];

					intParts.erase( intParts.begin());

					errorNr=feJob.addElement( nodeNr, elementType, intParts);
				}
			}

			if( !done) {
				getline( file, line);
				lCount++;
			}
		}
	}

	if( errorNr==UNKNOWN_ELEMENTTYPE) {
		logMessage="error! - elementtype ";
		logMessage.append( elementType);
		logMessage.append( " unknown; will be ignored\nline ");
		logMessage.append( intToStr( lCount-1));
		logMessage.append( "\n");
		appendToLogFile( logMessage);

		errors=true;
	}

	return line;
}

//--------------------MATERIAL-----NAME--------------------
string IFParser::readMaterial( ifstream &file, FETask &feJob, vector<string> &strParts, int &lCount) {
	string line, logMessage, secondChar;
	bool done=false;
	int errorNr=0;

	Material tmpMaterial( strParts[2]);

	getline( file, line);
	lCount++;

	while( (!file.eof()) && (!done)) {
		secondChar=line.substr(1,2);

		if( secondChar.compare( "EL")==0) {				//ELASTIC
			getline( file, line);
			lCount++;

			strParts.clear();
			strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

			tmpMaterial.setElastic( atof(strParts[0].c_str()), atof(strParts[1].c_str()));
		}
		else if( secondChar.compare( "EX")==0) {		//EXPANSION
			getline( file, line);
			lCount++;

			strParts.clear();
			strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

			tmpMaterial.setAlphaT( atof(strParts[0].c_str()));
		}
		else {
			done=true;
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	errorNr=feJob.addMaterial( tmpMaterial);

	if( errorNr==MATERIAL_EXISTS) {
		logMessage="error! - material ";
		logMessage.append( tmpMaterial.getMaterialName());
		logMessage.append( " already exists; last one will be skipped\nline ");
		logMessage.append( intToStr( lCount-1));
		logMessage.append( "\n");
		appendToLogFile( logMessage);

		errorNr=0;
		errors=true;
	}

	return line;
}

//--------------------ELSET-----NAME-----GENERATE--------------------
string IFParser::readElset( ifstream &file, FETask &feJob, vector<string> &strParts, int &lCount) {
	string line, elsetName, logMessage;
	vector<int> nodeNrsOfElset;
	vector<int> intParts;
	vector<int>::iterator intPartsIT;

	int nrOfNodes;
	int errorNr=0;

	bool done=false, generate=false;

	if( strParts.size()>2) {
		if( strParts[1].compare( "GENERATE")==0) {
			generate=true;
			elsetName=strParts[3];
		}
		else if( strParts[3].compare( "GENERATE")==0) {
			generate=true;
			elsetName=strParts[2];
		}
		else {
			elsetName=strParts[2];
		}
	}

	getline( file, line);
	lCount++;

	while( !file.eof() && !done) {
		if( line.at(0)=='*') {
			done=true;
		}
		else {
			intParts.clear();
			intParts=IFParser::splitStrToInt( line, (char*) " ,\t");

			nrOfNodes=intParts.size();

			if( generate) {
				if( nrOfNodes<2) {
					logMessage="error! - too few arguments for generateing elset\nline ";
					logMessage.append( intToStr( lCount));
					logMessage.append( " ignored\n ");
					appendToLogFile( logMessage);

					done=true;
					errors=true;
				}
				else {
					int i, from, to;
					int step=1;

					from=intParts[0];			//start-node-nr
					to=intParts[1];				//end-node-nr

					if( nrOfNodes>2) {
						step=intParts[2];		//step
					}

					for( i=from; i<=to; i+=step) {
						nodeNrsOfElset.push_back( i);
					}
				}
			}
			else {
				for( intPartsIT=intParts.begin(); intPartsIT!=intParts.end(); intPartsIT++) {
					nodeNrsOfElset.push_back( *intPartsIT);
				}
			}
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	if( errorNr==0) {
		errorNr=feJob.addElset( elsetName, nodeNrsOfElset);
	}

	if( errorNr==ELSET_EXISTS) {
		logMessage="error! - elementset ";
		logMessage.append( elsetName);
		logMessage.append( " already exists; last one will be used\nline ");
		logMessage.append( intToStr( lCount-1));
		logMessage.append( "\n");
		appendToLogFile( logMessage);

		errors=true;
	}

	return line;
}

void IFParser::readSolidSection( ifstream &file, FETask &feJob, vector<std::string> &strParts, int &lCount) {
	int errorNr=0;
	string elsetName, materialName, logMessage;

	if( strParts.size()>=6) {
		if( strParts[2].compare( "ELSET")==0) {
			elsetName=strParts[3];
		}
		else {
			elsetName=strParts[5];
		}

		if( strParts[2].compare( "MATERIAL")==0) {
			materialName=strParts[3];
		}
		else {
			materialName=strParts[5];
		}

		errorNr=feJob.addSolidSection( elsetName, materialName);

		if( errorNr==SOLIDSECTION_EXISTS) {
			logMessage="error! - solidsection already exists\nline ";
			logMessage.append( intToStr( lCount));
			logMessage.append( " will be skipped\n");
			appendToLogFile( logMessage);

			errors=true;
		}
	}
	else {
		logMessage="error! - wrong number of arguments for solidsection\nline ";
		logMessage.append( intToStr( lCount));
		logMessage.append( " will be skipped\n");
		appendToLogFile( logMessage);

		errors=true;
	}
}

void IFParser::readStatic( ifstream &file, Step &analysisStep, vector<std::string> &strParts, int &lCount) {
	string line, logMessage;
	int errorNr=0;
	double factor;
	double factorInc;

	getline( file, line);
	lCount++;

	strParts.clear();
	strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

	if( line.at(0)!='*') {
		if( strParts.size()==2) {
			factorInc=atof( strParts[0].c_str());
			factor=atof( strParts[1].c_str());			

			analysisStep.setLoadFactor( factor);
			analysisStep.setLoadFactorInc( factorInc);
		}
		else {
			errorNr=ERROR_STATIC;
		}
	}
	else {
		errorNr=ERROR_STATIC;
	}

	if( errorNr==ERROR_STATIC) {
		logMessage="error! - wrong (number of) arguments for static\nline ";
		logMessage.append( intToStr( lCount));
		logMessage.append( " will be skipped\n");
		appendToLogFile( logMessage);

		errors=true;
	}
}

string IFParser::readCLoads( ifstream &file, FETask &feJob, Step &analysisStep, vector<string> &strParts, int &lCount) {
	string line;
	double force;
	vector<int> tmpNodes;
	int i, nodeNr, direction;

	bool done=false;

	getline( file, line);
	lCount++;

	while( !file.eof() && !done) {
		if( line.at(0)=='*') {
			done=true;
		}
		else {
			strParts.clear();
			strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

			nodeNr=atoi( strParts[0].c_str());
			direction=atoi( strParts[1].c_str());
			force=atof( strParts[2].c_str());

			if( nodeNr==0) {
				tmpNodes=feJob.getNset( strParts[0]);

				for( i=0; i<(int) tmpNodes.size(); i++) {
					analysisStep.addCLoad( tmpNodes[i], direction, force);
				}
			}
			else {
				analysisStep.addCLoad( nodeNr, direction, force);
			}
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	return line;
}

string IFParser::readBoundaries( ifstream &file, FETask &feJob, Step &analysisStep, vector<string> &strParts, int &lCount) {
	string line;
	vector<int> tmpNodes;
	int nodeNr, direction;

	bool done=false;

	getline( file, line);
	lCount++;

	while( !file.eof() && !done) {
		if( line.at(0)=='*') {
			done=true;
		}
		else {
			strParts.clear();
			strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

			nodeNr=atoi( strParts[0].c_str());

			tmpNodes.clear();

			if( nodeNr==0) {
				tmpNodes=feJob.getNset( strParts[0]);
			}
			else {
				tmpNodes.push_back( nodeNr);
			}

			direction=atoi( strParts[1].c_str());

			if( analysisStep.getParsing()) {				
				for( int i=0; i<(int) tmpNodes.size(); i++) {
					analysisStep.addBoundaryCondition( tmpNodes[i], direction);
				}
			}
			else {
				for( int i=0; i<(int) tmpNodes.size(); i++) {
					feJob.addBoundaryCondition( tmpNodes[i], direction);					
				}
			}
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	return line;
}

string IFParser::getInitialTemperature( std::ifstream &file, FETask::FETask &feJob, int &lCount) {
	bool done=false;
	int nodeNr;
	double value;
	string line;
	vector<string> strParts;
	vector<int> tmpNodes;
	map< int, double> temperatureList;

	getline( file, line);
	lCount++;

	while( !file.eof() && !done) {
		if( line.at(0)=='*') {
			done=true;
		}
		else {
			strParts.clear();
			strParts=IFParser::splitStrToStrs( line, (char*) " ,\t");

			nodeNr=atoi( strParts[0].c_str());
			value=atof( strParts[1].c_str());

			if( nodeNr==0) {
				tmpNodes=feJob.getNset( strParts[0]);

				for( int i=0; i<(int) tmpNodes.size(); i++) {
					nodeNr=tmpNodes[i];
					temperatureList[nodeNr]=value;
				}
			}
			else {
				temperatureList[nodeNr]=value;
			}
		}

		if( !done) {
			getline( file, line);
			lCount++;
		}
	}

	feJob.setInitialTemperatureNodes( temperatureList);

	return line;
}

//----------------------------------------
int IFParser::readTmpInputFile( FETask &feJob) {
	int errorNr=0;
	int lineCount=0;
	bool knownCommand=true;
	bool readNewLine=true;
	Step analysisStep;

	vector<string> strParts;

	ifstream file;
	string line, logMessage;

	logMessage="--------------------------------------------------------------------------------\nstart reading temporary inputfile --> ";
	logMessage.append( ifPathNameTmp);
	logMessage.append( " <--\n\n");
	appendToLogFile( logMessage);

	file.open( ifPathNameTmp.c_str(), ios::in);

	getline( file, line);
	lineCount++;

	while( !( file.eof())) {			// && ( errorNr==0)) {
		strParts.clear();

		strParts=splitStrToStrs( line, (char*) " ,*=\t");
		string &keyw0=strParts[0];
		//string &keyw1=strParts[1];
		string &keyw2=strParts[2];

		if( line.at(0)=='*') {			//commands start with '*'
			if( keyw0.compare( "HEADING")==0) {						//heading
				getHeading( file, feJob, lineCount);
			}
			else if( keyw0.compare( "NODE")==0) {					//nodes
				line=readNodes( file, feJob, strParts, lineCount);
				readNewLine=false;
			}
			else if( keyw0.compare( "NSET")==0) {					//nsets
				line=readNset( file, feJob, strParts, lineCount);
				readNewLine=false;
			}
			else if( keyw0.compare( "ELEMENT")==0) {				//elements
				line=readElements( file, feJob, strParts, lineCount);
				readNewLine=false;
			}
			else if( keyw0.compare( "MATERIAL")==0) {				//material
				line=readMaterial( file, feJob, strParts, lineCount);
				readNewLine=false;
			}
			else if( keyw0.compare( "ELSET")==0) {					//nsets
				line=readElset( file, feJob, strParts, lineCount);
				readNewLine=false;
			}
			else if( keyw0.compare( "SOLID")==0) {					//solid section
				readSolidSection( file, feJob, strParts, lineCount);
			}
			else if( keyw0.compare( "STEP")==0) {					//step
				string tmpStepName;

				if( strParts.size()<3) {
					tmpStepName="Step ";
					tmpStepName.append( IFParser::intToStr( feJob.getNrOfSteps()+1));
				}
				else {
					tmpStepName=keyw2;
				}

				analysisStep.setStepName( tmpStepName);
				analysisStep.setParsing( true);
			}
			else if( keyw0.compare( "BOUNDARY")==0) {				//BOUNDARY
				line=readBoundaries( file, feJob, analysisStep, strParts, lineCount);
				readNewLine=false;
			}
			else if( keyw0.compare( "STATIC")==0) {					//static
				if( analysisStep.getParsing()) {
					readStatic( file, analysisStep, strParts, lineCount);
				}
				else {
					knownCommand=false;
				}
			}
			else if( keyw0.compare( "CLOAD")==0) {					//cload
				if( analysisStep.getParsing()) {
					line=readCLoads( file, feJob, analysisStep, strParts, lineCount);
					readNewLine=false;
				}
				else {
					knownCommand=false;
				}
			}
			else if( keyw0.compare( "END")==0) {					//step end
				if( analysisStep.getParsing()) {
					errorNr=feJob.addStep( analysisStep);

					if( errorNr==STEP_EXISTS) {
						logMessage="error! - step ";
						logMessage.append( analysisStep.getStepName());
						logMessage.append( " already exists; last one will be used\n");
						logMessage.append( intToStr( lineCount));
						logMessage.append( " has been skipped\n");
						appendToLogFile( logMessage);

						errorNr=0;
						errors=true;
					}

					analysisStep.setParsing( false);
				}
				else {
					knownCommand=false;
				}
			}
			else if( keyw0.compare( "INITIAL")==0) {				//heading
				line=getInitialTemperature( file, feJob, lineCount);
				readNewLine=false;
			}
			else {
				knownCommand=false;
			}
		}
		else {
			logMessage="error! - line ";
			logMessage.append( intToStr( lineCount));
			logMessage.append( " has been skipped\n");
			appendToLogFile( logMessage);
			errors=true;
		}

		if( !knownCommand) {
			knownCommand=true;

			logMessage="error! - unknown or wrong placed or wrong used keyword --> ";
			logMessage.append( keyw0);
			logMessage.append( " <-- in temporary inputfile --> ");
			logMessage.append( ifPathNameTmp);
			logMessage.append( " <--\nline ");
			logMessage.append( intToStr( lineCount));
			logMessage.append( " has been skipped\n");
			appendToLogFile( logMessage);
			errors=true;
		}

		if( readNewLine) {
			getline( file, line);
			lineCount++;
		}
		else {
			readNewLine=true;
		}
	}

	//summary
	logMessage="\n--------------------------------------------------------------------------------\nsummary of --> ";
	logMessage.append( feJob.getHeading());
	logMessage.append( " <-- (");
	logMessage.append( ifPathName);
	logMessage.append( ")\n");
	logMessage.append( "nodes: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfNodes()));
	logMessage.append( "\n");
	logMessage.append( "nsets: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfNsets()));
	logMessage.append( "\n");
	logMessage.append( "elements: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfElements()));
	logMessage.append( "\n");
	logMessage.append( "elementsets: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfElsets()));
	logMessage.append( "\n");
	logMessage.append( "materials: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfMaterials()));
	logMessage.append( "\n");
	logMessage.append( "solid sections: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfSolidSections()));
	logMessage.append( "\n");
	logMessage.append( "initial temperature nodes: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfInitialTemperatureNodes()));
	logMessage.append( "\n");
	logMessage.append( "general boundary conditions: ");
	logMessage.append( IFParser::intToStr( feJob.getNrOfBoundaries()));
	logMessage.append( "\n");
	appendToLogFile( logMessage);

	logMessage="steps: ";
	int nr=feJob.getNrOfSteps();
	logMessage.append( IFParser::intToStr( nr));
	logMessage.append( "\n");
	appendToLogFile( logMessage);

	for( int i=0; i<nr; i++) {
		logMessage.clear();
		logMessage.append( "*step ");

		Step& tmpStep=feJob.getStepNr( i);

		logMessage.append( IFParser::intToStr( i+1));
		logMessage.append( ": '");
		logMessage.append( tmpStep.getStepName());
		logMessage.append( "'\n");
		logMessage.append( "  cloads: ");
		logMessage.append( IFParser::intToStr( tmpStep.getNrOfCLoads()));
		logMessage.append( "\n");
		logMessage.append( "  boundary conditions: ");
		logMessage.append( IFParser::intToStr( tmpStep.getNrOfBoundaries()));
		logMessage.append( "\n");
		appendToLogFile( logMessage);
	}

	if( errors) {
		logMessage="--------------------------------------------------------------------------------\nthere have been errors while parsing - see log-file for detials\n\n";

		appendToLogFile( logMessage);
		errorNr=ERR_IN_INPUTFILE;
	}

	time ( &rawTimeInterim);
	struct tm *timeinfo=localtime( &rawTimeInterim);

	feJob.setStartTime( rawTimeStart);
	feJob.setInterimTime( rawTimeInterim);

	logMessage="--------------------------------------------------------------------------------\nfinished reading temporary inputfile --> ";
	logMessage.append( ifPathName);
	logMessage.append( " <--\n");
	logMessage.append( asctime( timeinfo));
	logMessage.append( "T=");
	logMessage.append( IFParser::doubleToStr( difftime( rawTimeInterim, rawTimeStart)).c_str());
	logMessage.append( "s\n\n");
	appendToLogFile( logMessage);

	file.close();

	return errorNr;
}
