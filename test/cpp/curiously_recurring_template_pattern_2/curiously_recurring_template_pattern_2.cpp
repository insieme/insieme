
#include <string>
#include <vector>

template<class T_DERIVED,class T_RET>
class FSM
{
	// Actually a boost::multi_array would be the best choice here,
	// but boost shall not be used in this software, so a vector of 
	// vectors is used
	typedef T_RET(T_DERIVED::*mtdPtr)();
	
	typedef std::vector<mtdPtr> VMTDPTR;
	typedef std::vector<VMTDPTR> FptrArrayType;	
	
	typedef std::vector<unsigned> VUINT;
	typedef std::vector<VUINT> NextStateArrayType;

public:
	unsigned currentState;
	
protected:
	FSM(unsigned dimX_,unsigned dimY_);
	void setEntry(unsigned state, unsigned event,mtdPtr,unsigned nextState);
	void runFSM(unsigned initState,unsigned initEvent);
	void init(unsigned initState);
	unsigned getState() const;
	virtual T_RET inv()=0 ;
	virtual ~FSM();

	unsigned dimX,dimY;
	FptrArrayType fpntTab;
	NextStateArrayType nextStateTab;
};

// Constructor
template<class T_DERIVED,class T_RET>
FSM<T_DERIVED,T_RET >::FSM (unsigned dimX_,unsigned dimY_):
dimX(dimX_),dimY(dimY_)
{
	for(unsigned i=0;i<dimX;i++)
	{
		fpntTab.push_back(VMTDPTR(dimY,&T_DERIVED::inv));
		nextStateTab.push_back(VUINT(dimY,0));
	}
}

// Destructor
template<class T_DERIVED,class T_RET>
FSM<T_DERIVED,T_RET >::~FSM (){}

// Just for debugging
template<class T_DERIVED,class T_RET>
unsigned FSM<T_DERIVED,T_RET >::getState() const
{
	return currentState;
}

// Set up a transistion
template<class T_DERIVED,class T_RET>
void FSM<T_DERIVED,T_RET >::setEntry(unsigned state, unsigned event, mtdPtr fp,unsigned nextState)
{
	fpntTab[state][event]=fp;
	nextStateTab[state][event]=nextState;
}

// Initialize with a state
template<class T_DERIVED,class T_RET>
void FSM<T_DERIVED,T_RET >::init(unsigned initState)
{
    currentState=initState;
}


class AlgorithmIO : public FSM<AlgorithmIO,void>
{
public:
	AlgorithmIO(const std::string& idString_) : FSM<AlgorithmIO,void>(100,100) {};
	~AlgorithmIO() {};
	void inv() {};
protected:
/*
	virtual void loadArguments();
	void readGeom();
	void writeGeom();
	void run();
	void runFSM(IOSt initState);
	IOEv getEvent(const std::string& sLine);
	void act_nop();
	void act_getHeadInfo();
	void act_getTriangles();
	void act_getVertices();
	void act_getFeatures();
	void act_getNeigs();
	void act_getPidFiles();
	void act_getTPidIdx();
	void act_getOrgVtx();
	
	std::string getGzLine();
	void setupGeomfileFSM();
	void registerTag(const std::string& s,IOEv ev);
	std::string geomFileIn,geomFileOut;
	std::map<rex,IOEv> mRex2Ev;
	gzFile gzInfile;
	std::string userFriendlyState(unsigned st) const;
	std::string userFriendlyEvent(unsigned ev) const;
	void writeHead(std::ostringstream& s);
	void removeCollinearTriangles();
	unsigned getCollinearCenterIdx(Triangle* t);
	void split12(Triangle* t,Point_3* splitPoint);
	int geomFile_formatVersion;
	int geomFile_buildNumber;
	int geomFile_numTriangles;
	IORT geomFile_realType;
	std::vector<Point_3*> vOrgPPtrFromFile;
	std::vector<Point_3*> vPPtrFromFile;
	std::vector<PartID*> vPidPointers;
	std::vector<int> vTPidIdx;
*/
};

int main() {
	
	AlgorithmIO("test");

}
