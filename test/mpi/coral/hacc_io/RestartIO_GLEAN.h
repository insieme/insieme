#ifndef _RESTART_IO_SINGLE_FILE
#define _RESTART_IO_SINGLE_FILE

#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <fcntl.h>
#include <unistd.h>

#include "mpi.h"

// Needed to get this working on OSX
#ifndef   pwrite64               
#define pwrite64  pwrite
#endif

#ifndef   pread64               
#define pread64 pread           
#endif


enum GLEAN_IO_MODE
{
    WRITE_CHECKPOINT,
    READ_RESTART,
    UNDEF_MODE
};


enum GLEAN_IO_INTERFACE
{
    USE_POSIX,
    USE_MPIIO, // Default
    UNDEF_INTERFACE
};

enum GLEAN_POSIX_API
{
    USE_READ_WRITE =  0,// Default
    USE_PREAD_PWRITE = 1
};

enum GLEAN_FILE_DISTRIBUTION
{
    GLEAN_SINGLE_FILE,
    GLEAN_FILE_PER_RANK,
    GLEAN_BGQ_ION_PART
};

enum GLEAN_MPIIO_FILE_PTR
{
    INDEPENDENT_FILE_PTR,
    SHARED_FILE_PTR
};


static const int GLEAN_MAX_STRING_LEN           = 8192;

// Set this to 24 MB for now
static const int64_t FILE_HEADER_SIZE_MAX		= 25165824;

static const int HEADER_METAINFO_SIZE          = 16;

class RestartIO_GLEAN
{

public:
    
    RestartIO_GLEAN ();
		
    ~RestartIO_GLEAN ();
		
    int Initialize (MPI_Comm comm);

    int Finalize (void);

    int CreateCheckpoint (char* path_prefix, int64_t& num_particles);
    
    int64_t OpenRestart (char* pathname);
	
    int Close (void);
	
    int Write ( float* xx, float* yy, float* zz,
                float* vx, float* vy, float* vz,
                float* phi, int64_t* pid,
                uint16_t* mask);

    // Return the number of particles for the current rank
    int Read (  float*& xx, float*& yy, float*& zz,
                float*& vx, float*& vy, float*& vz,
                float*& phi, int64_t*& pid,
                uint16_t*& mask);

    // Need some Get and Set Attributes to Tweak Performance
    int SetMPIIOSharedFilePointer(void);
    
    int SetMPIOIndepFilePointer (void);
    
    int SetFileDistribution (GLEAN_FILE_DISTRIBUTION file_dist);
    
    void EnablePreAllocateFile(void);
    
    void DisablePreAllocateFile(void);
    
    void SetMPI_IO_Interface(void);
    
    void SetPOSIX_IO_Interface(int val = 0);
    
    void PrintIOCoordInfo (void);
	
private:
    
    int __duplicateCommunicator (MPI_Comm comm);
    
    int __initalizePartitionInfo (void);
    
    int __createPartitions (void);
    
    int __destroyPartitions (void);
    
    #ifdef __bgq__
    int __initalize_BGQ_PartitionInfo (void);
    #endif

    // Associated with Checkpoints
    
    int __POSIX_Create (void );
    
    int __MPIIO_Create (void );
    
    
    int __POSIX_Close_Checkpoint (void);
    
    int __MPIIO_Close_Checkpoint (void);
    
    
    // Associated with Restarts
    
    int __POSIX_Read_Header (void);
    
    int __MPIIO_Read_Header (void);
    
    int __POSIX_Open_Restart (void);
    
    int __MPIIO_Open_Restart (void);
    
    int __POSIX_Close_Restart (void);
    
    int __MPIIO_Close_Restart (void);
    
    
    
    int __POSIX_Write_Data (const char *buf, int64_t* nbytes,
                            off_t start_off);
    
    int64_t __POSIX_Read_Data (unsigned char *buf, int64_t& nbytes,
                            off_t start_off);
    

    void __HandleMPIIOError (int errcode, char *str);
    

private:
		
    MPI_Comm m_globalComm; //  Global Communicator

    int m_globalCommSize; // Size of Global Communicator
		
    int m_globalCommRank; // Rank of Global Communicator
        
    int m_totPartitions; // Divide into partitions and write a file out per partition
    
    int m_partitionID; // 1D Partition ID
	
    int m_idInPartition; // Rank ID in the given partition
		
    MPI_Comm m_partitionComm;

    int m_partitionSize;

    int m_partitionRank;
    
    char* m_basePathName; // Root Path Name
    
    int m_basePathNameLen;
    
    char* m_partFileName; // File of Partition
    
    int m_partFileNameLen;
    
    int64_t m_localParticles;
    
    int64_t m_totPartParticles;
    
    int64_t m_totGlobalParticles;
    
    GLEAN_IO_MODE m_mode; // Current Access Mode
    
    GLEAN_IO_INTERFACE m_interface; // Which IO Interface to Use
    
    GLEAN_POSIX_API m_posixAPI;
    
    MPI_File m_fileHandle; // MPI File Handle
    
    GLEAN_MPIIO_FILE_PTR m_filePtrMode;
    
    MPI_Comm m_fileComm; // Shared or independent Ptr for File
    
    int m_posixFD; // POSIX Descriptor
    
    int64_t m_partFileSize; // Valid only on the Part Comm Root
    
    int64_t* m_header;

    int64_t m_headerSize;

    // Performance Related Information
    
    double m_startTime;

    double m_endTime;
    
    // Performance Tweaking Parameters
    int m_preallocFile;
    
   GLEAN_FILE_DISTRIBUTION m_fileDist;

};


#endif

