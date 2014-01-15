#include "RestartIO_GLEAN.h"
#include "GLEAN_Util_Printmessage.h"

#ifdef __bgq__
// Needed for the various Personality functions
#include <spi/include/kernel/location.h>
#include <spi/include/kernel/process.h>
#include <firmware/include/personality.h>
#endif

#include <iostream>
using namespace std;


RestartIO_GLEAN::RestartIO_GLEAN () :
                                    m_basePathName (0),
                                    m_basePathNameLen (GLEAN_MAX_STRING_LEN),
                                    m_partFileName (0),
                                    m_partFileNameLen (GLEAN_MAX_STRING_LEN),
                                    m_localParticles (-1),
                                    m_totPartParticles (-1),
                                    m_totGlobalParticles (-1),
                                    m_mode(UNDEF_MODE),
                                    m_interface (USE_MPIIO),
                                    m_posixAPI (USE_READ_WRITE),
                                    m_filePtrMode (SHARED_FILE_PTR),
                                    m_startTime(0),
                                    m_endTime (0),
                                    m_preallocFile(1),
                                    m_fileDist (GLEAN_SINGLE_FILE)
{
    // \TODO : Make this more dynamic. For now this will work on 96 Sequoia Racks
    m_headerSize = FILE_HEADER_SIZE_MAX;
    
    #ifdef __bgq__
    m_fileDist = GLEAN_BGQ_ION_PART;
    //m_interface = USE_POSIX;
    #endif

}

RestartIO_GLEAN::~RestartIO_GLEAN ()
{
    
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------

int RestartIO_GLEAN::Initialize (MPI_Comm comm)
{
    int status = -1;
    
    status = this->__duplicateCommunicator(comm);
    assert (status == 0);
    
    status = this->__initalizePartitionInfo();
    assert (status == 0);
    
    status = this->__createPartitions();
    assert (status == 0);
    
    return 0;
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------

int RestartIO_GLEAN::Finalize (void)
{
    int status = -1;
    
    // Set the Mode to Undefined
    m_mode = UNDEF_MODE;
    
    // Destroy the Partitions Created
    status =  this->__destroyPartitions();
    assert (status == 0);
    
    // Destroy the Global Communicator
    status =  MPI_Comm_free (&m_globalComm);
    assert(status == 0);
    
    return 0;
}


//  ---------------------------------------------------------------------------
//  Create Checkpoint
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: CreateCheckpoint (char* pathname, int64_t& num_particles)
{
    int status =0;
    
    m_mode = WRITE_CHECKPOINT;
    
    m_startTime = MPI_Wtime();
    
    m_localParticles = num_particles;
    
    // Set the Base Path Name
    m_basePathName = new char [GLEAN_MAX_STRING_LEN];
    memset (m_basePathName, '\0', GLEAN_MAX_STRING_LEN * sizeof(char));
	
    strncpy (m_basePathName, pathname, strlen(pathname));
    
    m_basePathNameLen = GLEAN_MAX_STRING_LEN;
    
    // Set the Partition FileName
    m_partFileName = new char [GLEAN_MAX_STRING_LEN];
    memset (m_partFileName, '\0', GLEAN_MAX_STRING_LEN * sizeof(char));
    
    m_partFileNameLen = GLEAN_MAX_STRING_LEN;
    
    snprintf (m_partFileName, m_partFileNameLen, "%s-Part%08d-of-%08d.data",
              m_basePathName, m_partitionID, m_totPartitions);
    
    //	----------------------------------------
    //   Total number of particles in Partition
    //	----------------------------------------
    MPI_Allreduce(&m_localParticles, &m_totPartParticles, 1, MPI_LONG_LONG,
                  MPI_SUM, m_partitionComm);
    
    // Total Gloabl Particles 
    MPI_Allreduce(&m_localParticles, &m_totGlobalParticles, 1, MPI_LONG_LONG,
                  MPI_SUM, m_globalComm);
    
    // \TODO : Make this more dynamic. For now this will work on 96 Sequoia Racks
    m_headerSize = FILE_HEADER_SIZE_MAX;
	
    //	----------------------------------------------
    //	Calculate the total Size of the Partition file
    //	----------------------------------------------
    int64_t record_size,  tot_file_size;
    
    record_size = (7 * sizeof(float)) + sizeof(int64_t) + sizeof(uint16_t);
    
    tot_file_size = ((m_totPartParticles * record_size ) ) + m_headerSize;
    m_partFileSize = tot_file_size;
    
    
    // Create the header on rank 0 of the partition
    if ( 0 == m_partitionRank)
    {
        m_header = new int64_t [ m_headerSize /(sizeof(int64_t))];
        memset (m_header, 0, m_headerSize);
        m_header[0] = 0; // 0 = Invalid  and 1 = valid -- Unused for now
        m_header[1] = (int64_t)HEADER_METAINFO_SIZE; //
        m_header[2] = m_headerSize;
        m_header[3] = record_size;
        m_header[4] = m_totPartParticles;
        m_header[5] = m_totGlobalParticles;
        m_header[6] = (int64_t)m_partitionSize;
        m_header[7] = (int64_t)m_totPartitions;
        m_header[8] = (int64_t)m_partitionID;
    }
	
    // Gather the particle count from each rank in Partition
    MPI_Gather(&num_particles, 1, MPI_LONG_LONG,
               &m_header[HEADER_METAINFO_SIZE], 1, MPI_LONG_LONG,
               0, m_partitionComm);
    
    
    // Gather all the Global ranks in the partition
    // Note: This is a int64_t and need to change this in future to int
    int64_t hrank = (int64_t)m_globalCommRank;
    
    MPI_Gather(&hrank, 1, MPI_LONG_LONG,
               &m_header[HEADER_METAINFO_SIZE + m_partitionSize],
               1, MPI_LONG_LONG, 0, m_partitionComm);
               
    
    // Create the Checkpoint File and Allocate Space if set
    // Open the Checkpoint file handle.
    switch (m_interface)
    {
        case USE_POSIX:
            status = this->__POSIX_Create();
            break;
        case USE_MPIIO:
            status = this->__MPIIO_Create();
            break;
        default:
            status = this->__MPIIO_Create();
            break;
    }

    return status;
}




//  ---------------------------------------------------------------------------
//  Purpose: Create the File using POSIX I/O
//          Preallocate the File if set
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __POSIX_Create (void )
{
    int retval;
    
    // Create the File on root and set the file size if needed
    if ( 0 == m_partitionRank)
    {
        retval = ::open((char *)m_partFileName, O_CREAT | O_WRONLY, 0664);
        assert (retval > 0);
        
        m_posixFD = retval;
        
        if (1 == m_preallocFile)
        {
            //retval = ::ftruncate64(m_posixFD, m_partFileSize);
            retval = ::ftruncate(m_posixFD, m_partFileSize);
            if (retval == -1)
            {
                GLEAN_PRINT_PERROR(" Error Opening %s \n", m_partFileName);
            }
            //assert (retval == 0);
        }
        ::close (m_posixFD);
    }

    MPI_Barrier (m_partitionComm);

    // Open the file for writing on all ranks of partition
    retval = ::open((char *)m_partFileName, O_WRONLY, 0664);
    if (retval == -1)
    {
        GLEAN_PRINT_PERROR(" Error Ftruncating %s \n", m_partFileName);
    }
    //assert (retval > 0);
    
    m_posixFD = retval;
    
    MPI_Barrier (m_partitionComm);
    
    return 0;
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __MPIIO_Create (void )
{
    int retval;
    
    // Set the File Point Mode
    switch (m_filePtrMode)
    {
        case INDEPENDENT_FILE_PTR:
            m_fileComm = MPI_COMM_SELF;
            break;
        case SHARED_FILE_PTR:
            m_fileComm = m_partitionComm;
            break;
        default:
            m_fileComm = m_partitionComm;
            break;
    }
    
    
    //  ------------------------
    //   Create and Preallocate the File On Partition Room
    //  ------------------------
    if ( 0 == m_partitionRank)
    {
        retval = MPI_File_open(MPI_COMM_SELF, (char *)m_partFileName,
                               MPI_MODE_WRONLY | MPI_MODE_CREATE,
                               MPI_INFO_NULL,&m_fileHandle);
        assert(retval == MPI_SUCCESS);
        
        if (1 == m_preallocFile)
        {
            MPI_File_set_size(m_fileHandle, m_partFileSize);
        }
        
        MPI_File_close (&m_fileHandle);
        
    }
    
    MPI_Barrier (m_partitionComm);
    
    //  ---------------------------
    //   Open the File for Writing
    //  ---------------------------
    
    retval = MPI_File_open(m_fileComm,
                           (char *)m_partFileName,
                           MPI_MODE_WRONLY,
                           MPI_INFO_NULL,
                           &m_fileHandle);
    assert(retval == MPI_SUCCESS);
    
    MPI_Barrier (m_partitionComm);
    
    return 0;
}


//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------

int64_t RestartIO_GLEAN :: OpenRestart (char* pathname)
{
    int status = 0;
    
    m_startTime = MPI_Wtime();
    
    m_localParticles =-1;
    m_totPartParticles = -1;
    m_totGlobalParticles = -1;
    m_mode = READ_RESTART;
    
    // Set the Base Path Name
    m_basePathName = new char [GLEAN_MAX_STRING_LEN];
    memset (m_basePathName, '\0', GLEAN_MAX_STRING_LEN * sizeof(char));
	
    strncpy (m_basePathName, pathname, strlen(pathname));
    
    m_basePathNameLen = GLEAN_MAX_STRING_LEN;
    
    // Set the Partition FileName
    m_partFileName = new char [GLEAN_MAX_STRING_LEN];
    memset (m_partFileName, '\0', GLEAN_MAX_STRING_LEN * sizeof(char));
    
    m_partFileNameLen = GLEAN_MAX_STRING_LEN;
    
    snprintf (m_partFileName, m_partFileNameLen, "%s-Part%08d-of-%08d.data",
              m_basePathName, m_partitionID, m_totPartitions);
   
    // Populate the Header
    switch (m_interface)
    {
        case USE_POSIX:
            status = this->__POSIX_Read_Header();
            break;
        case USE_MPIIO:
            status = this->__MPIIO_Read_Header();
            break;
        default:
            status = this->__MPIIO_Read_Header();
            break;
    }

    // Scatter the particles count
    MPI_Scatter(&m_header[HEADER_METAINFO_SIZE],
                1, MPI_LONG_LONG, &m_localParticles, 1,
                MPI_LONG_LONG, 0, m_partitionComm);
    
    int64_t rank_in_file, g_rank;
    
    // Scatter the global ranks
    MPI_Scatter(&m_header[HEADER_METAINFO_SIZE + m_partitionSize],
                1, MPI_LONG_LONG, &rank_in_file,
                1, MPI_LONG_LONG, 0, m_partitionComm);
    
    g_rank = (int64_t)m_globalCommRank;
    
    // Check that the same global rank is reading it back up
    assert(g_rank == rank_in_file );
    
    switch (m_interface)
    {
        case USE_POSIX:
            status = this->__POSIX_Open_Restart();
            break;
        case USE_MPIIO:
            status = this->__MPIIO_Open_Restart();
            break;
        default:
            status = this->__MPIIO_Open_Restart();
            break;
    }
    
    return m_localParticles;
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __POSIX_Open_Restart(void)
{
    int status;
    status = ::open((char *)m_partFileName, O_RDONLY, 0664);
    if (status == -1)
    {
        GLEAN_PRINT_PERROR(" Error Opening File: %s \n", m_partFileName);
    }
    
    m_posixFD = status;
    
    MPI_Barrier (m_partitionComm);
    
    return 0;
}
                                               
//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------                                               
int RestartIO_GLEAN :: __MPIIO_Open_Restart(void)
{
   int status;
   
   switch (m_filePtrMode)
   {
       case INDEPENDENT_FILE_PTR:
           m_fileComm = MPI_COMM_SELF;
           break;
       case SHARED_FILE_PTR:
           m_fileComm = m_partitionComm;
           break;
       default:
           m_fileComm = m_partitionComm;
           break;
   }
   
   status = MPI_File_open(m_fileComm,
                          (char *)m_partFileName,
                          MPI_MODE_RDONLY,
                          MPI_INFO_NULL,
                          &m_fileHandle);
   assert(status == MPI_SUCCESS);
   
   MPI_Barrier (m_partitionComm);
   
   
   return 0;
}
                                               

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __POSIX_Read_Header(void)
{
    int status;
    off_t ofst;
    int64_t nread;
    
    if (0 == m_partitionRank)
    {
        
        m_header = new int64_t [ m_headerSize / sizeof(int64_t)];
        
        status = ::open((char *)m_partFileName, O_RDONLY, 0664);
        assert (status > 0);
        m_posixFD = status;
        
        ofst = 0;
        nread = m_headerSize;
        status = __POSIX_Read_Data ( (unsigned char*)m_header, nread, ofst);
        
        ::close(m_posixFD);
    }
    
    MPI_Barrier (m_partitionComm);
    
    return 0;
}

                                               
//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __MPIIO_Read_Header(void)
{
    MPI_Status status;
    int errcode;
    off_t ofst;
    
    if (0 == m_partitionRank)
    {
        
        m_header = new int64_t [ m_headerSize / sizeof(int64_t)];
        
        errcode = MPI_File_open(MPI_COMM_SELF, (char *)m_partFileName,
                               MPI_MODE_RDONLY,
                               MPI_INFO_NULL,&m_fileHandle);
        assert(errcode == MPI_SUCCESS);
        
        ofst = 0;
        errcode = MPI_File_read_at (m_fileHandle, ofst, m_header,
                                   m_headerSize, MPI_BYTE, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At Header");
        }
        
        MPI_File_close (&m_fileHandle);
    }
    
    MPI_Barrier (m_partitionComm);
    
    return 0;
}
                
                                               


//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------

int RestartIO_GLEAN :: __POSIX_Write_Data (const char* buf,
                                           int64_t* nbytes,
                                           off_t start_off)
{
        
    int64_t nleft, nwrite, totwrite = 0;
    int myerr;
    
    nleft = *nbytes;
        
    while (nleft > 0)
    {
        // Use the appropriate POSIX API
        if (m_posixAPI == USE_READ_WRITE)
        {
        
            if (-1 == lseek(m_posixFD, start_off, SEEK_SET))
            {
                GLEAN_PRINT_PERROR (" Lseek Error: Rank %d - %s \n", \
                                m_globalCommRank, m_partFileName);
                return -1;
            }
            
            nwrite = write(m_posixFD, buf, nleft);
        }
        else
        {
            nwrite = pwrite64 ( m_posixFD, buf, nleft, start_off);
        }
            
        if (nwrite > 0)
        {
            nleft -= nwrite;
            buf += nwrite;
            totwrite += nwrite;
            start_off += nwrite;
        }
        else
        {
            if (0 == nwrite)
                continue;
                
            myerr = errno;
            switch (myerr)
            {
                #ifdef __APPLE__
                case EWOULDBLOCK:
                #endif
                #ifdef __linux__
                case EAGAIN:
                #endif
                case EINTR:
                    continue;
                    break;
                default:
                    GLEAN_PRINT_PERROR (" Write Error: Rank %d  \n", \
                                        m_globalCommRank);
    
                    *nbytes =  totwrite;
                    return -1;
                    break;
            }
        }
    }
    
    *nbytes =  totwrite;
    
    return 0;
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
                                               
int64_t RestartIO_GLEAN :: __POSIX_Read_Data (  unsigned char* buf,
                                                int64_t& len,
                                                off_t start_off)
{
    int64_t nread = 0, totread = 0;
    int64_t nleft =  len ;
    off_t offset = start_off ;
    int myerr, retval;
    
    while (nleft > 0)
    {
        errno = 0;
        
        // Use the appropriate POSIX API
        if (m_posixAPI == USE_READ_WRITE)
        {
        
            retval = ::lseek (m_posixFD, offset, SEEK_SET);
            if ( -1 == retval)
            {
                GLEAN_PRINT_PERROR (" Error Seeking in File %s %lld \n", \
                                m_partFileName, offset);
                return -1;
            }
        
            nread = ::read(m_posixFD,buf,nleft);
        }
        else
        {
        
            nread = ::pread64 ( m_posixFD, buf, nleft, offset);
        }
            
        if (nread > 0)
        {
            nleft -= nread;
            buf += nread;
            totread += nread;
            offset += nread;
        }
        else if (nread < 0)
        {
            myerr = errno;
            
            switch (myerr)
            {
                case EINTR:
                    nread = 0;
                    continue;
                    break;
                    
                default:
                    return -1;
                    break;
            }
        }
        else
        {
            if (nleft == 0)
                break;
            else
            {
                return -1;
                break;
            }
        }
        
    }
    
    return totread;
}
                                               
                                               
//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: Close (void)
{
    //  ----------------
    //  Close the File
    //  ----------------
    int status = -1;
    
    if (m_mode == WRITE_CHECKPOINT)
    {
        // Close the Cehckpoint File
        switch (m_interface)
        {
            case USE_POSIX:
                status = this->__POSIX_Close_Checkpoint();
                break;
            case USE_MPIIO:
                status = this->__MPIIO_Close_Checkpoint();
                break;
        }
    }
    else if (m_mode == READ_RESTART)
    {
        // Close the Restart File
        switch (m_interface)
        {
            case USE_POSIX:
                status = this->__POSIX_Close_Restart();
                break;
            case USE_MPIIO:
                status = this->__MPIIO_Close_Restart();
                break;
        }

    }
    else
    {
        // Unknown MODE
        status = -1;
    }
    
    
    //  --------------------
    //  Compute Statistics
    //  --------------------
    double tot_time, agg_max_time =0, agg_min_time, agg_avg_time;
    double part_max_time, part_min_time;
    int64_t tot_data_size, part_size;
    
    tot_time = m_endTime - m_startTime;
    
    part_size = 0;
    if (0 ==m_partitionRank)
        part_size = m_partFileSize;
    
    //  Compute Global Stats
    MPI_Reduce (&tot_time, &agg_max_time, 1, MPI_DOUBLE, MPI_MAX, 0, m_globalComm);
    MPI_Reduce (&tot_time, &agg_min_time, 1, MPI_DOUBLE, MPI_MIN, 0, m_globalComm);
    MPI_Reduce (&part_size, &tot_data_size, 1, MPI_LONG_LONG, MPI_SUM, 0, m_globalComm);
    
    
    if (0 == m_globalCommRank)
    {
        cout << "-------- Aggregate Performance --------" << endl;
        
        double bw = ((double)tot_data_size)/ agg_max_time / (1024.0 * 1024.0);
        switch (m_mode)
        {
            case READ_RESTART:
                cout << " READ Restart Perf: " << bw << " BW[MB/s] " << tot_data_size << " Bytes " << agg_max_time << " MaxTime[sec] " <<  endl;
                break;
            case WRITE_CHECKPOINT:
                cout << " WRITE Checkpoint Perf: " << bw <<" BW[MB/s] " << tot_data_size << " Bytes " << agg_max_time << " MaxTime[sec] " <<  endl;
                break;
        }
        
    }
    
    /*
    //  Compute the Stats for the Partition
    MPI_Reduce (&tot_time, &part_max_time, 1, MPI_DOUBLE, MPI_MAX, 0, m_partitionComm);
    MPI_Reduce (&tot_time, &part_min_time, 1, MPI_DOUBLE, MPI_MIN, 0, m_partitionComm);
    if (0 == m_partitionRank)
    {
        cout << "-------- Partition Performance: ID  " << m_partitionID << " Of " << m_totPartitions   << endl;
        double bw = ((double)m_partFileSize)/ part_max_time / (1024.0 * 1024.0);
        
        switch (m_mode)
        {
            case READ_RESTART:
                cout << " READ Restart Perf: " << bw << " BW[MB/s] " << m_partFileSize << " Bytes " << part_max_time << " MaxTime[sec] " <<  endl;
                break;
            case WRITE_CHECKPOINT:
                cout << " WRITE Checkpoint Perf: " << bw <<" BW[MB/s] " << m_partFileSize << " Bytes " << part_max_time << " MaxTime[sec] " <<  endl;
                break;
        }
        
    }
    */
    
    
    //  ---------------------
    //  Delete Allocated Data
    //  ---------------------
    if (m_partFileName)
    {
        delete [] m_partFileName;
        m_partFileName = 0;
    }
    
    if (m_basePathName)
    {
        delete [] m_basePathName;
        m_basePathName = 0;
    }
    
    //  Need to reset various member variables
    m_localParticles =-1;
    m_totPartParticles = -1;
    m_totGlobalParticles = -1;
    m_mode = UNDEF_MODE;
    
    return 0;
}


int RestartIO_GLEAN :: SetMPIIOSharedFilePointer (void)
{
	m_filePtrMode = SHARED_FILE_PTR;
	
	return 0;
}

int RestartIO_GLEAN :: SetMPIOIndepFilePointer (void)
{
    m_filePtrMode = INDEPENDENT_FILE_PTR;
    
    return 0;
}

int RestartIO_GLEAN :: SetFileDistribution (GLEAN_FILE_DISTRIBUTION file_dist)
{
    m_fileDist = file_dist;
    return 0;
}

void RestartIO_GLEAN :: EnablePreAllocateFile (void)
{
    m_preallocFile = 1;
    return;
}

void RestartIO_GLEAN :: DisablePreAllocateFile (void)
{
    m_preallocFile = 0;
    return;
}



void RestartIO_GLEAN :: SetMPI_IO_Interface (void)
{
    m_interface = USE_MPIIO;
    return;
}



void RestartIO_GLEAN :: SetPOSIX_IO_Interface (int val)
{
    m_interface = USE_POSIX;
    
    if (val == 1)
        m_posixAPI = USE_PREAD_PWRITE;
    else
        m_posixAPI = USE_READ_WRITE;
    
    return;
}


//----------------------------------------------------------------------------
//  MPI error handler
//----------------------------------------------------------------------------
void RestartIO_GLEAN::__HandleMPIIOError (int errcode, char *str)
{
    char msg[MPI_MAX_ERROR_STRING];
    int resultlen;
    MPI_Error_string(errcode, msg, &resultlen);
    fprintf(stderr, "%s: %s\n", str, msg);
    
	return;
}

void RestartIO_GLEAN::PrintIOCoordInfo (void)
{
    if ( 0 == m_globalCommRank)
    {
        cout << "----------------------------------------------" << endl;
        
        switch (m_fileDist)
        {
            case GLEAN_FILE_PER_RANK:
                cout << " Mode: Single Shared File" << endl;
                break;
            
            #ifdef __bgq__
            case GLEAN_BGQ_ION_PART:
                // BGQ Mechanism
                cout << " Mode: BGQ File per ION" << endl;
                break;
            #endif
                
            case GLEAN_SINGLE_FILE:
            default:
                cout << " Mode: Single Shared File" << endl;
                break;
        }
        
        cout << "----------------------------------------------" << endl;
        cout << "G: R Sz P: Tot PID IDinP RinP PSz " << endl;
        
    }
    
    // Prints Global Rank:Size #ofParts, PartID, IDinPart PartComm Rank:Size
    cout << "G: " << m_globalCommRank << " " << m_globalCommSize << " P: "  \
    << m_totPartitions << " " << m_partitionID << " " << m_idInPartition \
    << " " << m_partitionRank << " " << m_partitionSize << endl;
    
    
    MPI_Barrier (m_globalComm);
    
    return;
}


int RestartIO_GLEAN::__duplicateCommunicator (MPI_Comm comm)
{
    int status = -1;
    
    status = MPI_Comm_dup (comm, &m_globalComm);
    assert(status == MPI_SUCCESS);
    
    MPI_Comm_size (m_globalComm, &m_globalCommSize);
    MPI_Comm_rank (m_globalComm, &m_globalCommRank);
    
    return 0;
}



int RestartIO_GLEAN::__initalizePartitionInfo (void)
{    
    switch(m_fileDist)
    {
        case GLEAN_SINGLE_FILE:
            // Single Shared File
            m_totPartitions = 1;
            m_partitionID = 0;
            m_idInPartition = m_globalCommRank;
            
            break;
        
        case GLEAN_FILE_PER_RANK:
            // File Per Rank
            m_totPartitions = m_globalCommSize;
            m_partitionID = m_globalCommRank;
            m_idInPartition = 0;
            
            break;
        
        #ifdef __bgq__
        case GLEAN_BGQ_ION_PART:
            // BGQ Mechanism
            this->__initalize_BGQ_PartitionInfo();
            break;
        #endif
            
        default:
            // Single Shared File
            m_totPartitions = 1;
            m_partitionID = 0;
            m_idInPartition = m_globalCommRank;
            
            break;
            
    }
    
    return 0;
}

#ifdef __bgq__
int RestartIO_GLEAN::__initalize_BGQ_PartitionInfo (void)
{
    Personality_t pers;
    
    //  Nodes in an ION Partition
    int SPLIT_A = 2;
    int SPLIT_B = 2;
    int SPLIT_C = 4;
    int SPLIT_D = 4;
    int SPLIT_E = 2;
    
    // Get the Personality  Information
    Kernel_GetPersonality(&pers, sizeof(pers));

    int Anodes, Bnodes, Cnodes, Dnodes, Enodes;
    int Acoord, Bcoord, Ccoord, Dcoord, Ecoord;
    int A_color, B_color, C_color, D_color, E_color;
    int A_blocks, B_blocks, C_blocks, D_blocks, E_blocks;
    uint32_t id_on_node;
    int ranks_per_node, color;
    
    Anodes = pers.Network_Config.Anodes;
    Acoord = pers.Network_Config.Acoord;
    
    Bnodes = pers.Network_Config.Bnodes;
    Bcoord = pers.Network_Config.Bcoord;
    
    Cnodes = pers.Network_Config.Cnodes;
    Ccoord = pers.Network_Config.Ccoord;
    
    Dnodes = pers.Network_Config.Dnodes;
    Dcoord = pers.Network_Config.Dcoord;
    
    Enodes = pers.Network_Config.Enodes;
    Ecoord = pers.Network_Config.Ecoord;
    
    A_color  = Acoord /  SPLIT_A;
    B_color  = Bcoord /  SPLIT_B;
    C_color  = Ccoord /  SPLIT_C;
    D_color  = Dcoord /  SPLIT_D;
    E_color  = Ecoord /  SPLIT_E;
    
    // Number of blocks
    A_blocks = Anodes / SPLIT_A;
    B_blocks = Bnodes / SPLIT_B;
    C_blocks = Cnodes / SPLIT_C;
    D_blocks = Dnodes / SPLIT_D;
    E_blocks = Enodes / SPLIT_E;
    
    color = (A_color * (B_blocks * C_blocks * D_blocks * E_blocks))
            + (B_color * (C_blocks * D_blocks * E_blocks))
            + (C_color * ( D_blocks * E_blocks))
            + (D_color * ( E_blocks))
            + E_color;

    m_totPartitions = A_blocks * B_blocks * C_blocks * D_blocks * E_blocks;
    m_partitionID = color;
    
    // Have MPI decide on the rank ordering ??
    // Or, do this on our own?
    m_idInPartition = m_globalCommRank;
    /*
    id_on_node = Kernel_ProcessorID();
    ranks_per_node = m_globalCommSize / (Anodes * Bnodes * Cnodes * Dnodes * Enodes);
    
    int A_id, B_id, C_id, D_id, E_id, T_id;
    
    A_id = Acoord % SPLIT_A;
    B_id = Bcoord % SPLIT_B;
    C_id = Ccoord % SPLIT_C;
    D_id = Dcoord % SPLIT_D;
    E_id = Ecoord % SPLIT_E;
    T_id = id_on_node;
    
    m_idInPartition = (A_id * (SPLIT_B * SPLIT_C * SPLIT_D * SPLIT_E * ranks_per_node))
        + (B_id * (SPLIT_C * SPLIT_D * SPLIT_E * ranks_per_node))
        + (C_id * (SPLIT_D * SPLIT_E * ranks_per_node))
        + (D_id * (SPLIT_E * ranks_per_node))
        + (E_id * (ranks_per_node))
        + T_id;
    
    */
    
    
    return 0;
}

#endif

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------

int RestartIO_GLEAN::__createPartitions (void)
{
    int status = -1;
    int mycolor, mykey; // Color and Key passed to comm_split
    
    mycolor = m_partitionID;
    mykey = m_idInPartition;
 
    status = MPI_Comm_split (m_globalComm, mycolor, mykey, &m_partitionComm);
    assert(status == MPI_SUCCESS);
        
    MPI_Comm_size (m_partitionComm, &m_partitionSize);
    MPI_Comm_rank(m_partitionComm, &m_partitionRank);
    
    return 0;
}


//  ---------------------------------------------------------------------------
//  Destroy the Paartition Communicator 
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN::__destroyPartitions (void)
{
    int status;
    
    status =  MPI_Comm_free (&m_partitionComm);
    assert(status == MPI_SUCCESS);
    
    return 0;
}



//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __MPIIO_Close_Checkpoint (void)
{
    MPI_Status status;
    int errcode;
    off_t ofst;
    
    // Have Root Write the Header Out
    if (0 == m_partitionRank)
    {
        ofst = 0;
        
        errcode = MPI_File_write_at (m_fileHandle, ofst,m_header,
                                     m_headerSize, MPI_BYTE, &status);
        if (MPI_SUCCESS != errcode)
        {
            this->__HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At HEADER");
        }
        
        if (m_header)
        {
            delete [] m_header;
            m_header = 0;
        }
    }
    
    errcode  = MPI_File_close(&m_fileHandle);
    assert (errcode == MPI_SUCCESS);
    
    MPI_Barrier(m_partitionComm);
    
    m_endTime = MPI_Wtime();

    return 0;
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __MPIIO_Close_Restart (void)
{
    int status;
    
    status  = MPI_File_close(&m_fileHandle);
    assert (status == MPI_SUCCESS);
    
    MPI_Barrier(m_partitionComm);
    
    m_endTime = MPI_Wtime();
    
    return status;
}


//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __POSIX_Close_Checkpoint (void)
{
    int status;
    off_t ofst;
    int64_t nwrite;
    
    // Have Root Write the Header Out
    if (0 == m_partitionRank)
    {
        ofst = 0;
        nwrite = m_headerSize;
        status = __POSIX_Write_Data ((const char*)m_header, &nwrite, ofst);
        
        if (m_header)
        {
            delete [] m_header;
            m_header = 0;
        }
    }
    MPI_Barrier(m_partitionComm);
    
    status = ::close (m_posixFD);
    if (status == -1)
    {
        GLEAN_PRINT_PERROR("Error Closing POSIX File %s in Restart \n", \
                           m_partFileName);
    }
    
    // Wait for all clients in partition to close
    MPI_Barrier(m_partitionComm);
    
    m_endTime = MPI_Wtime();
    
    return status;
}


//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __POSIX_Close_Restart(void)
{
    int status;
     
    status = ::close (m_posixFD);
    if (status == -1)
    {
        GLEAN_PRINT_PERROR("Error Closing POSIX File %s in Restart \n", \
                          m_partFileName);
    }
    
    // Wait for all clients in partition to close
    MPI_Barrier(m_partitionComm);
    
    m_endTime = MPI_Wtime();
    
    return status;
}



//----------------------------------------------------------------------------
//
// Writes checkpoint restart file
//
// num_particles: number of particles in this block
// xx, yy, zz: particle positions
// vx, vy, vz: particle velocities
// phi: particle potentials
// pid: particle IDs
// mask: unused
//
//----------------------------------------------------------------------------
int RestartIO_GLEAN :: Write(float *xx, float *yy, float *zz,
                             float *vx, float *vy, float *vz,
                             float *phi, int64_t *pid, uint16_t *mask)
{
    MPI_Status status;
    int errcode, retval;

    int64_t scan_size = 0, num_particles, nwrite, record_size;
    
    record_size = (7 * sizeof(float)) + sizeof(int64_t) + sizeof(uint16_t);
    
	MPI_Offset ofst = m_headerSize; // file pointer
    off_t pos_offst = m_headerSize;
    
    num_particles =  m_localParticles;
    
	MPI_Exscan(&num_particles, &scan_size, 1,
               MPI_LONG_LONG, MPI_SUM, m_partitionComm);
    
	if (0== m_partitionRank)
		scan_size = 0;

    
    if (m_interface == USE_POSIX)
    {
        pos_offst += (scan_size * record_size);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)xx, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)yy, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)zz, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)vx, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)vy, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)vz, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(float);
        errcode = __POSIX_Write_Data ((const char*)phi, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(float);
        nwrite = num_particles * sizeof(int64_t);
        errcode = __POSIX_Write_Data ((const char*)pid, &nwrite, pos_offst);
        
        pos_offst += num_particles * sizeof(int64_t);
        nwrite = num_particles * sizeof(uint16_t);
        errcode = __POSIX_Write_Data ((const char*)mask, &nwrite, pos_offst);
        
    }
    else if (m_interface == USE_MPIIO)
    {
    
        ofst += scan_size * record_size;
    
        errcode = MPI_File_write_at (m_fileHandle, ofst, xx, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At XX");
        }
    
        ofst += num_particles * sizeof(float);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, yy, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At YY");
        }
        ofst += num_particles * sizeof(float);
    
        errcode = MPI_File_write_at (m_fileHandle, ofst, zz, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At ZZ");
        }
        ofst += num_particles * sizeof(float);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, vx, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At VX");
        }
        ofst += num_particles * sizeof(float);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, vy, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At VY");
        }
        ofst += num_particles * sizeof(float);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, vz, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At VZ");
        }
        ofst += num_particles * sizeof(float);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, phi, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At PHI");
        }
        ofst += num_particles * sizeof(float);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, pid, num_particles, MPI_LONG_LONG, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At PID");
        }
        ofst += num_particles * sizeof(int64_t);
	
        errcode = MPI_File_write_at (m_fileHandle, ofst, mask, num_particles, MPI_UNSIGNED_SHORT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Write_At Mask");
        }
    }
    else
    {
        cout <<" Unknown Interface" << endl;
    }
    return 0;
}



//----------------------------------------------------------------------------
//
// Reads checkpoint restart file
//
// num_particles: number of particles in this block
//
//  following arrays are allocated by this function, not the caller
// xx, yy, zz: particle positions
// vx, vy, vz: particle velocities
// phi: particle potentials
// pid: particle IDs
// mask: unused
//
// side effects: allocates above arrays
// returns: local number of particles
//
//----------------------------------------------------------------------------
int RestartIO_GLEAN::Read ( float *&xx, float *&yy, float *&zz,
                            float *&vx, float *&vy, float *&vz,
                            float *&phi, int64_t *&pid, uint16_t *&mask)
                                
{
    int errcode;
    MPI_Status status;
    int64_t scan_size = 0, num_particles, nread;
    num_particles =  m_localParticles;
    
	MPI_Exscan(&num_particles, &scan_size, 1, MPI_LONG_LONG, MPI_SUM, m_partitionComm);
    
    if (0== m_partitionRank)
        scan_size = 0;
    
    int64_t record_size = (7*sizeof(float)) + sizeof(int64_t) + sizeof(uint16_t);
    
    MPI_Offset ofst = m_headerSize;
    off_t pos_offst = m_headerSize;
    
    
    // allocate data arrrays and create data type
	xx = new float[num_particles];
	yy = new float[num_particles];
	zz = new float[num_particles];
	vx = new float[num_particles];
	vy = new float[num_particles];
	vz = new float[num_particles];
	phi = new float[num_particles];
	pid = new int64_t[num_particles];
	mask = new uint16_t[num_particles];
    
    
    if (m_interface == USE_MPIIO)
    {
        ofst += scan_size * record_size;
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, xx, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At XX");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, yy, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At YY");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, zz, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At ZZ");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, vx, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At VX");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, vy, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At VY");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, vz, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At VZ");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, phi, num_particles, MPI_FLOAT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At PHI");
        }
        ofst += num_particles * sizeof(float);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, pid, num_particles, MPI_LONG_LONG, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At PID");
        }
        ofst += num_particles * sizeof(int64_t);
        
        errcode = MPI_File_read_at (m_fileHandle, ofst, mask, num_particles, MPI_UNSIGNED_SHORT, &status);
        if (MPI_SUCCESS != errcode)
        {
            __HandleMPIIOError(errcode, (char *)"MPI_FILE_Read_At Mask");
        }

    }
    else if (m_interface == USE_POSIX)
    {
        pos_offst += (scan_size * record_size);
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)xx, nread, pos_offst);

        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)yy, nread, pos_offst);
        
        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)zz, nread, pos_offst);
        
        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)vx, nread, pos_offst);
        
        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)vy, nread, pos_offst);
        
        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)vz, nread, pos_offst);
        
        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(float);
        errcode = __POSIX_Read_Data ((unsigned char*)phi, nread, pos_offst);
       
        pos_offst += (num_particles * sizeof(float));
        nread = num_particles * sizeof(int64_t);
        errcode = __POSIX_Read_Data ((unsigned char*)pid, nread, pos_offst);
        
        pos_offst += (num_particles * sizeof(int64_t));
        nread = num_particles * sizeof(uint16_t);
        errcode = __POSIX_Read_Data ((unsigned char*)mask, nread, pos_offst);
        
    }
    else
    {
        cout << " Unknown Mode for Reading Data" << endl;
    }
    

    return 0;
}
                                               
                                               
/*

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------
int RestartIO_GLEAN :: __MPIIO_Write(float *xx, float *yy, float *zz,
                                    float *vx, float *vy, float *vz,
                                    float *phi, int64_t *pid, uint16_t *mask)
{
    return 0;
}

//  ---------------------------------------------------------------------------
//
//
//  ---------------------------------------------------------------------------

int RestartIO_GLEAN :: __Posix_Write(int64_t& ofst, int64_t &num_particles,
                                     float *xx, float *yy, float *zz,
                                     float *vx, float *vy, float *vz,
                                     float *phi, int64_t *pid, uint16_t *mask)
{
    return 0;
}
*/

