/*
* Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.
*
* Please refer to the NVIDIA end user license agreement (EULA) associated
* with this source code for terms and conditions that govern your use of
* this software. Any use, reproduction, disclosure, or distribution of
* this software and related documentation outside the terms of the EULA
* is strictly prohibited.
*
*/

#ifndef SHR_QATEST_H
#define SHR_QATEST_H

// *********************************************************************
// Generic utilities for NVIDIA GPU Computing SDK 
// *********************************************************************

// reminders for output window and build log
#ifdef _WIN32
    #pragma message ("Note: including windows.h")
    #pragma message ("Note: including math.h")
    #pragma message ("Note: including assert.h")
#endif

// OS dependent includes
#ifdef _WIN32
    // Headers needed for Windows
    #include <windows.h>
#else
    // Headers needed for Linux
    #include <sys/stat.h>
    #include <sys/types.h>
    #include <sys/time.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <stdarg.h>
#endif

#ifndef STRCASECMP
#ifdef _WIN32
#define STRCASECMP _stricmp
#else
#define STRCASECMP strcasecmp
#endif
#endif

#ifndef STRNCASECMP
#ifdef _WIN32
#define STRNCASECMP _strnicmp
#else
#define STRNCASECMP strncasecmp
#endif
#endif


// Standardized QA Start/Finish for CUDA SDK tests
#define shrQAStart(a, b)      __shrQAStart(a, b)
#define shrQAFinish(a, b, c)  __shrQAFinish(a, b, c)
#define shrQAFinish2(a, b, c, d) __shrQAFinish2(a, b, c, d)

inline int findExeNameStart(const char *exec_name)
{
    int exename_start = (int)strlen(exec_name);

    while( (exename_start > 0) && 
            (exec_name[exename_start] != '\\') && 
            (exec_name[exename_start] != '/') )
    {
		exename_start--;
    }
    if (exec_name[exename_start] == '\\' || 
        exec_name[exename_start] == '/')
    {
        return exename_start+1;
    } else {
        return exename_start;
    }
}

inline int __shrQAStart(int argc, char **argv)
{
    bool bQATest = false;
    for (int i=1; i < argc; i++) {
        int string_start = 0;
        while (argv[i][string_start] == '-')
           string_start++;
        char *string_argv = &argv[i][string_start];

        if (!STRCASECMP(string_argv, "qatest")) {
           bQATest = true;
        }
    }
    
    // We don't want to print the entire path, so we search for the first 
	int exename_start = findExeNameStart(argv[0]);
    if (bQATest) {
        printf("&&&& RUNNING %s", &(argv[0][exename_start]));
        for (int i=1; i < argc; i++) printf(" %s", argv[i]);
        printf("\n");
    } else {
        printf("[%s] starting...\n", &(argv[0][exename_start]));
    }
    return exename_start;
}

enum eQAstatus {
    QA_FAILED = 0,
    QA_PASSED = 1,
    QA_WAIVED = 2
};


inline void __shrQAFinish(int argc, const char **argv, int iStatus)
{
    bool bQATest = false, bNoPrompt = false;
    const char *sStatus[] = { "FAILED", "PASSED", "WAIVED", NULL };
	
    for (int i=1; i < argc; i++) {
        int string_start = 0;
            while (argv[i][string_start] == '-')
                string_start++;
            const char *string_argv = &argv[i][string_start];

	    if (!STRCASECMP(string_argv, "qatest")) {
           bQATest = true;
#ifndef WIN32
           bNoPrompt = true;
#endif
        }	
#ifdef WIN32
        // we will not prompt only if -noprompt is specified, so in the SDK browser it will show a window
        // after it is run
        if (!STRCASECMP(string_argv, "noprompt")) {
           bNoPrompt = true;
        }
#else // For Linux/Mac we want have prompting disabled
        if (!STRCASECMP(string_argv, "prompt")) {
           bNoPrompt = false;
        } else {
           bNoPrompt = true;
        }
#endif
    }

    int exename_start = findExeNameStart(argv[0]);
    if (bQATest) {
        printf("&&&& %s %s", sStatus[iStatus], &(argv[0][exename_start]));
        for (int i=1; i < argc; i++) printf(" %s", argv[i]);
        printf("\n");
    } else {
        printf("[%s] test results...\n%s\n", &(argv[0][exename_start]), sStatus[iStatus]);
    }
    if (!bNoPrompt) {
        printf("\nPress ENTER to exit...\n");
        fflush( stdout);
        fflush( stderr);
        getchar();
    }
}

inline void __shrQAFinish2(bool bQATest, int argc, const char **argv, int iStatus)
{
    const char *sStatus[] = { "FAILED", "PASSED", "WAIVED", NULL };
	
    for (int i=1; i < argc; i++) {
        int string_start = 0;
            while (argv[i][string_start] == '-')
                string_start++;
            const char *string_argv = &argv[i][string_start];

	    if (!STRCASECMP(string_argv, "qatest")) {
            bQATest = true;
        }	
    }

    int exename_start = findExeNameStart(argv[0]);
    if (bQATest) {
        printf("&&&& %s %s", sStatus[iStatus], &(argv[0][exename_start]));
        for (int i=1; i < argc; i++) printf(" %s", argv[i]);
        printf("\n");
    } else {
        printf("[%s] test results...\n%s\n", &(argv[0][exename_start]), sStatus[iStatus]);
    }
}

inline void shrQAFinishExit(int argc, const char **argv, int iStatus)
{
    __shrQAFinish(argc, argv, iStatus);

    exit(iStatus ? EXIT_SUCCESS : EXIT_FAILURE); 
}

inline void shrQAFinishExit2(bool bQAtest, int argc, const char **argv, int iStatus)
{
    __shrQAFinish2(bQAtest, argc, argv, iStatus);

    exit(iStatus ? EXIT_SUCCESS : EXIT_FAILURE);
}

#endif
