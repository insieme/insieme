#ifndef __GLEAN__UTIL__PRINTMESSAGE__H
#define __GLEAN__UTIL__PRINTMESSAGE__H

#include <cstdio> /* printf */
#include <unistd.h> /* gethostname */
#include <cerrno> /* perror */

/* MACROS FOR PRINTING ALERTS, ERRORS, INFO and DEBUGGING MESSAGES */
	
#ifdef GLEAN_PRINT_ALERT
#undef GLEAN_PRINT_ALERT
#define GLEAN_PRINT_ALERT(format, args ...)  do {  \
				printf("--- ALERT: ");printf(format,args);} while(0)
#else
#define GLEAN_PRINT_ALERT(format, args ...) ((void)0)
#endif

#ifdef GLEAN_PRINT_ERROR
#undef GLEAN_PRINT_ERROR
#define GLEAN_PRINT_ERROR(format, args ...) do { \
				printf("@@@@@ ERROR @@@@@: ");   \
				printf(format,args);} while(0)
#else
#define GLEAN_PRINT_ERROR(format, args ...) ((void)0)
#endif

#ifdef GLEAN_PRINT_PERROR
#undef GLEAN_PRINT_PERROR
#define GLEAN_PRINT_PERROR(format, args ...) do { \
				printf("@ PERROR @: "); perror(" perror: " );  \
				printf(format,args);} while(0)
#else
#define GLEAN_PRINT_PERROR(format, args ...) ((void)0)
#endif

#ifdef GLEAN_PRINT_INFO
#undef GLEAN_PRINT_INFO
#define GLEAN_PRINT_INFO(format, args ...) do { \
					printf("??? INFO : ");printf(format, args);} while(0)
#else
#define GLEAN_PRINT_INFO(format, args ...) ((void)0)
#endif

#ifdef GLEAN_PRINT_DEBUG1
#undef GLEAN_PRINT_DEBUG1
#define GLEAN_PRINT_DEBUG1(format, args ...) do { \
				printf("D1: ");printf(format, args);} while (0)
#else
#define GLEAN_PRINT_DEBUG1(format, args ...) ((void)0)
#endif

#ifdef GLEAN_PRINT_DEBUG2
#undef GLEAN_PRINT_DEBUG2
#ifdef GLEAN_PRINT_DEBUG1	
#undef GLEAN_PRINT_DEBUG1
#define GLEAN_PRINT_DEBUG1(format, args ...) do { \
				printf("D1: ");printf(format, args);} while (0)
#endif	
#define GLEAN_PRINT_DEBUG2(format, args ...) do { \
				printf("--D2: ");printf(format, args); } while(0)
#else
#define GLEAN_PRINT_DEBUG2(format, args ...) ((void)0)
#endif

#ifdef GLEAN_PRINT_DEBUG3
#undef GLEAN_PRINT_DEBUG3
#ifdef GLEAN_PRINT_DEBUG1
#undef GLEAN_PRINT_DEBUG1
#define GLEAN_PRINT_DEBUG1(format, args ...) do { \
				printf("D1: ");printf(format, args);} while (0)
#endif
#ifdef GLEAN_PRINT_DEBUG2
#undef GLEAN_PRINT_DEBUG2
#define GLEAN_PRINT_DEBUG2(format, args ...) do {  \
				printf("--D2: ");printf(format, args); } while(0)
#endif
#define GLEAN_PRINT_DEBUG3(format, args ...) do {  \
				printf("----D3: ");printf(format, args); }  while(0)
#else
#define GLEAN_PRINT_DEBUG3(format, args ...) ((void)0)
#endif


#endif
