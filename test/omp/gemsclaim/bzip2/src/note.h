/* The level is 0 initially */
extern int verbosityLevel;

/* Messages with level > verbosityLevel are not displayed */
extern void note( int level, const char * format, ... );
