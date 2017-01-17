#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "note.h"

int verbosityLevel = 0;

static char programName[] = "bzip2smp: ";

void note( int level, const char * format, ... )
{
  if ( verbosityLevel >= level )
  {
    char * str;
    va_list args;

    str = malloc( strlen( format ) + strlen( programName ) + 1 );

    if ( str == NULL )
    {
      fprintf( stderr, "%sfailed to allocate memory\n", programName );
      exit( 1 );
    }

    strcpy( str, programName );
    strcat( str, format );

    va_start( args, format );

    vfprintf( stderr, str, args );

    va_end( args );

    free( str );
  }
}
