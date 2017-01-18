#include <stdio.h>
#include <string.h>
#include "note.h"
#include "detectht.h"

#ifdef __linux__

static void noHt( void )
{
  fprintf(stderr, "No hyperthreading detected.\n" );
}

static char cpuInfoFile[128] = "/proc/cpuinfo";
static char siblingsString[128] = "siblings";
static char cpuCoresString[128] = "cpu cores";

int isHtPresent( void )
{
  char buf[ 4096 ];
  char * str;
  FILE * f;

  f = fopen( cpuInfoFile, "r" );

  if ( f == NULL )
  {
    fprintf(stderr, "Failed to open %s, assuming there is no hyperthreading\n",
             cpuInfoFile );

    return 0;
  }

  buf[ fread( buf, 1, sizeof( buf ) - 1, f ) ] = 0;

  fclose( f );

  if ( ( str = strstr( buf, siblingsString ) ) )
  {
    int siblings;
    int cores;

    if ( sscanf( str + strlen( siblingsString ) + 3, "%d", & siblings ) != 1 )
    {
      fprintf(stderr, "Failed to read the number of siblings in %s, assuming no ht\n",
               cpuInfoFile );
      return 0;
    }

    str = strstr( buf, cpuCoresString );

    if ( str == NULL )
    {
      fprintf(stderr, "No cpu cores string found in %s, but it should be there, assuming no ht\n",
               cpuInfoFile );

      return 0;
    }

    if ( sscanf( str + strlen( cpuCoresString ) + 3, "%d", & cores ) != 1 )
    {
      fprintf(stderr, "Failed to read the number of cores in %s, assuming no ht\n",
               cpuInfoFile );
      return 0;
    }

    switch( siblings / cores )
    {
      case 1:
        noHt();
        return 0;

      case 2:
        fprintf(stderr, "Hyperthreading detected.\n" );
        return 1;

      default:
        fprintf(stderr, "Strange number of siblings (%d) in %s, but assuming ht is present\n",
                 siblings / cores, cpuInfoFile );

        return 1;
    }
  }
  else
  {
    noHt();
    return 0;
  }
}
#else
int isHtPresent( void )
{
  fprintf(stderr, "Assuming that hyperthreading is not present.\n" );
  return 0;
}
#endif
