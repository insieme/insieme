
/* mbsinit example */
#include <wchar.h>
#include <string.h>
#include <stdio.h>

int main() {

	char buffer[80];
	mbstate_t mbst;
	wchar_t wcs []  = L"this is a long string";
	const wchar_t * p;

	p = wcs;

	if ( !mbsinit(&mbst) )
	memset (&mbst,0,sizeof(mbst));  /* set to initial state */

	wcsrtombs ( buffer, &p, 80, &mbst);

	return 0;
}
