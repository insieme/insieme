#include "lib_icl_bmp.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>

#ifdef _WIN32

/*
 * Load a BMP file from disk.
 * Returns a pointer to the bitmap if successful, NULL otherwise.
 */
ubyte *icl_loadbmp(const char *filename, BITMAPINFO **info) 
{
    FILE             *fp;          // Open file pointer 
    ubyte            *bits;        // Bitmap pixel bits 
    size_t/*int*/              bitsize;      // Size of bitmap 
    size_t/*int*/              infosize;     // Size of header information 
    BITMAPFILEHEADER header;       // File header 

    // Try opening the file; use "rb" mode to read this *binary* file.
    if ((fp = fopen(filename, "rb")) == NULL)
        return (NULL);

    // Read the file header and any following bitmap information
    if (fread(&header, sizeof(BITMAPFILEHEADER), 1, fp) < 1)
        {
        // Couldn't read the file header - return NULL
	fclose(fp);
        return (NULL);
        }
		
    if (header.bfType != 'MB')	// Check for BM reversed
        {
        // Not a bitmap file - return NULL
        fclose(fp);
        return (NULL);
        }

    infosize = header.bfOffBits - sizeof(BITMAPFILEHEADER);
    if ((*info = (BITMAPINFO *)malloc(infosize)) == NULL)
        {
        // Couldn't allocate memory for bitmap info - return NULL
        fclose(fp);
        return (NULL);
        }

    if (fread(*info, 1, infosize, fp) < infosize)
        {
        // Couldn't read the bitmap header - return NULL
        free(*info);
		fclose(fp);
        return (NULL);
	}
		
    // read pixel info
    if ((bitsize = (*info)->bmiHeader.biSizeImage) == 0)
        bitsize = ((*info)->bmiHeader.biWidth *
                   (*info)->bmiHeader.biBitCount + 7) / 8 *
  	           abs((*info)->bmiHeader.biHeight);

    if ((bits = (ubyte*)malloc(bitsize)) == NULL)
        {
        /* Couldn't allocate memory - return NULL! */
        free(*info);
        fclose(fp);
        return (NULL);
        }

    if (fread(bits, 1, bitsize, fp) < bitsize)
        {
        /* Couldn't read bitmap - free memory and return NULL! */
        free(*info);
        free(bits);
        fclose(fp);
        return (NULL);
        }

    // OK, everything went fine - return the allocated bitmap. 
    fclose(fp);    
	return (bits);
}


/*
 * Save a BMP file to disk.
 * Returns 0 on success or -1 on failure.
 */

int                                
icl_savebmp(const char *filename, BITMAPINFO *info, ubyte *bits)     
{
    FILE             *fp;          /* Open file pointer */
    size_t/*int*/         size,         /* Size of file */
                     infosize,     /* Size of bitmap info */
                     bitsize;      /* Size of bitmap pixels */
    BITMAPFILEHEADER header;       /* File header */


    /* Try opening the file; use "wb" mode to write this *binary* file. */
    if ((fp = fopen(filename, "wb")) == NULL){
        return (-1);
	}

    /* Figure out the bitmap size */
    if (info->bmiHeader.biSizeImage == 0)
	bitsize = (info->bmiHeader.biWidth *
        	   info->bmiHeader.biBitCount + 7) / 8 *
		  abs(info->bmiHeader.biHeight);
    else
	bitsize = info->bmiHeader.biSizeImage;

    /* Figure out the header size */
    infosize = sizeof(BITMAPINFOHEADER);
    switch (info->bmiHeader.biCompression)
	{
	case BI_BITFIELDS :
            infosize += 12; /* Add 3 RGB double word masks */
            if (info->bmiHeader.biClrUsed == 0)
	      break;
	case BI_RGB :
            if (info->bmiHeader.biBitCount > 8 &&
        	info->bmiHeader.biClrUsed == 0)
	      break;
	case BI_RLE8 :
	case BI_RLE4 :
            if (info->bmiHeader.biClrUsed == 0)
              infosize += (1 << info->bmiHeader.biBitCount) * 4;
	    else
              infosize += info->bmiHeader.biClrUsed * 4;
	    break;
	}

    size = sizeof(BITMAPFILEHEADER) + infosize + bitsize;

    /* Write the file header, bitmap information, and bitmap pixel data... */
    header.bfType      = 'MB'; /* Non-portable... sigh */
    header.bfSize      = size;
    header.bfReserved1 = 0;
    header.bfReserved2 = 0;
    header.bfOffBits   = sizeof(BITMAPFILEHEADER) + infosize;

    if (fwrite(&header, 1, sizeof(BITMAPFILEHEADER), fp) < sizeof(BITMAPFILEHEADER))
        {
        /* Couldn't write the file header - return... */
        fclose(fp);
        return (-1);
        }

    if (fwrite(info, 1, infosize, fp) < infosize)
        {
        /* Couldn't write the bitmap header - return... */
        fclose(fp);
        return (-1);
        }

    if (fwrite(bits, 1, bitsize, fp) < bitsize)
        {
        /* Couldn't write the bitmap - return... */
        fclose(fp);
        return (-1);
        }

    /* OK, everything went fine - return... */
    fclose(fp);

	printf("%s successfully saved\n", filename);
    return (0);
}



#else /* !WIN32 */
/*
 * Functions for reading and writing 16- and 32-bit little-endian integers.
 */

static unsigned short read_word(FILE *fp);
static unsigned int   read_dword(FILE *fp);
static int            read_long(FILE *fp);

static int            write_word(FILE *fp, unsigned short w);
static int            write_dword(FILE *fp, unsigned int dw);
static int            write_long(FILE *fp, int l);


/*
 * 'LoadDIBitmap()' - Load a DIB/BMP file from disk.
 *
 * Returns a pointer to the bitmap if successful, NULL otherwise...
 */

GLubyte *                          /* O - Bitmap data */
icl_loadbmp(const char *filename, /* I - File to load */
             BITMAPINFO **info)    /* O - Bitmap information */
    {
    FILE             *fp;          /* Open file pointer */
    GLubyte          *bits;        /* Bitmap pixel bits */
    GLubyte          *ptr;         /* Pointer into bitmap */
    GLubyte          temp;         /* Temporary variable to swap red and blue */
    int              x, y;         /* X and Y position in image */
    int              length;       /* Line length */
    int              bitsize;      /* Size of bitmap */
    int              infosize;     /* Size of header information */
    BITMAPFILEHEADER header;       /* File header */


    /* Try opening the file; use "rb" mode to read this *binary* file. */
    if ((fp = fopen(filename, "rb")) == NULL)
        return (NULL);

    /* Read the file header and any following bitmap information... */
    header.bfType      = read_word(fp);
    header.bfSize      = read_dword(fp);
    header.bfReserved1 = read_word(fp);
    header.bfReserved2 = read_word(fp);
    header.bfOffBits   = read_dword(fp);

    if (header.bfType != BF_TYPE) /* Check for BM reversed... */
        {
        /* Not a bitmap file - return NULL... */
        fclose(fp);
        return (NULL);
        }

    infosize = header.bfOffBits - 18;
    if ((*info = (BITMAPINFO *)malloc(sizeof(BITMAPINFO))) == NULL)
        {
        /* Couldn't allocate memory for bitmap info - return NULL... */
        fclose(fp);
        return (NULL);
        }

    (*info)->bmiHeader.biSize          = read_dword(fp);
    (*info)->bmiHeader.biWidth         = read_long(fp);
    (*info)->bmiHeader.biHeight        = read_long(fp);
    (*info)->bmiHeader.biPlanes        = read_word(fp);
    (*info)->bmiHeader.biBitCount      = read_word(fp);
    (*info)->bmiHeader.biCompression   = read_dword(fp);
    (*info)->bmiHeader.biSizeImage     = read_dword(fp);
    (*info)->bmiHeader.biXPelsPerMeter = read_long(fp);
    (*info)->bmiHeader.biYPelsPerMeter = read_long(fp);
    (*info)->bmiHeader.biClrUsed       = read_dword(fp);
    (*info)->bmiHeader.biClrImportant  = read_dword(fp);

    if (infosize > 40)
	if (fread((*info)->bmiColors, infosize - 40, 1, fp) < 1)
            {
            /* Couldn't read the bitmap header - return NULL... */
            free(*info);
            fclose(fp);
            return (NULL);
            }

    /* Now that we have all the header info read in, allocate memory for *
     * the bitmap and read *it* in...                                    */
    if ((bitsize = (*info)->bmiHeader.biSizeImage) == 0)
        bitsize = ((*info)->bmiHeader.biWidth *
                   (*info)->bmiHeader.biBitCount + 7) / 8 *
  	           abs((*info)->bmiHeader.biHeight);

    if ((bits = (GLubyte*)malloc(bitsize)) == NULL)
        {
        /* Couldn't allocate memory - return NULL! */
        free(*info);
        fclose(fp);
        return (NULL);
        }

    if (fread(bits, 1, bitsize, fp) < bitsize)
        {
        /* Couldn't read bitmap - free memory and return NULL! */
        free(*info);
        free(bits);
        fclose(fp);
        return (NULL);
        }

    /* Swap red and blue */
    length = ((*info)->bmiHeader.biWidth * 3 + 3) & ~3;
    for (y = 0; y < (*info)->bmiHeader.biHeight; y ++)
        for (ptr = bits + y * length, x = (*info)->bmiHeader.biWidth;
             x > 0;
	     x --, ptr += 3)
	    {
	    temp   = ptr[0];
	    ptr[0] = ptr[2];
	    ptr[2] = temp;
	    }

    /* OK, everything went fine - return the allocated bitmap... */
    fclose(fp);
    return (bits);
    }


/*
 * 'SaveDIBitmap()' - Save a DIB/BMP file to disk.
 *
 * Returns 0 on success or -1 on failure...
 */

int                                /* O - 0 = success, -1 = failure */
icl_savebmp(const char *filename, /* I - File to load */
             BITMAPINFO *info,     /* I - Bitmap information */
			 ubyte    *bits)     /* I - Bitmap data */
    {
    FILE *fp;                      /* Open file pointer */
    int  size,                     /* Size of file */
         infosize,                 /* Size of bitmap info */
         bitsize;                  /* Size of bitmap pixels */


    /* Try opening the file; use "wb" mode to write this *binary* file. */
    if ((fp = fopen(filename, "wb")) == NULL)
        return (-1);

    /* Figure out the bitmap size */
    if (info->bmiHeader.biSizeImage == 0)
	bitsize = (info->bmiHeader.biWidth *
        	   info->bmiHeader.biBitCount + 7) / 8 *
		  abs(info->bmiHeader.biHeight);
    else
	bitsize = info->bmiHeader.biSizeImage;

    /* Figure out the header size */
    infosize = sizeof(BITMAPINFOHEADER);
    switch (info->bmiHeader.biCompression)
	{
	case BI_BITFIELDS :
            infosize += 12; /* Add 3 RGB doubleword masks */
            if (info->bmiHeader.biClrUsed == 0)
	      break;
	case BI_RGB :
            if (info->bmiHeader.biBitCount > 8 &&
        	info->bmiHeader.biClrUsed == 0)
	      break;
	case BI_RLE8 :
	case BI_RLE4 :
            if (info->bmiHeader.biClrUsed == 0)
              infosize += (1 << info->bmiHeader.biBitCount) * 4;
	    else
              infosize += info->bmiHeader.biClrUsed * 4;
	    break;
	}

    size = sizeof(BITMAPFILEHEADER) + infosize + bitsize;

    /* Write the file header, bitmap information, and bitmap pixel data... */
    write_word(fp, BF_TYPE);        /* bfType */
    write_dword(fp, size);          /* bfSize */
    write_word(fp, 0);              /* bfReserved1 */
    write_word(fp, 0);              /* bfReserved2 */
    write_dword(fp, 18 + infosize); /* bfOffBits */

    write_dword(fp, info->bmiHeader.biSize);
    write_long(fp, info->bmiHeader.biWidth);
    write_long(fp, info->bmiHeader.biHeight);
    write_word(fp, info->bmiHeader.biPlanes);
    write_word(fp, info->bmiHeader.biBitCount);
    write_dword(fp, info->bmiHeader.biCompression);
    write_dword(fp, info->bmiHeader.biSizeImage);
    write_long(fp, info->bmiHeader.biXPelsPerMeter);
    write_long(fp, info->bmiHeader.biYPelsPerMeter);
    write_dword(fp, info->bmiHeader.biClrUsed);
    write_dword(fp, info->bmiHeader.biClrImportant);

    if (infosize > 40)
	if (fwrite(info->bmiColors, infosize - 40, 1, fp) < 1)
            {
            /* Couldn't write the bitmap header - return... */
            fclose(fp);
            return (-1);
            }

    if (fwrite(bits, 1, bitsize, fp) < bitsize)
        {
        /* Couldn't write the bitmap - return... */
        fclose(fp);
        return (-1);
        }

    /* OK, everything went fine - return... */
    fclose(fp);
    return (0);
    }


/*
 * 'read_word()' - Read a 16-bit unsigned integer.
 */

static unsigned short     /* O - 16-bit unsigned integer */
read_word(FILE *fp)       /* I - File to read from */
    {
    unsigned char b0, b1; /* Bytes from file */

    b0 = getc(fp);
    b1 = getc(fp);

    return ((b1 << 8) | b0);
    }


/*
 * 'read_dword()' - Read a 32-bit unsigned integer.
 */

static unsigned int               /* O - 32-bit unsigned integer */
read_dword(FILE *fp)              /* I - File to read from */
    {
    unsigned char b0, b1, b2, b3; /* Bytes from file */

    b0 = getc(fp);
    b1 = getc(fp);
    b2 = getc(fp);
    b3 = getc(fp);

    return ((((((b3 << 8) | b2) << 8) | b1) << 8) | b0);
    }


/*
 * 'read_long()' - Read a 32-bit signed integer.
 */

static int                        /* O - 32-bit signed integer */
read_long(FILE *fp)               /* I - File to read from */
    {
    unsigned char b0, b1, b2, b3; /* Bytes from file */

    b0 = getc(fp);
    b1 = getc(fp);
    b2 = getc(fp);
    b3 = getc(fp);

    return ((int)(((((b3 << 8) | b2) << 8) | b1) << 8) | b0);
    }


/*
 * 'write_word()' - Write a 16-bit unsigned integer.
 */

static int                     /* O - 0 on success, -1 on error */
write_word(FILE           *fp, /* I - File to write to */
           unsigned short w)   /* I - Integer to write */
    {
    putc(w, fp);
    return (putc(w >> 8, fp));
    }


/*
 * 'write_dword()' - Write a 32-bit unsigned integer.
 */

static int                    /* O - 0 on success, -1 on error */
write_dword(FILE         *fp, /* I - File to write to */
            unsigned int dw)  /* I - Integer to write */
    {
    putc(dw, fp);
    putc(dw >> 8, fp);
    putc(dw >> 16, fp);
    return (putc(dw >> 24, fp));
    }


/*
 * 'write_long()' - Write a 32-bit signed integer.
 */

static int           /* O - 0 on success, -1 on error */
write_long(FILE *fp, /* I - File to write to */
           int  l)   /* I - Integer to write */
    {
    putc(l, fp);
    putc(l >> 8, fp);
    putc(l >> 16, fp);
    return (putc(l >> 24, fp));
    }
#endif /* WIN32 */

/* Load a tile of the image. */
uchar4 *icl_loadbmp_tile_uchar4(const char *filename, BITMAPINFO **info, unsigned tile_x, unsigned tile_y){
	uchar4 *original = icl_loadbmp_uchar4(filename, info);
	uchar4 *out =  NULL;

	unsigned height = (*info)->bmiHeader.biHeight;
	unsigned width = (*info)->bmiHeader.biWidth;

	if(tile_x > width){
		fprintf(stderr,"Error in loadbmp_tile: tilesizex is bigger than width\n");
		return NULL;
	}

	if(tile_y > height){
		fprintf(stderr,"Error in loadbmp_tile: tilesizey is bigger than height\n");
		return NULL;
	}

	out = (uchar4*)malloc(tile_x*tile_y*sizeof(uchar4));

	// copy only the are of the image inside the tile
	for(unsigned i=0; i< tile_x; ++i)
		for(unsigned j=0; j<tile_y; ++j)
		{
			unsigned int orIndex  = j * width + i; // width
			unsigned int outIndex = j * tile_x + i;
			out[outIndex] = original[orIndex];
		}

		free(original);

		// fixing header with new sizes
		(*info)->bmiHeader.biHeight = tile_y;
		(*info)->bmiHeader.biWidth  = tile_x;
		(*info)->bmiHeader.biSizeImage = tile_x * tile_y * 4;
		return out;
}


/* 	Load a BMP with 24 bit. */
uchar4 *icl_loadbmp_uchar4(const char *filename, BITMAPINFO **info)
{
	ubyte *raw = icl_loadbmp(filename, info);
	uchar4 *out =  NULL;

	// here is required a check if the image is still uchar4
	if( (*info)->bmiHeader.biBitCount == 32) return (uchar4*)raw;

	unsigned height = (*info)->bmiHeader.biHeight;
	unsigned width = (*info)->bmiHeader.biWidth;
	out = (uchar4*)malloc(height*width*sizeof(uchar4));

	// this code
	for(unsigned i=0; i< height; ++i)
		for(unsigned j=0; j<width; ++j)
		{
			unsigned int outIndex = i * width + j;
			unsigned int rawIndex = outIndex *3; 
			// gray = (0.3*R + 0.59*G + 0.11*B)
			out[outIndex].x = raw[rawIndex];
			out[outIndex].y = raw[rawIndex + 1];
			out[outIndex].z = raw[rawIndex + 2];
			out[outIndex].w = 0;
		}
		free(raw);

		// Note(Biagio): because of the different data packing, we should change some header fields	
		(*info)->bmiHeader.biBitCount = 32;
		(*info)->bmiHeader.biSizeImage = width * height * 4;

		return out;
}

uchar4 *icl_loadbmp_pixel_uchar4(const char *filename, BITMAPINFO **info, unsigned pixelNum, unsigned *tile_x, unsigned *tile_y)
{
	unsigned tilesize = (unsigned)sqrt((double)pixelNum);	
	unsigned mulTile = 32; 
	if(tilesize < 128){
		mulTile = 32;
	}
	else if(tilesize < 512){
		mulTile = 128;
	}
	else // if(tilesize >= 512) 
	{
		mulTile = 512;
	}
	
	tilesize = (tilesize / mulTile ) * mulTile ; // this rounds the size to a multiple of <mulTile>

	*tile_y = tilesize; //min(tilesize, height);
	*tile_x = pixelNum / tilesize;	
	return icl_loadbmp_tile_uchar4(filename, info, *tile_x, *tile_y);
}

