#ifndef _BITMAP_H_
#define _BITMAP_H_

#define INSIEME_TEST_BMP "/software-local/insieme/test/ocl/common/big_image.bmp"

#ifdef _WIN32
    #include <windows.h>
    #include <wingdi.h>
#endif /* WIN32 */

/*
 * Bitmap file data structures (these are defined in <wingdi.h> under Windows) 
 * Note that most Windows compilers will pack the following structures, so
 * when reading them under MacOS or UNIX we need to read individual fields
 * to avoid differences in alignment.
 */

#ifndef _WIN32
typedef struct                       /**** BMP file header structure ****/
    {
    unsigned short bfType;           // Magic number for file 
    unsigned int   bfSize;           // Size of file 
    unsigned short bfReserved1;      // Reserved 
    unsigned short bfReserved2;      // ... 
    unsigned int   bfOffBits;        // Offset to bitmap data 
    } BITMAPFILEHEADER;

#define BF_TYPE 0x4D42             // MB 

typedef struct                       /**** BMP file info structure ****/
    {
    unsigned int   biSize;           // Size of info header 
    int            biWidth;          // Width of image 
    int            biHeight;         // Height of image 
    unsigned short biPlanes;         // Number of color planes 
    unsigned short biBitCount;       // Number of bits per pixel 
    unsigned int   biCompression;    // Type of compression to use 
    unsigned int   biSizeImage;      // Size of image data 
    int            biXPelsPerMeter;  // X pixels per meter 
    int            biYPelsPerMeter;  // Y pixels per meter 
    unsigned int   biClrUsed;        // Number of colors used 
    unsigned int   biClrImportant;   // Number of important colors 
    } BITMAPINFOHEADER;

/*
 * Constants for the biCompression field...
 */

#define BI_RGB       0             /* No compression - straight BGR data */
#define BI_RLE8      1             /* 8-bit run-length compression */
#define BI_RLE4      2             /* 4-bit run-length compression */
#define BI_BITFIELDS 3             /* RGB bitmap with RGB masks */

typedef struct                       /**** Colormap entry structure ****/
    {
    unsigned char  rgbBlue;          /* Blue value */
    unsigned char  rgbGreen;         /* Green value */
    unsigned char  rgbRed;           /* Red value */
    unsigned char  rgbReserved;      /* Reserved */
    } RGBQUAD;

typedef struct                       /**** Bitmap information structure ****/
    {
    BITMAPINFOHEADER bmiHeader;      /* Image header */
    RGBQUAD          bmiColors[256]; /* Image colormap */
    } BITMAPINFO;
#endif /* !WIN32 */


/* Prototypes */

typedef unsigned char ubyte;
typedef unsigned char GLubyte;

typedef struct {
	unsigned char x;    
	unsigned char y;    
	unsigned char z;    
	unsigned char w;
} uchar4;

/** Load a BMP image, no bit limitation */
ubyte  *icl_loadbmp(const char *filename, BITMAPINFO **info);

/** Load a 32-bit image. If the image is 24bit, it add extra padding. */
uchar4 *icl_loadbmp_uchar4(const char *filename, BITMAPINFO **info);

/** Load a tile of a 32-bit image (with padding). */
uchar4 *icl_loadbmp_tile_uchar4(const char *filename, BITMAPINFO **info, unsigned tile_x, unsigned tile_y);

/** Load a tile having about of pixelNum pixels. Returns the tile size. */
uchar4 *icl_loadbmp_pixel_uchar4(const char *filename, BITMAPINFO **info, unsigned pixelNum, unsigned *tile_x, unsigned *tile_y);

/** Save on a file a BMP image. */
int     icl_savebmp(const char *filename, BITMAPINFO *info, ubyte *bits);
#endif
