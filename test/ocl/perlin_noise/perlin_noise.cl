__constant int perm[512] = {
  151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233,
  7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23,
  190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219,
  203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174,
  20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48,
  27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133,
  230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65,
  25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18,
  169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198,
  173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147,
  118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17,
  182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44,
  154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39,
  253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
  218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191,
  179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214,
  31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127,
  4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243,
  141, 128, 195, 78, 66, 215, 61, 156, 180, 151, 160, 137, 91, 90,
  15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30,
  69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234,
  75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177,
  33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175,
  74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111,
  229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40,
  244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132,
  187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164,
  100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5,
  202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47,
  16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152,
  2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22,
  39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
  218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179,
  162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181,
  199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
  138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78,
  66, 215, 61, 156, 180
};

/**
 * p
 * 
 * Lookup values in perm table.
 */
int p(int i)
{
  return perm[i];  
}

/**
 * fade
 * 
 * Compute fade values.
 */
float fade(float t)
{
  return t * t * t * (t * (t * 6.0f - 15.0f) + 10.0f);
}

/**
 * grad
 * 
 * Compute gradient values.
 
static inline float grad(int hash, float x, float y, float z)
{
  int h = hash & 15;           // Convert low 4 bits of hash code
  float u = bitselect(y, x, as_float4((h < 8)));       // into 12 gradient directions.
  float xz = bitselect(z, x, as_float4((h == 12) | (h == 14)));
  float v = bitselect(xz, y, as_float4(h < 4));

  u = bitselect(-u, u, as_float4((h & 1) == 0));
  v = bitselect(-v, v, as_float4((h & 2) == 0));
  return u + v;
}
*/
/// Note(Biagio)
/// if bitselect is not implemented, try this
float grad(int hash, float x, float y, float z)
{
  int h = hash & 15;            // Convert low 4 bits of hash code
  float u = (h < 8) ? x : y;    // into 12 gradient directions.
  float v = (h < 4) ? y : (h == 12 || h == 14) ? x : z;

  u = (h & 1) == 0 ? u : -u;
  v = (h & 2) == 0 ? v : -v;
  return u + v;
}



/**
 * noise3
 * 
 * Compute noise values for points.
 */
float noise3(float x, float y, float z)
{
  float floor_x = floor(x);
  float floor_y = floor(y);
  float floor_z = floor(z);

  int X = (int)(floor_x) & 255; // Find unit cube that
  int Y = (int)(floor_y) & 255; // contains point.
  int Z = (int)(floor_z) & 255;

  x -= floor_x;                 // Find relative x,y,z
  y -= floor_y;                 // of point in cube.
  z -= floor_z;

  float x1 = x - 1.0f;
  float y1 = y - 1.0f;
  float z1 = z - 1.0f;

  float u = fade(x);           // Compute fade curves
  float v = fade(y);           // for each of x,y,z.
  float w = fade(z);

  int A = p(X) + Y;
  int AA = p(A) + Z;
  int AB = p(A + 1) + Z;       // Hash coordinates of
  int B = p(X + 1) + Y;        // the 8 cube corners.
  int BA = p(B) + Z;
  int BB = p(B + 1) + Z;

  float g0 = grad(p(AA), x, y, z);
  float g1 = grad(p(BA), x1, y, z);
  float g2 = grad(p(AB), x, y1, z);
  float g3 = grad(p(BB), x1, y1, z);

  float g4 = grad(p(AA + 1), x, y, z1);
  float g5 = grad(p(BA + 1), x1, y, z1);
  float g6 = grad(p(AB + 1), x, y1, z1);
  float g7 = grad(p(BB + 1), x1, y1, z1);

  // Add blended results from 8 corners of cube.
  float u01 = mix(g0, g1, u);
  float u23 = mix(g2, g3, u);
  float u45 = mix(g4, g5, u);
  float u67 = mix(g6, g7, u);

  float v0 = mix(u01, u23, v);
  float v1 = mix(u45, u67, v);

  return mix(v0, v1, w);
}

/**
 * transpose
 * 
 * Transpose matrix.
 */
static inline void transpose(float4 m[4])
{
  // Read Matrix into a float16 vector
  //float16 x = (float16) (m[0], m[1], m[2], m[3]);
  float16 x;
  float16 t;

  x.lo.lo = m[0];
  x.lo.hi = m[1];
  x.hi.lo = m[2];
  x.hi.hi = m[3];

  // Transpose
  t.even = x.lo;
  t.odd = x.hi;
  x.even = t.lo;
  x.odd = t.hi;

  // write back
  m[0] = x.lo.lo;               // { m[0][0], m[1][0], m[2][0], m[3][0] }
  m[1] = x.lo.hi;               // { m[0][1], m[1][1], m[2][1], m[3][1] }
  m[2] = x.hi.lo;               // { m[0][2], m[1][2], m[2][2], m[3][2] }
  m[3] = x.hi.hi;               // { m[0][3], m[1][3], m[2][3], m[3][3] }
}

typedef struct colors {
  float r;
  float g;
  float b;
} pixel;

/**
 * compute_perlin_noise
 * 
 * Compute perlin noise algorithm given time offset and rowstride. Set the required work group
 * size to the value provided on the command line (defaults to 1). 
 */
__kernel 
void compute_perlin_noise(__global uchar4 * output, const float time, const unsigned int rowstride)
{
  const float vdx = 0.03125f;
  const float vdy = 0.0125f;
  const float vs = 2.0f;
  const float bias = 0.35f;
 
  unsigned int i, j;
  unsigned width = rowstride;
  //i = get_global_id(0);
  //j = get_global_id(1);
  uint id = get_global_id(0);
  
  i = id % width;
  j = id / width;
  
  float vx = convert_float(i) * vdx;
  float vy = convert_float(j) * vdy;
  float vt = time * 2.0f;

  float4 colors; // 0 = red, 1 = green, 2 = blue
       
  float xx = vx * vs;
  float yy = vy * vs;

  colors.x = noise3(xx, vt, yy);
  colors.y = noise3(vt, yy, xx);
  colors.z = noise3(yy, xx, vt);
  
  colors += bias;
  colors.w = 1.0f; // alpha channel will be 255 in the end 

  clamp(colors, 0.0f, 1.0f);

  colors *= 255.0f;

  output[id] = convert_uchar4(colors);
}
