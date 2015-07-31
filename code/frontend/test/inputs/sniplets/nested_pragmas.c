#define MAX_ALN_LENGTH 10
#define TRUE 1
#define INT_SCALE 1

#define MIN(x,y)\
	(x<y)? x: y;


int pw_go_penalty;
int mat_avscore;
int gap_open_scale;
int pw_ge_penalty;
int gap_extend_scale;


int main()
{
   int i, n, m, si, sj;
   int len1, len2, maxres;
   double gg, mm_score;
   int    *mat_xref, *matptr;

   maxres = 100;
   int nseqs = 10;

   int seqlen_array[101];
   int seq_array[101][10];

   char gap_pos1, gap_pos2;
   int bench_output[10];

   int dnaFlag;


   #pragma omp parallel
   {
   #pragma omp single private(i,n,si,sj,len1,m)
      for (si = 0; si < nseqs; si++) {
         n = seqlen_array[si+1];
         for (i = 1, len1 = 0; i <= n; i++) {
            char c = seq_array[si+1][i];
            if ((c != gap_pos1) && (c != gap_pos2)) len1++;
         }
         for (sj = si + 1; sj < nseqs; sj++)
         {
            m = seqlen_array[sj+1];
            if ( n == 0 || m == 0 ) {
               bench_output[si*nseqs+sj] = (int) 1.0;
            } else {
               #pragma omp task untied \
               private(i,gg,len2,mm_score) firstprivate(m,n,si,sj,len1) \
               shared(nseqs,bench_output,seqlen_array,seq_array,gap_pos1,gap_pos2,pw_ge_penalty,pw_go_penalty,mat_avscore)
               {
                  int se1, se2, sb1, sb2, maxscore, seq1, seq2, g, gh;
                  int displ[2*MAX_ALN_LENGTH+1];
                  int print_ptr, last_print;

                  for (i = 1, len2 = 0; i <= m; i++) {
                     char c = seq_array[sj+1][i];
                     if ((c != gap_pos1) && (c != gap_pos2)) len2++;
                  }
                  if ( dnaFlag == TRUE ) {
                     g  = (int) ( 2 * INT_SCALE * pw_go_penalty * gap_open_scale ); // gapOpen
                     gh = (int) (INT_SCALE * pw_ge_penalty * gap_extend_scale); //gapExtend
                  } else {
                     gg = pw_go_penalty; // temporary value
                     g  = (int) ((mat_avscore <= 0) ? (2 * INT_SCALE * gg) : (2 * mat_avscore * gg * gap_open_scale) ); // gapOpen
                     gh = (int) (INT_SCALE * pw_ge_penalty); //gapExtend
                  }

                  seq1 = si + 1;
                  seq2 = sj + 1;

                  //forward_pass(&seq_array[seq1][0], &seq_array[seq2][0], n, m, &se1, &se2, &maxscore, g, gh);
                  //reverse_pass(&seq_array[seq1][0], &seq_array[seq2][0], se1, se2, &sb1, &sb2, maxscore, g, gh);

                  print_ptr  = 1;
                  last_print = 0;

                  //diff(sb1-1, sb2-1, se1-sb1+1, se2-sb2+1, 0, 0, &print_ptr, &last_print, displ, seq1, seq2, g, gh);
                  //mm_score = tracepath(sb1, sb2, &print_ptr, displ, seq1, seq2);

                  if (len1 == 0 || len2 == 0) mm_score  = 0.0;
                  else                        mm_score /= (double) MIN(len1,len2);

                  bench_output[si*nseqs+sj] = (int) mm_score;
               } // end task
            } // end if (n == 0 || m == 0)
         } // for (j)
      } // end single
   } // end parallel
   return 0;
}
