#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>
#include <time.h>
typedef struct {
  float val;
  int index;
} THEFIT;

    THEFIT *work;
    THEFIT *a;
#pragma omp threadprivate (work,a)


void RecMergeSort(int left, int right);
void Sort(THEFIT *Ain, int n);
void Merge(int s, int n, int m);
void merge2(THEFIT *d1,int n,THEFIT *d2,int m,THEFIT *out);

THEFIT *vector(int nl, int nh);
void free_vector(THEFIT *v, int nl);


void validate(THEFIT *A, int N){
    float before=A[1].val;
    for (int i=2;i<=N;i++){
        if(before<A[i].val){
            printf("Validation not successfull!\n");
            //exit(0);
        }
        before=A[i].val;
    }
    printf("Validation successfull!\n");
}

void randomArray(THEFIT *A, int N){
    time_t ti=time(NULL);
    srand((unsigned int)ti);

    for (int i=0;i<N;i++)
        A[i].val=rand()%500;
}


int main() {
    THEFIT *data,*output;
    int i,k,k1,k2;
    double start=omp_get_wtime();
    i=30000000;
    data=vector(1,i);

    randomArray(data,i);

    k=i/2;
    k1=k+1;
    k2=(i-k1)+1;
#pragma omp parallel sections
    {
#pragma omp section
    Sort(&data[1],k);
#pragma omp section
    Sort(&data[k1],k2);
    }

    output=vector(1,i);
    merge2(&data[1],k,&data[k1],k2,&output[1]);

    double end=omp_get_wtime();
    validate(output,i);
//    printf("time taken: %.4f\n",end-start);
    return 0;
}

 void Sort(THEFIT *Ain, int n){
    work=vector(1,n);
    a=Ain-1;
    RecMergeSort(1,n);
    free_vector(work,1);
 }

  void RecMergeSort(int left, int right) {
    int  middle;
    if (left < right) {
        middle = (left + right) / 2;
        RecMergeSort(left,middle);
        RecMergeSort(middle+1,right);
        Merge(left,middle-left+1,right-middle);
    }
  }

  void Merge(int s, int n, int m) {
    int i,  j, k, t, u;
    k = 1;
    t = s + n;
    u = t + m;
    i = s;
    j = t;
    if ((i < t) && (j < u)){
        while ((i < t) && (j < u)){
            if (a[i].val >= a[j].val){
                work[k] = a[i];
                i = i + 1;
                k = k + 1;
            } else {
                work[k] = a[j];
                j = j + 1;
                k = k + 1;
            }
         }
    }
    if(i < t ){
        while (i < t ) {
            work[k] = a[i];
            i = i + 1;
            k = k + 1;
        }
    }
    if(j < u){
        while (j < u ) {
            work[k] = a[j];
            j = j + 1;
            k = k + 1;
        }
    }
    i = s;
    k=k-1;
    for( j = 1; j<= k; j++) {
        a[i] = work[j];
        i = i + 1;
    }
  }

/*
! this subroutine takes two sorted lists of type(THEFIT) and merges them

! input d1(1:n) , d2(1:m)
! output out(1:n+m)
*/
void merge2(THEFIT *d1,int n,THEFIT *d2,int m,THEFIT *out) {
  int i,j,k;
  i=1;
  j=1;
  d1--; d2--; out--;
  for( k=1; k<=n+m;k++) {
    if(i > n){
      out[k]=d2[j];
      j=j+1;
    }
    else if(j > m){
      out[k]=d1[i];
      i=i+1;
    } else {
      if(d1[i].val > d2[j].val){
        out[k]=d1[i];
        i=i+1;
      } else {
        out[k]=d2[j];
        j=j+1;
      }
    }
   }
  }


THEFIT *vector(int nl, int nh)
{
        THEFIT *v;

        v=(THEFIT *)malloc((unsigned) (nh-nl+1)*sizeof(THEFIT));
        if (!v) {
            printf("allocation failure in ivector()\n");
                exit(1);
    }
        return v-nl;
}


void free_vector(THEFIT *v, int nl)

{

 free((char*) (v+nl));

}

