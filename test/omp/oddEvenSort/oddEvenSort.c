#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <omp.h>

#define size 10000

void validate(int *A, int N){
    int before=A[0];
    for (int i=1;i<N;i++){
        if(before>A[i]){
            printf("Validation not successfull!\n");
            exit(0);
        }
        before=A[i];
    }
    printf("Validation successfull!\n");
}

void randomArray(int *A, int N){
    time_t ti=time(NULL);
    srand((unsigned int)ti);

    for (int i=0;i<N;i++)
        A[i]=rand()%500;
}

void oddEvenSort (int *A, int N) {
    int exch0, exch1 = 1,trips=0,i;
    while ( exch1 ) {
        exch0=0;
        exch1=0;
        #pragma omp parallel
        {
            int temp;

            #pragma omp for
            for ( i = 0 ; i < N-1; i += 2) {
                if (A[ i ] > A[ i +1]) {
                    temp = A[ i ] ;
                    A[ i ] = A[ i +1] ;
                    A[ i +1] = temp ;
                    exch0=1;
                }
            }
            if(exch0 || ! trips) {
                #pragma omp for
                for (i=1; i<N-1; i+=2) {
                    if (A[ i ] > A[ i +1]) {
                        temp = A[ i ] ;
                        A[ i ] = A[ i +1] ;
                        A[ i +1] = temp ;
                        exch1=1;
                    }
                }
            }

        }
        trips=1;
    }
}

int main(){
    int* A=(int*) malloc(sizeof(int)*size);
    randomArray(A,size);
//    double start=omp_get_wtime();
    oddEvenSort(A,size);
  //  double end=omp_get_wtime();
    validate(A,size);
//    printf("Time: %.4f\n",end-start);
    free(A);
}
