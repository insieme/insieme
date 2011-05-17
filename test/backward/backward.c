// backward pricing kernel 
// h.m. 2011-02-07

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define mmax 1000 // 100 time steps: time indices from 0 to 100 (total of 101)
#define jmax 1000 // 100 value steps in each direction + 0: value indices from 0 to 200 (total of 201)

#define c_fix 0.05 // fix coupon rate

static double rate[(2*jmax+1)*mmax];
static double prob[3*(2*jmax+1)];
static int    succ[3*(2*jmax+1)];

static double pv[2*jmax+1];
static double pv_1[2*jmax+1];

static double nomval=1000.0;
static double dt=1.0;
// static double dt=1.0/12.0;

clock_t begin_timer()
{
    clock_t t_begin; 
    t_begin = clock(); 
    return t_begin;
}

clock_t end_timer(clock_t t_begin)
{
    clock_t t_end;
    t_end = clock(); 
    return t_end-t_begin;
}

double coupon (int m, double r, double nomval) {
    return nomval*(exp(r)-1.0); // variable coupon
    // return nomval*c_fix;		// fix coupon
    }

void backward_column(int m) {

    
    // printf("column %d\n",m);

	//double pv_u, pv_m, pv_d;
    	//double prob_u, prob_m, prob_d;
    	//int i,j;


	    #pragma omp for
	    for(int j = 0; j<=2*jmax; j++) {

		// printf("row %d \n",j);

		double prob_u = prob[j*3+2];
		double prob_m = prob[j*3+1];
		double prob_d = prob[j*3];
	    
		double pv_u = pv_1[succ[j*3+2]];
		double pv_m = pv_1[succ[j*3+1]];
		double pv_d = pv_1[succ[j*3]];
	    
		int i = (2*jmax+1)*m + j; // i : (m,j) linearized

		pv[j] = exp(-rate[i]*dt) * 
		    (prob_d*pv_d + prob_m*pv_m + prob_u*pv_u + coupon(m,rate[i],nomval)) ;

		// printf("rate = %f\n",rate[i]);
		// printf("coupon = %f\n", coupon(m,rate[i],nomval));
		// printf("pv = %f\n",pv[j]);
	}

}

int main() {

    int m, j;
    float ticks;

    succ[2] = 2;
    succ[1] = 1;
    succ[0] = 0;
    for(j = 1; j<2*jmax; j++) {
         succ[j*3+2] = j+1;  // up
         succ[j*3+1] = j;    // mid
         succ[j*3]   = j-1;  // down
        }
    succ[(2*jmax)*3+2] = 2*jmax;
    succ[(2*jmax)*3+1] = 2*jmax-1;
    succ[(2*jmax)*3]   = 2*jmax-2;

    for(j = 0; j<=2*jmax; j++) {
         prob[j*3+2] = (1.0/4.0)*(1.0-0.05+0.1*(rand()/(RAND_MAX + 1.0)));  // up
         prob[j*3+1] = (1.0/2.0)*(1.0-0.05+0.1*(rand()/(RAND_MAX + 1.0)));  // mid
         prob[j*3]   = 1.0 - prob[j*3+1] - prob[j*3+2];  // down
         // printf("prob = %d %f %f %f\n",j,prob[j*3+2],prob[j*3+1],prob[j*3]);
        }

    for(m=0; m<mmax; m++) {
        for(j = 0; j<=2*jmax; j++) {
            rate[(2*jmax+1)*m+j] = (0.01+0.05*((double)j/(double)jmax)+0.03*((double)m/(double)mmax))*
                (1.0-0.05+0.1*(rand()/(RAND_MAX + 1.0)));
            }
        }

    printf ("start..\n");

    double begin = begin_timer();

    for(j = 0; j<=2*jmax; j++) {
        pv_1[j] = nomval;
        }

    #pragma omp parallel
    {
    for(int n = mmax-1; n>=0; n--) {

        backward_column(n);

	#pragma omp for
        for(int j = 0; j<=2*jmax; j++) {
            pv_1[j] = pv[j];
            }

        }
   }

    ticks = end_timer(begin);    

    printf("pval=%8.2f\n", pv[jmax]);

    //printf("time=%8.2f sec\n", ticks/1000000.0f);
    }
