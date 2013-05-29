


void f (int *a, int *b){
}


int main(){

	int d[3];
	int array[2][2][2];

	int i,j,k;
	for (i =0 ; i < d[0]; i++){
	for (j =0 ; j < d[1]; j++){
	for (k =0 ; k < d[2]; k++){
		f(&i,&j);
	}
	}
	}

}
