
int main() {

    int arr[] = { 1,2,3,0,4 };

    int *ip = arr+1;
	
    return *++(++(--(++ip)));
}
