
void a();
void b();

void a(){
	b();
}
void b(){
	a();
}

int main(){
	a();
}
