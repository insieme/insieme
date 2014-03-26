typedef struct {

	int id;

} ms;

ms *func()
{
	static ms static_s = {-1};
	
	return &static_s;
}

int main() {

    func();

    return 0;
}
