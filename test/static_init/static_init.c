typedef struct {

	int id;

} ms;

ms *func()
{
	static ms static_s = {-1};
    static ms static_v[] = { { 0 } };
    static char cvec[] = "hello!";
	
    static_s.id += static_v[0].id;

	return &static_s;
}

int main() {

    func();

    return 0;
}
