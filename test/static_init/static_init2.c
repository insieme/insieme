
int func() {
    typedef struct {
    	char *str;
    } ts;

    static ts s[] = { { "help" } };

    return s[0].str[0];
}

int main() 
{
    return func();
}
