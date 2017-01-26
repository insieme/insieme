int func(void * ptr) {
        return *((int*)ptr);
}

int main() {
    int i[100];
    void *ptr = i;

    #pragma omp parallel
    func(ptr);

    return 0;
}

