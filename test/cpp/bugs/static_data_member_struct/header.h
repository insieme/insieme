extern int globalx;
struct AF {
	AF();
	static AF* afp;
	static AF* getInst();
	void cleanup();
};
