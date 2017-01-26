#include <fstream>
#include <string>
#include <iostream>

int main(int argc, char** argv) {
    if(argc < 2) {
        std::cout << "Usage: " << argv[0] << " [input file]\n";
        return -1;
    }

    std::fstream in(argv[1], std::fstream::in);
    std::fstream out("input_file.h", std::fstream::out);

    out << "#ifndef _INPUT_FILE_H_\n";
    out << "#define _INPUT_FILE_H_\n\n";
    
    // The following is needed by our version of stdio.h
    out << "#include <assert.h>\n\n";
    out << "long input_file_pos = 0;\n\n";

    out << "const char input_file[] = {\n\t";

    unsigned long len = 0;
    char c;
    unsigned long newline = 0;

    in.read(&c, 1);
    while(!in.eof()) {
        out << std::to_string(c) << ", ";

        if(++newline > 49) {
            out << "\n\t";
            newline = 0;
        }
        len ++;
        in.read(&c, 1);
    }

    out << EOF; 
    out << "\n};\n\n";
    out << "const unsigned long INPUT_FILE_LEN = " << len << ";\n\n";
    out << "#endif // _INPUT_FILE_H_\n";
    in.close();
    out.close();

    return 0;
}
