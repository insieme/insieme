Visual Studio has no support for inline assembly code within x64 applications. Therefore the assembly code was moved to the external file win64asm.asm. You'll need YASM Assembler to assemble this file, which you can get here: http://yasm.tortall.net/

Assemble the .asm file using: yasm -f win64 win64asm.asm

which will produce an obj file with the same name. (If your have downloaded a yasm-1.2.0-win64.exe file, rename it to yasm.exe for simplicity)

However, a preassembled version is contained in this folder: win64asm.obj. You need to link this file whenever you want to compile a program using the Insieme Runtime under Windows x64.
