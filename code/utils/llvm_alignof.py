# replace LLVM alignof<T>() method in llvm_alignof<T> for C++0x compability issues 
import os, sys

def rename(d): 
	for root,dirs,files in os.walk(d):
		print "Visiting dir: " + root
		for file in files:
			 if file.endswith('.h'):
					code = open(root + "/" + file).read()
					code = code.replace('alignof', 'llvm_alignof')
					open(root + "/" + file, 'w').write(code)
		for dir in dirs:
				rename(dir)

print "Starting code substitution, root directory is: " + sys.argv[1]
rename(sys.argv[1])
print "done."
