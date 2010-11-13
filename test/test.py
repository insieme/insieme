import os, subprocess, shlex, sys

INSIEMEC='../../../insieme2_build/code/driver/main'
CFLAGS = "--std=c99"
LDFLAGS = "-lm"
CC = "gcc"

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

    def disable(self):
        self.HEADER = ''
        self.OKBLUE = ''
        self.OKGREEN = ''
        self.WARNING = ''
        self.FAIL = ''
        self.ENDC = ''

def run_cmd(cmd):
    pid = subprocess.Popen( shlex.split(cmd), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    pid.wait()
    if pid.returncode == 0:
        return None;
    return pid.communicate()[1]

def check_for_error(ret):
    if ret is not None:
        sys.stderr.write(bcolors.FAIL + '@ [FAILED]' + bcolors.ENDC + '\n')
        sys.stderr.write(bcolors.FAIL + "@------------------ start cause --------------------\n" + bcolors.ENDC)
        sys.stderr.write(ret)
        sys.stderr.write(bcolors.FAIL + "@------------------- end cause ---------------------\n\n" + bcolors.ENDC)
        return True
    return False
    
def check_diff(ref, test):
    ret = run_cmd("diff {0} {1}".format(ref, test))
    return not check_for_error(ret)

failed_test = 0

def fail_test(failed_test):
    failed_test += 1
    os.chdir("../")

test_num = 1;

for (_,dirs,_) in os.walk("."):
    for test_case_name in dirs:
        if test_case_name.startswith("#"):
            continue # SKIP
            
        print "#----------------------------------------"
        print "# {0}: {1}".format(test_num, test_case_name)
        
        # name of the file contaning the reference IR for the current test
        refIr   = test_case_name + '.ref.ir'
        # name of the file containing the generated IR for the case test
        testIr  = test_case_name + '.ir'
        # name of the file containing the reference C output for the current test
        refC    = test_case_name + '.ref.insieme.c'
        # name of the file containing the generated C output for the current test
        testC   = test_case_name + '.insieme.c'
        
        os.chdir(test_case_name)
        # RUN insieme to generate the IR dump and C output
        run_cmd( "{0} {1}.c --dump-ir={2} -o {3}".format(INSIEMEC, test_case_name, testIr, testC) )
        
        print " * Comparing generated IR... "
        if not check_diff(refIr, testIr):
            fail_test(failed_test)
            continue
        print " * -> " + bcolors.OKGREEN  +"Success." + bcolors.ENDC
          
        print " * Comparing generated C output... "
        if not check_diff(refC, testC):
            fail_test(failed_test)
            continue
        print " * -> " + bcolors.OKGREEN  +"Success." + bcolors.ENDC
        
        # file containing the output of the reference program
        ref_out = "ref.out"
        # file containing the output of the test program
        test_out = "test.out"
        
        run_cmd( "{0} {1} -o ref {2}.c {3}".format(CC, CFLAGS, test_case_name, LDFLAGS) )
        os.system("./ref > {0}".format(ref_out))
        
        print " * Compiling generated C output... "
        ret = run_cmd("{0} {1} -o test {2} {3}".format(CC, CFLAGS, testC, LDFLAGS))
        if check_for_error(ret):
            fail_test(failed_test)
            continue
        print " * -> " + bcolors.OKGREEN  +"Success." + bcolors.ENDC
        
        os.system("./test > {0}".format(test_out))
        
        print " * Checking generated program output... "
        if not check_diff(ref_out, test_out):
            fail_test(failed_test)
            continue
        print " * -> " + bcolors.OKGREEN  +"Success." + bcolors.ENDC
        
        os.remove( testIr )
        os.remove( testC )
        os.remove( ref_out )
        os.remove( test_out )
        os.remove( "ref" )
        os.remove( "test" )
        
        print "@ " + bcolors.OKGREEN + "[SUCCESS]" + bcolors.ENDC
        os.chdir("../")
        
        test_num += 1
        
os._exit(failed_test);
        