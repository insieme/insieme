#! /bin/sh

""":"
exec python2.7 $0 ${1+"$@"}
"""
import optparse, sys, os, re, random, math, shutil, time, subprocess, shlex, copy, re
import traceback, datetime, binascii, logging

from multiprocessing import Pool, Process, Queue

ISOLATOR      	= '${INSIEME_DRIVER_BUILD}/isolator'
CC			  	= 'ccache gcc'
CFG_FILE_NAME 	= './test.cfg'
INPUTS_DATA   	= './inputs.data'
# INSIEME_FLAGS = '/insieme.flags'
REF_GCC_FLAGS 	= './ref-gcc.flags'
TEST_GCC_FLAGS	= './test-gcc.flags'
PROG_INPUTS	  	= './prog.input'

REF_GCC_FLAGS  =  "-std=c99 -I{INSIEME_RUNTIME_SRC}/include "\
				  "-D_XOPEN_SOURCE=700 -D_GNU_SOURCE -ldl -lm -pthread -lrt "\
				  "-L{INSIEME_RUNTIME_BUILD}/pmlib -linsieme_power_measurement"

INSIEME_CFLAGS =  ''

# Reads the configuration file returning the list of defined configuration in the order of
# definition
def read_runtime_confs(file_path):
	assert os.path.exists(file_path)

	confs = []

	# List of standard variables which can be used in the config file
	variables = [ ('NUM_CORES', os.sysconf("SC_NPROCESSORS_ONLN")) ] 

	lines = open(file_path, 'r').read().split('\n')
	# read all the variables in this configuration file 
	line_no = 0
	while line_no < len(lines):
		line = lines[line_no].strip()
		# skip comments
		if len(line) == 0 or line.startswith('#'):
			line_no+=1
			continue
		# as soon as we encounter a configuration we exit from this loop
		if line.startswith('['):
			break
		
		name = line.split('=')[0].strip()
		value = line.split('=')[1].strip()
		while value.endswith('\\'):
			# remove the '\'
			value = value[:-1]
			line_no+=1
			value += lines[line_no].strip()

		variables.append( (name, value) )
		line_no += 1

	# if we exit it means we are positioned on the first configuration
	while line_no < len(lines):
		line = lines[line_no].strip()
		if len(line) == 0:
			line_no+=1
			continue

		assert line.startswith('[CONFIG]')
		# the first line is the set of compiler flags
		flags = [flag.strip().format( **dict(variables) ) for flag in lines[line_no+1].split(',')]
		# the second line is the set of environmental variables 
		envs = [env.strip().format( **dict(variables) ) for env in lines[line_no+2].split(',')]
		line_no += 3

		confs.append( (flags, envs) )
	
	print ('Number of runtime configurations: {0}'.format(len(confs)))
	return confs


###################################################################################################
##
## Builds the list of test cases to be executed.
##
## This is done by searching for the file test.cfg which hould be contained in those folder grouping
## together sub benchmarks.
###################################################################################################

def read_test_cfg( path, prefix='./' ):
	assert os.path.exists( path )
	return [prefix + x.strip() \
			  for x in open(path).read().split('\n') \
					if len(x) > 0 and not x.startswith('#') ]

def expand(tests):
	if isinstance(tests, list):
		ret = []
		for test in tests:
			ret.extend(expand(test))
		return ret

	if not os.path.isdir( tests ):
		ret = [x.strip() for x in open(tests).read().split('\n') if len(x) > 0 and not x.startswith('#') ]
		return expand( ret )

	# Handle the cases where we need to recur and resolve
	assert os.path.isdir( tests )

	# If the directoy has a test.cfg file it means we have to run all the
	# contained tests
	if os.path.exists( tests+CFG_FILE_NAME ):
		return expand( read_test_cfg( tests+CFG_FILE_NAME, tests ) )

	return [ tests ]

def load_from_file(file, default=''):
	if os.path.exists(file):
		return " ".join( [ x.strip() for x in open(file).read().format( **{'SRC_DIR': ''} ).split(',') ] )
	return default

# Combine multiple iterators into a single iterator, therefore making it possible to iterate through
# the cross product of the given iterators. This is done without actually computing the
# cross-product but instead yielding the values one after the other 
def iterate(iter):
	# if we get a list with a single element, unpack it
	if isinstance(iter, list) and len(iter) == 1:
		iter = iter[0]

	# if the element is not a list, we just go through its elements
	if not isinstance(iter, list):
		for (it, cfg) in iter:
			yield "{0}".format(it), cfg
		raise StopIteration

	# we have a list of iterators! we start chaining the first one
	for it1, cfg_none in iter[0]:
		# else, recursively call the function
		iter[1].folder = iter[0].folder + '/' + it1
		for it2, cfg in iterate( iter[1:] ):
			yield "{0}/{1}".format(it1, it2), cfg

	raise StopIteration

# Special iterator class which iterates through the list of given bechmarks 
class BenchIterator:
	def __init__(self, base_path, benchs):
		self.folder = base_path
		self.benchs = benchs if isinstance(benchs, list) else [benchs]

	def __iter__(self):
		return self.next()

	def next(self):
		for bench in self.benchs:
			yield bench.replace('/', '.'), None

		raise StopIteration

# Iterates through a list of subfolders having a name in the form of *name*. The folders are
# visited in alphabetical order
class SimpleSubFolderIterator:
	
	def __init__(self, base_path='./'):
		self.folder = base_path

	def __iter__(self):
		return self.next()

	def next(self):
		assert self.folder is not None

		for folder in sorted([(f) for f in os.listdir(self.folder) if os.path.isdir(self.folder + '/' + f) ]):
			yield folder, None
			assert self.folder is not None

		raise StopIteration

# Iterates through a list of subfolders in the form 'name_ID'. The folders are visited according to
# the ascending order of IDs 
class SubFolderIterator:

	def __init__(self, name, base_path='./', sep='\_'):
		self.folder = base_path
		self.sep = sep
		self.name = name

	def __iter__(self):
		return self.next()

	def next(self):
		assert self.folder is not None

		m = re.compile("{0}{1}(?P<id>(\d+))".format(self.name, self.sep))
		for id, folder in \
				sorted([(int(match.group('id')),folder) for folder,match in \
							[(folder, m.match(folder)) for folder in os.listdir( self.folder )] \
								if match is not None
							]):
			yield folder, id
			assert self.folder is not None

		raise StopIteration

# Iterates through configurations. The list of available configurations is read from the global
# variable confs which is set in the main function 
class ConfigIterator:
	def __init__(self, name):
		self.name = name

	def __iter__(self):
		return self.next()

	def next(self):
		for cfg in confs:
			yield '{0}_{1}'.format(self.name, confs.index(cfg)), cfg
		raise StopIteration

###################################################################################################
##
## Initialize the environment
##
###################################################################################################

def init( output_dir, bench_path ):

	bench_c_name = os.path.basename(bench_path)
	bench_name = bench_path.replace('/', '.')

	save_path = os.getcwd()
	try:
		print ('Setting up benchmark \'{0}\''.format(bench_path))
		print("</br>")
		os.chdir(bench_path)

		inputs 		 =	load_from_file(INPUTS_DATA, bench_c_name + '.c')
		# insiemeFlags = load_from_file(bench_path + INSIEME_FLAGS, INSIEME_CFLAGS)
		gccFlagsTest = load_from_file(TEST_GCC_FLAGS)
		input_args   = load_from_file(PROG_INPUTS)

		output_path = output_dir if os.path.isabs(output_dir) else save_path+'/'+output_dir

		print ('Producing outputs in {0}'.format(output_path))
		print("</br>")
		ret = os.system('{0} -D ENABLE_TIME_REPORT {1} {2} -n {3} -d {4}'.
			format(ISOLATOR, INSIEME_CFLAGS, inputs, bench_name, output_path)) >> 8

		output_path += '/'+bench_name

		# if ret != 0:
		# 	raise Exception("Isolator failed on benchmark '{0}'".format(bench_name))

		for path, cfg in iterate([ SubFolderIterator('kernel', output_path),
									SubFolderIterator('version'),
									ConfigIterator('config')
								 ]):

			# for each kernel iterate through the versions
			cfg_dir = output_path+'/'+path

			if not os.path.exists(cfg_dir):
				os.mkdir(cfg_dir)
			# else:
				# print ('WARNING: Directory for runtime configuration \'{0}\' already exists, skipping'.
				#	format(cfg_dir))

		print ('Done \'{0}\''.format(bench_path))
		print("</br>")
	
	# Catch any exception happening within this version and print a stack trace
	except Exception as ex:
		print("Exception raised: {0}".format(ex))
		traceback.print_exc()

	finally :
		os.chdir(save_path)

###################################################################################################
##
## Compiles Code Versions 
##
## Next section is responsible to compile the code versions which have been generated by the 
## initialization process. Compilation happens when the -c flag is used, which force the parallel 
## compilation of all the generated code versions or when the code versions are executed (-e). 
## In that case if the folder containing the configuration files has no executable, the compilation 
## is invoked and the executable is kept. 
##
###################################################################################################

backend_queue = Queue()

def gcc_backend_worker( ):
	
	while True:
		job = backend_queue.get()
		if job is None: break
		gcc_backend( *job )

def gcc_backend( cfg_num, file_name, cmd ):

	print('[GCC]: Compiling {0} ({1:{2}}/{3}) '.format(file_name, cfg_num, len(str(num_cfgs)), num_cfgs))
	print (cmd)
	print()
	start = time.time()
	pid = subprocess.Popen( shlex.split(cmd), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
	output = pid.communicate()[0]
	if pid.returncode != 0:
		print("</br>")
		# something went wrong, the program which was supposed to work is now failing
		print ("<font color='red'><b>[GCC]: compilation failed, reason:\n{0}</font>".format(output) )
	else:
		print ('[Time: <b>{0:.4} secs</b>]'.format( time.time()-start ))
	print("</br>")
	sys.stdout.flush()


def run_backend( output_dir, bench_path ):

	bench_c_name = os.path.basename(bench_path)
	bench_name = bench_path.replace('/', '.')

	save_path = os.getcwd()

	try:
		print ('[GCC]: Compiling benchmark \'{0}\''.format(bench_path))
		os.chdir(bench_path)
		cfg_num = 1

		inputs 		 =	load_from_file(INPUTS_DATA, bench_c_name + '.c')
		# insiemeFlags = load_from_file(bench_path + INSIEME_FLAGS, INSIEME_CFLAGS)
		gccFlagsTest = load_from_file(TEST_GCC_FLAGS)
		input_args   = load_from_file(PROG_INPUTS)

		output_path = output_dir if os.path.isabs(output_dir) else save_path+'/'+output_dir

		# compile, for each generated kernel and version the code for a specific configuration of the runtime
		output_path += '/'+bench_name

		for path, cfg in iterate([ SubFolderIterator('kernel', output_path),
								   SubFolderIterator('version'),
								   ConfigIterator('config')
						 		 ]):
			# for each kernel iterate through the versions
			cfg_dir = output_path+'/'+path

			# Build compilation command
			cmd = '{GCC} {CFLAGS_GCC} {FLAGS} {CFG}/../kernel.c -o {CFG}/kernel'.format(
					**{'GCC':			CC,
						'CFLAGS_GCC':	REF_GCC_FLAGS,
						'FLAGS':		' '.join(cfg[0]),
						'CFG': 			cfg_dir
					})
			cmd = cmd.format(**{'INSIEME_RUNTIME_SRC': INSIEME_RUNTIME_SRC,
								'INSIEME_RUNTIME_BUILD': INSIEME_RUNTIME_BUILD
						 	 })
			backend_queue.put( (cfg_num, bench_path+'/'+path, cmd) )
			cfg_num+=1
	
	# Catch any exception happening within this version and print a stack trace
	except Exception as ex:
		print("Exception raised: {0}".format(ex))
		traceback.print_exc()

	finally :
		os.chdir(save_path)

###################################################################################################
##
## Execute the code versions 
##
###################################################################################################

def run_local(envs, cmd, kill_after=None):
	save_envs = []
	# Set environment variables 
	for env_id, env_value in list(map(lambda x: x.split('='), envs)):
		save_envs.append( (env_id, os.getenv(env_id, '')) )
		os.putenv(env_id, env_value)

	pid = subprocess.Popen( shlex.split(cmd), stdout=subprocess.PIPE )
	if kill_after is None:
		pid.wait()
	else:
		remaining = kill_after
		while remaining > 0 and pid.poll() is None:
			time.sleep(1)
			remaining -= 1
	if pid.poll() is None:
	  print ("%%%% Killing process {0}".format( pid.pid ))
	  pid.kill()

	# Restore ENVIRONMENT Variables
	for env_id, env_value in save_envs:
		os.putenv(env_id, env_value)
	return pid.returncode, pid.communicate()

# Print a report to the stdout containing information on the execution of the benchmark
def write_report(retcode, stdout, stderr):
	stdout = str(stdout)
	stderr = str(stderr)

	if retcode != 0:
		print('%%%% ERROR!! {0}\nSTDOUT:\n{1}\nSTDERR:\n{2}'.format(retcode, stdout, stderr))
	else:
		m = re.compile("Verification\s+=\s+(?P<succ>(\w+))")
		match = m.search( stdout )
		if match and match.group('succ') == 'SUCCESSFUL':
			m = re.compile("Time in seconds\s+=\s+(?P<time>(\d+\.\d+))")
			match = m.search( stdout )
			print('<font color="green"><b>@@@@ SUCCESS: Execution Time was {0:.4} secs.</b></font>'.format( str(match.group('time')) ))
		else:
			print ('<font color="red"><b>%%%% ERROR Verification was: UNSUCCESSFUL</b></font>')
	print("</br>")
	sys.stdout.flush()

def run(output_dir, benchs, num_runs=1):
	print ('{0:~^80}'.format('> Saving previous execution files <'))
	print ('</br>')
	# cleaning all the status files from the previous runs
	for path, cfg in iterate([ BenchIterator(output_dir, benchs),
							   SubFolderIterator('kernel'),
							   SubFolderIterator('version'),
							   ConfigIterator('config')
							 ]):
		done_file_path = output_dir+'/'+path

		m = re.compile("done\.(?P<id>(\d+))")
		if os.path.exists(done_file_path + '/done'):
			done_ids = [int(match.group('id')) for folder,match in \
							[(folder, m.match(folder)) for folder in os.listdir( done_file_path )] \
								if match is not None ]
			shutil.move(done_file_path+'/done', done_file_path+'/done.{0}'.format(max(done_ids)+1 if len(done_ids)>0 else 0))

		os.system('touch {0}/done'.format(done_file_path))
	print ('{0:~^80}'.format('> Completed <'))

	# Start the execution of the benchmark
	print ('{0:~^80}'.format('> Start Execution <'))
	print ('</br>')

	for run in range(num_runs):
		print('{0:~^80}'.format('> Starting RUN #{0}/{1} <'.format(run, num_runs)))
		print ('</br>')
		
		# reset the configuration number
		cfg_num = 1

		# go through all the benchmars, kernels, versions and configurations 
		for path, cfg in iterate([ BenchIterator(output_dir, benchs),
								   SubFolderIterator('kernel'),
								   SubFolderIterator('version'),
								   ConfigIterator('config')
								 ]):

			save_path = os.getcwd()
			try:
				os.chdir(output_dir+'/'+path)

				# retrieve the last run_X folder for this subdirectory
				m = re.compile("run\_(?P<id>(\d+))")

				run_ids = [int(match.group('id')) for folder,match in \
								[(folder, m.match(folder)) for folder in os.listdir( './' )] \
									if match is not None ]

				folder_name = 'run_0'
				if len(run_ids) != 0:
					# create a new run folder adding 1
					folder_name = 'run_{0}'.format(max(run_ids)+1)

				print ('<hr/>')
				# this is the first run of this benchamrk
				os.mkdir(folder_name)
				os.chdir(folder_name)

				if not os.path.exists('../kernel'):
					cmd = '{GCC} {CFLAGS_GCC} {FLAGS} ../../kernel.c -o ../kernel'.format(
						**{'GCC':			CC,
						   'CFLAGS_GCC':	REF_GCC_FLAGS,
						   'FLAGS':			' '.join(cfg[0])
						})

					cmd = cmd.format(**{'INSIEME_RUNTIME_SRC': INSIEME_RUNTIME_SRC,
									    'INSIEME_RUNTIME_BUILD': INSIEME_RUNTIME_BUILD})

					gcc_backend(cfg_num, path, cmd)

				assert os.path.exists('../kernel')
				print ('[<b>{8}</b>]: Running configuration (<b>{0:{1}}/{2} - {3:{4}}/{5}</b>)\n<br>>>>> {6}/{7}'.
						format(run, len(str(num_runs)), num_runs, cfg_num, len(str(num_cfgs)),
									num_cfgs, path, folder_name, time.ctime()
								)
						)
				print("</br>")
				retcode, output = run_local(cfg[1], '../kernel')
				
				# Save the result on a file called kernel.out
				out = output[0]
				file = open('kernel.out', 'w')
				file.write(out)
				file.close()

				# Write a report on the execution of the benchmark
				write_report(retcode, output[0], output[1])

				assert os.path.exists('../done')

				# Add this run to the done file which contains the list of runs associated to this
				# code execution
				os.system('echo \'{0} {1}\' >> ../done'.format(folder_name, datetime.datetime.now()))

			except Exception as ex:
				print("Exception raised: {0}".format(ex))
				traceback.print_exc()

			finally :
				cfg_num += 1
				os.chdir(save_path)

		print ('{0:~^80}'.format('> Completed run #{0} <'.format(run)))
		print ('</br>')



###################################################################################################
##
## Analyze the results 
##
###################################################################################################


class Unsuccessfull(Exception):
	pass

class RuntimeError(Exception):
	pass

class NoData(Exception):
	pass

def extract_results(run_path):
	
	folder_name = run_path.split(' ')[0]
	assert os.path.exists(folder_name)

	# check whether kernel.out file is there
	if not os.path.exists(folder_name + '/kernel.out'):
		raise RuntimeError()

	# check whether the kernel.out file is empty
	if os.path.getsize(folder_name + '/kernel.out')==0:
		raise RuntimeError()
	
	# check whether there are a log files
	if not os.path.exists(folder_name + '/worker_event_log.0000'):
		raise NoData()

	# check whether the log files are empty
	if os.path.getsize(folder_name + '/worker_event_log.0000')==0:
		raise NoData()

	m = re.compile("Verification\s+=\s+(?P<succ>(\w+))")
	match = m.search( open(folder_name + '/kernel.out', 'r').read() )
	if match and match.group('succ') == 'UNSUCCESSFUL':
		raise Unsuccessfull()

	logs = []
	for file_name, id in SubFolderIterator('worker\_event\_log', folder_name, '.'):
		# Extract all the lines in the log which begins with a RG (these are the region information)
		lines = [line.strip() for line in open(folder_name+'/'+file_name, 'r').read().split('\n') if
				len(line) > 0 and line.startswith('RG' )]
		
		# If the log doesn't contain any region information then continue
		if len(lines) == 0:
			logs.append( [] )
			continue

		# assumes that every region start has a corresponding region end
		assert len(lines)%2 ==0 

		# Because a log can contain multiple region start/end we extract all the start/end tuples
		# and append to a list of measurments for this worker 

		region_num = 0
		for idx in range(0, len(lines), 2):
			reg_values_start = [val.strip() for val in lines[idx].split(',')]
			assert reg_values_start[2] == 'START'

			reg_values_end   = [val.strip() for val in lines[idx+1].split(',')]
			assert reg_values_end[2] == 'END'
			
			# make sure this is the same region 
			assert reg_values_start[1] == reg_values_end[1]

			ret = zip(reg_values_start[3:], reg_values_end[3:])

			values = []
			for tup in ret:
				for val in tup:
					values.append( val )

			logs.append( [id, region_num] + values )
			region_num+=1

	return logs;

(SUCCESS, UNSUCCESS, RUNTIME_ERROR, NO_DATA) = range(0,4)

header_line = [ 'bench', 'kernel', 'version', 'config', 'run', 'success', 'worker', 'region', 'start_ns', 'end_ns',	'start', 'end' ]

## Analyze the result of the benchmarks and produce a result document containing all the sensible
## informations
def analyze(output_dir, benchs):

	print (', '.join(map(lambda x : '{0}'.format(x), header_line)))

	# go through all the benchmars, kernels, versions and configurations 
	for cfg_path, cfg in iterate([SimpleSubFolderIterator( output_dir),
								  SubFolderIterator('kernel'),
								  SubFolderIterator('version'),
								  SubFolderIterator('config')]):

		save_path = os.getcwd()
		try:
			done_file_path = output_dir+'/'+cfg_path
			if not os.path.exists(done_file_path+'/done'):
				# benchmark is not executed, skip this configuration
				continue
			
			sub_vals = [sub for sub in cfg_path.split('/')]
			ret = [sub_vals[0]] + [int(val.split('_')[1]) for val in sub_vals[1:]]

			# enter the directoy where the results are stored 
			os.chdir(done_file_path)
		
			# the file ../done contains the list of runs which are executed for this experiment 
			assert ( os.path.exists('done') )

			runs = [run.strip() for run in open('done', 'r').read().split('\n') if len(run) > 0]
			# for each run extract the execution data 
			for curr_run in runs:
				line = ret[:] + [curr_run.split(' ')[0].split('_')[1]]
				try:
					for result in extract_results(curr_run):
						line = ret[:] + [curr_run.split(' ')[0].split('_')[1]]
						line.append( SUCCESS )
						line.extend( result )
						# print out the result 
						print (', '.join(map(lambda x : '{0}'.format(x), line)))
				except Unsuccessfull as ex:
					line.append( UNSUCCESS )
					print (', '.join(map(lambda x : '{0}'.format(x), line)))
				except RuntimeError as ex:
					line.append( RUNTIME_ERROR )
					print (', '.join(map(lambda x : '{0}'.format(x), line)))
				except NoData as ex:
					line.append( NO_DATA )
					print (', '.join(map(lambda x : '{0}'.format(x), line)))

		except Exception as ex:
			print("Exception raised: {0}".format(ex))
		finally :
			os.chdir(save_path)
	

## Analyze the size of regions
#def plot_regions(output_dir, benchs):

#    # go through all the benchmars, kernels, versions and configurations 
#    for bench_path, cfg in BenchIterator(output_dir, benchs):
	
#        print (bench_path)
#        ret = [ ['version'] + ['config_'+str(idx) for idx in range(len(confs))] ]

#        for kernel_path, cfg in SubFolderIterator('kernel', output_dir+'/'+bench_path+'/'):

#            save_path = os.getcwd()

#            cfgs = []
#            for path, cfg in iterate(SubFolderIterator('config', output_dir+'/'+bench_path+'/'+kernel_path+'/version_0/')):
#                try:
#                    done_file_path = output_dir+'/'+bench_path+'/'+kernel_path+'/version_0/'+path
#                    if not os.path.exists(done_file_path+'/done'):
#                        cfgs.append( None )
#                        # benchmark is not executed, skip this configuration
#                        continue
					
#                    # enter the directoy where the results are stored 
#                    os.chdir(done_file_path)
			
#                    # the file ../done contains the list of runs which are executed for this experiment 
#                    assert ( os.path.exists('done') )

#                    runs = [run.strip() for run in open('done', 'r').read().split('\n') if len(run) > 0]
					
#                    avg_time = None
#                    # for each run extract the execution data 
#                    if len(runs) > 0:
#                        results = list(map(extract_results, runs))
						
#                        if results is not None:
#                            avg_time = sum(map(lambda x: x[0], results))/len(runs)
					
#                    cfgs.append( avg_time/1000000 )
#                except Unsuccessfull as ex:
#                    cfgs.append( ("unsuccess") )
#                except Exception as ex:
#                    cfgs.append( (None) )
#                    # print("Exception raised: {0}".format(ex))
#                finally :
#                    os.chdir(save_path)
	
#            # do reorder
#            ret.append( [kernel_path] + cfgs )
		
#        print ( '\n'.join( list(map(lambda x: ', '.join(map(lambda y: '{0}'.format(y),x)), ret)) ) )

def main(argv=None):
	parser = optparse.OptionParser()

	parser.add_option("-w", "--workers", dest="workers", type="int",
							help=("The number parallel workers: default: '%default'."),
							default=os.sysconf("SC_NPROCESSORS_ONLN"))

	parser.add_option("-r", "--runs", dest="runs", type="int",
							help=("Execute the binary multiple times and produce a report on"
							" average execution time and standard deviation , default= %default"),
							default=1)

	parser.add_option("-i", "--init", action="store_true", dest="init",
							help=("Initialize test environment, default= %default"),
							default=False)

	parser.add_option("-e", "--execute", action="store_true", dest="execute",
							help=("Execute the tests, default= %default"),
							default=False)

	parser.add_option("-d", "--directory", dest="output_dir",
							help=("Execute the tests, default= %default"),
							default='uberout/')

	parser.add_option("-c", action="store_true", dest="compile",
							help=("Enable compilation of the generated kernels default= %default"),
							default=False)

	parser.add_option("--rc", dest="reorder", 
							help=("Reorder runtime configurations = %default"),
							default=None)

	parser.add_option("--analyze", action="store_true", dest="analyze", 
							help=("Analyize data = %default"),
							default=True)

	(options, args) = parser.parse_args()

	global runs
	runs = options.runs

	global confs
	confs = read_runtime_confs('./runtime.cfg')

	global confs_order
	confs_order = range(len(confs)) if options.reorder is None else [int(val) for val in options.reorder.split(',')]

	global INSIEME_RUNTIME_SRC
	INSIEME_RUNTIME_SRC = os.getenv('INSIEME_RUNTIME_SRC', '')
	if INSIEME_RUNTIME_SRC is '':
		print('WARNING: INSIEME_RUNTIME_SRC envinronment path not set')

	global INSIEME_RUNTIME_BUILD
	INSIEME_RUNTIME_BUILD = os.getenv('INSIEME_RUNTIME_BUILD', '')
	if INSIEME_RUNTIME_BUILD is '':
		print('WARNING: INSIEME_RUNTIME_BUILD envinronment path not set')

	global INSIEME_DRIVER_BUILD
	INSIEME_DRIVER_BUILD = os.getenv('INSIEME_DRIVER_BUILD', '')
	if INSIEME_DRIVER_BUILD is '':
		print('WARNING: INSIEME_DRIVER_BUILD envinronment path not set')

	output_dir = options.output_dir
	test_cases = expand( args )

	if options.init is True:
		print ('{0:~^80}'.format('> Initializing environment <'))
		for test_case in test_cases:
			init(output_dir, test_case)
		print ('{0:~^80}'.format('> Environment initialization completed <'))

	global num_cfgs
	num_cfgs=0
	# count the number of total configurations
	for path, cfg in iterate([ BenchIterator(output_dir, test_cases),
								SubFolderIterator('kernel'),
								SubFolderIterator('version'),
								ConfigIterator('config')
							]):
		num_cfgs+=1

	print('Total number of configurations: {0}'.format(num_cfgs))
	print('</br>')
	if options.compile is True:
		print ('{0:~^80}'.format('> Compiling generated kernels/versions <'))

		t = [0]*options.workers
		for w in range(options.workers):
			t[w] = Process(target=gcc_backend_worker)
			t[w].start()

		for test_case in test_cases:
			run_backend(options.output_dir, test_case)

		# make the threads exit
		for w in range(options.workers): backend_queue.put(None)
		for w in range(options.workers): t[w].join()

	# Execution of the benchmatks is done using 1 worker
	if options.execute is True:
		ldPath = os.getenv('LD_LIBRARY_PATH', '')
		os.putenv('LD_LIBRARY_PATH', ldPath +
			':{INSIEME_RUNTIME_BUILD}/pmlib'.format(
				**{
					'INSIEME_RUNTIME_BUILD': INSIEME_RUNTIME_BUILD
				}))
		run(options.output_dir, test_cases, options.runs)
		os.putenv('LD_LIBRARY_PATH', ldPath)
	
	#plot_regions(options.output_dir, test_cases)
	if options.analyze is True:
		analyze(options.output_dir, test_cases)

if __name__ == "__main__":
	sys.exit(main())
