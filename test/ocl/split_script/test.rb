class Test
  attr_accessor :num_devs, :splits, :checks, :test_names, :sizes, :iterations

  def initialize(num_devs, splits, checks, test_names, sizes, iterations)
    @num_devs = num_devs
    @splits = splits
    @checks = checks
    @test_names = test_names
    @sizes = sizes
    @iterations = iterations
  end

  def print_conf
    puts  "#####################################"
    puts  "#####     " + "Test Configuration".light_blue + "    #####"
    puts  "#####################################"
    puts  "devices    = #{@num_devs}"
    print "splits     = "; @splits.each_index{|i| print "[#{@splits[i]}] "}; puts;
    print "checks     = "; @checks.each_index{|i| print "[#{@checks[i]}] "}; puts;
    print "tests      = "; @test_names.each_index{|i| print "#{@test_names[i]}  "}; puts;
    print "sizes      = "; @sizes.each_index{|i| print "#{@sizes[i]}  "}; puts;
    puts  "iterations = #{@iterations}"; puts;
  end

  def run
    ENV['IRT_NUM_WORKERS'] = @num_devs.to_s;
    @test_names.each_index{ |i|
      compile_and_run(@test_names[i])
      @checks.each_index{ |j|
        check_run(@test_names[i], @checks[j])
      }
      @splits.each_index{ |j|
        split_run(@test_names[i], @splits[j], @iterations)
      }
      puts
    }
  end

  private # utility functions
  def check_run (test_name, split_values)
    print " * Testing OpenCL program with splitting: #{split_values}\t  "
    correct = true;
    ENV['IRT_OCL_SPLIT_VALUES'] = split_values
    correct = false if !correct? "#{test_name}.ocl.test"
    if correct
      puts "Success".green
    else
      puts "Fail".red
    end
  end

  def split_run (test_name, split_values, iterations)
    print " * Running OpenCL program with splitting: #{split_values}\t  "
    timer = 0;
    iterations.times{
      ENV['IRT_OCL_SPLIT_VALUES'] = split_values
      `./#{test_name}.ocl.test` 
      t = get_result
      timer += t
    }
    puts "[" + ((timer/iterations)/1_000_000_000.0).round(4).to_s + "]  "
  end

  def get_result
    file_name = "worker_event_log.0000"
    first = `head -n 1 #{file_name} | awk -v x=4 '{print $x }'`
    last  = `tail -n 1 #{file_name} | awk -v x=4 '{print $x }'`
    last.to_i - first.to_i
  end

  def print_check (flag)
    if flag
      puts " * ->" + " Success.".green
    else
      puts " * ->" + " Fail.".red; exit;
    end
  end

  def exist? (file_name)
    print_check File.exist?(file_name)
  end

  def correct? (exe_name)
    `./#{exe_name}  > file.tmp`
    last = `tail -n 1 file.tmp`
    `rm file.tmp`
    last =~ /OK/
  end

  def compile_and_run test_name
    puts "#####################################"
    puts "#####         " + test_name.light_blue + "        #####"
    puts "#####################################"
    #inside the test dir
    Dir.chdir($path + test_name)
    File.delete("#{test_name}.insieme.ocl.c") if File.exist?("#{test_name}.insieme.ocl.c")
    puts " * Running Compiler => OCL..."
    `#{$main_dir}/main --std=c99 -I. -DINSIEME -I. -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs --opencl #{test_name}.c -b ocl -o #{test_name}.insieme.ocl.c 2> /dev/null`
    exist? "#{test_name}.insieme.ocl.c"

    File.delete("#{test_name}.ref") if File.exist?("#{test_name}.ref")
    puts " * Compiling C input..."
    `gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -o #{test_name}.ref #{test_name}.c -lm -lpthread -lrt -D_POSIX_C_SOURCE=199309 ../../../code/backend/test/ocl_kernel/lib_icl.c -I$OPENCL_ROOT/include  -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> /dev/null`
    exist? "#{test_name}.ref"

    File.delete("#{test_name}.ocl.test") if File.exist?("#{test_name}.ocl.test")
    puts " * Compiling generated OCL output..."
    `gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -I../../../code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o #{test_name}.ocl.test #{test_name}.insieme.ocl.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../../code/backend/test/ocl_kernel/lib_icl.c -I$OPENCL_ROOT/include  -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> /dev/null`
    exist? "#{test_name}.ocl.test"

    puts " * Running input program..."
    print_check correct? "#{test_name}.ref"

    puts " * Running OCL program..."
    print_check correct? "#{test_name}.ocl.test"
  end

end

## end of the test class
#######################################################################################################

def print_devices
  Dir.chdir($path + $script_dir)
  `gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -I/home/sh4dow/insieme/code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o dev.ref devices_info.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../../code/backend/test/ocl_kernel/lib_icl.c -I$OPENCL_ROOT/include  -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> /dev/null`

  # execute the dev infos program and print information
  puts "#####################################"
  puts "### " +  "Devices present in the System".light_blue + " ###"
  puts "#####################################"
  dev = `./dev.ref`
  print dev.magenta
  puts "#####################################\n\n"
  #dev.split('\n').size
end


def install_gems
  # needed ruby gems
  ENV['GEM_PATH'] = "#{$lib_dir}/gem/"
  `mkdir #{$lib_dir}/gem/` if !File.directory?("#{$lib_dir}/gem/")
  `gem install -i #{$lib_dir}gem colorize` if !File.directory?("#{$lib_dir}/gem/gems/colorize-0.5.8/")
  require 'colorize'
end


def initialize_env
  host = `hostname`.strip
  if (host == "mc2") 
    $main_dir = '/software-local/insieme_build/code/driver/'
    $lib_dir =  '/software-local/insieme-libs/'
  end

  if (host == "SandyBridge")
    $main_dir = '/home/sh4dow/insieme/build_all/code/driver/'
    $lib_dir =  '/home/sh4dow/libs/'
  end

  $script_dir = 'split_script' # don't change
  $path =  Dir.pwd.gsub!($script_dir, '')
  
  # set PATH and LD_LIBRARY_PATH
  ENV['LD_LIBRARY_PATH'] = [
    "#{$lib_dir}/gcc-latest/lib64",
    "#{$lib_dir}/mpfr-latest/lib",
    "#{$lib_dir}/mpc-latest/lib",
    "#{$lib_dir}/gmp-latest/lib",
    "#{$lib_dir}/cloog-gcc-latest/lib",
    "#{$lib_dir}/ppl-latest/lib",
    ENV['LD_LIBRARY_PATH'],
  ].join(':')

  ENV['PATH'] = [ 
    "#{$lib_dir}/gcc-latest/bin/", 
    "#{$lib_dir}/ruby-latest/bin/", 
    ENV['PATH'], 
  ].join(':')
  
  install_gems
end


############################################################################################

initialize_env # add in this function the correct path for each machine
print_devices

all_2dev = ["1.0, 0.0", "0.9, 0.1", "0.8, 0.2",   # splits
           "0.7, 0.3", "0.6, 0.4", "0.5, 0.5",
           "0.4, 0.6", "0.3, 0.7", "0.2, 0.8",
           "0.1, 0.9", "0.0, 1.0"]


=begin
test1 = Test.new(2, 					# devices
		["1.0, 0.0", "0.5, 0.5", "0.0, 1.0"], 	# splits
		["0.4, 0.6", "0.7, 0.3"],		# checks
		["vec_add", "mat_mul"], 		# tests name 
		[128, 256, 512],			# sizes  
		3 					# iterations 
		)
test1.print_conf
test1.run
=end

test2 = Test.new(2,                                     # devices
		all_2dev,				# splits
                ["0.4, 0.6", "0.7, 0.3"],               # checks
                ["vec_add", "mat_mul"],                 # tests name 
                [128, 256, 512],                        # sizes  
                3                                       # iterations 
                )
test2.print_conf
test2.run

t2 = Test.new(2,                                     # devices
                all_2dev,                               # splits
                ["0.4, 0.6", "0.7, 0.3"],               # checks
                ["vec_add", "mat_mul"],                 # tests name 
                [128, 256, 512],                        # sizes  
                3                                       # iterations 
                )
test2.print_conf
test2.run
