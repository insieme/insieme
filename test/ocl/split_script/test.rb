$script_dir = 'split_script' 
$database_name = 'database.db' 

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
      init_db
      @checks.each_index{ |j|
        check_run(@test_names[i], @checks[j])
      }
      @sizes.each_index{ |k|
	puts
	puts "* -> " + "#{@sizes[k]}".light_blue
        update_features_db_tables @sizes[k]
        @splits.each_index{ |j|
          split_run(@test_names[i], @splits[j], @sizes[k], @iterations, j)
        }
        puts " * "+ "Best Configuration".light_blue + " "*20 + " #{@splits[$best_index]} ".light_blue + "  " +  
             "[#{(($best_timer/iterations)/1_000_000_000.0).round(4).to_s}]".light_blue
       update_measurement_db_tables  
      }
      puts
    }
  end

  private # utility functions
  def check_run (test_name, split_values)
    print " * Testing OpenCL program with splitting: #{split_values}\t  "
    correct = true;
    ENV['IRT_OCL_SPLIT_VALUES'] = split_values
    correct = false if !correct? "#{test_name}.ocl.test -check"
    if correct
      puts "Success".green
    else
      puts "Fail".red
    end
  end

  def split_run (test_name, split_values, size, iterations, split_index)
    print " * Running OpenCL program with splitting: #{split_values}\t  "
    timer = 0;
    iterations.times{
      ENV['IRT_OCL_SPLIT_VALUES'] = split_values
      `./#{test_name}.ocl.test -size #{size}` 
      t = get_result
      timer += t
    }
    puts "[" + ((timer/iterations)/1_000_000_000.0).round(4).to_s + "]  "
    
    if ((split_index == 0) || (timer < $best_timer))
      $best_timer = timer
      $best_index = split_index
    end
  end

  def get_result
    first = `cat worker_event_log.000* | grep WI | sort -k4 | head -n 1 | awk -v x=4 '{print $x }'`
    last = `cat worker_event_log.000* | grep WI | sort -k4 | tail -n 1 | awk -v x=4 '{print $x }'`
    last.to_i - first.to_i
  end

  def print_check (flag)
    if flag
      puts " * ->" + " Success.".green
    else
      puts " * ->" + " Fail.".red; puts $cmd; exit;
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

  def verbose?
    (ARGV[0] == "--verbose") ? true : false
  end

  def compile_and_run test_name
    puts "#####################################"
    puts "#####         " + test_name.light_blue.center(24) + "        #####"
    puts "#####################################"
    #inside the test dir
    Dir.chdir($path + test_name)
    File.delete("#{test_name}.insieme.ocl.c") if File.exist?("#{test_name}.insieme.ocl.c")
    puts " * Running Compiler => OCL..."
    cmd = "#{$main_dir}/main --std=c99 -I. -DINSIEME -I. -I../../ocl/common/ -I../../../code/frontend/test/inputs --opencl #{test_name}.c -b ocl:kernel.dat -o #{test_name}.insieme.ocl.c"
    verbose? ? `#{cmd}` : `#{cmd} 2> /dev/null`
    exist? "#{test_name}.insieme.ocl.c"

    File.delete("#{test_name}.ref") if File.exist?("#{test_name}.ref")
    puts " * Compiling C input..."
    cmd = "gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -o #{test_name}.ref #{test_name}.c -lm -lpthread -lrt -D_POSIX_C_SOURCE=199309 ../../ocl/common/lib_icl.c ../../ocl/common/lib_icl_ext.c ../../ocl/common/lib_icl_bmp.c -I$OPENCL_ROOT/include  -I../../ocl/common/ -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL"
    verbose? ? `#{cmd}` : `#{cmd} 2> /dev/null`
    exist? "#{test_name}.ref"
    
    File.delete("#{test_name}.ocl.test") if File.exist?("#{test_name}.ocl.test")
    puts " * Compiling generated OCL output..."
    cmd = "gcc -fshow-column -Wall -pipe -g --std=c99 -I. -I../../../code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o #{test_name}.ocl.test #{test_name}.insieme.ocl.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../ocl/common/lib_icl.c ../../ocl/common/lib_icl_ext.c ../../ocl/common/lib_icl_bmp.c -I$OPENCL_ROOT/include  -I../../ocl/common/ -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL"
    verbose? ? `#{cmd}` : `#{cmd} 2> /dev/null`
    exist? "#{test_name}.ocl.test"

    puts " * Running input program..."
    print_check correct? "#{test_name}.ref"

    puts " * Running OCL program..."
    print_check correct? "#{test_name}.ocl.test"

    puts " * Extracting the static Features from the kernel..."
    `mkdir #{$path}/database/` if !File.directory?("#{$path}/database/")
    # with -c create a clean database every time... change it
    cmd = "#{$main_dir}/genDB kernel.dat -c -u cid.txt -fSCF_NUM_integer_all_OPs_real -fSCF_NUM_integer_all_VEC_OPs_real -fSCF_NUM_real*_all_OPs_real -fSCF_NUM_real*_all_VEC_OPs_real -fSCF_NUM_externalFunction_lambda_real -fSCF_NUM_barrier_Calls_real -fSCF_IO_NUM_any_read/write_OPs_real -fSCF_COMP_localMemoryAccesses-allMemoryAccesses_real_ratio -fSCF_COMP_allOPs-memoryAccesses_real_2:1ratio -fSCF_COMP_scalarOPs-vectorOPs_real_sum -o #{$path}/database/#{$database_name}"
    verbose? ? `#{cmd}` : `#{cmd} 2> /dev/null`    
    exist? "kernel.dat"
 end

  def init_db
    if (!defined? $db)
      $db = Sequel.sqlite("#{$path}/database/#{$database_name}")
      $table_dynamic = $db[:dynamic_features]
      $table_setup = $db[:setup]
      $table_measurement = $db[:measurement]
      $table_static = $db[:static_features]
      $table_code = $db[:code]
    end
  end

  def update_features_db_tables size
      puts " * Extracting the dynamic Features from the program..."
      # generate the dynamic features name if not in the DB
      features = [
       'splittable_write_transfer',
       'unsplittable_write_transfer',
       'splittable_read_transfer',
       'unsplittable_read_transfer',
       'size',
       'splittable_write_transfer_per_computation',
       'unsplittable_write_transfer_per_computation',
       'splittable_read_transfer_per_computation',
       'unsplittable_read_transfer_per_computation']

      features.each{ |feature|
        if ($table_dynamic.filter(:name => feature).count == 0)
          $table_dynamic.insert(:name => feature)
        end
      }

      # read the dynamic features from dataToTransfer.txt
      values = `cat dataToTransfer.txt`.split # read values from file
      values[4] = size.to_s #size
      values.collect!{ |value|
        tmp = value.gsub(/sizeof/, '').gsub(/size/, size.to_s).gsub(/[^+*-\/\d]/,'')
        eval("#{tmp}")
      }

      # find the static features SCF_COMP_scalarOPs-vectorOPs_real_sum and rewrite as a dynamic features
      cid = `cat cid.txt`.split[1] # read values from file
      fid = $table_static.filter(:name => "SCF_COMP_scalarOPs-vectorOPs_real_sum").select(:id).single_value
      op_value = $table_code.filter(:cid => cid, :fid => fid).select(:value).single_value
      values[5] = values[0]/op_value
      values[6] = values[1]/op_value
      values[7] = values[2]/op_value
      values[8] = values[3]/op_value
      # insert the dynamic features values in the 'setup' table
      $table_setup.select(:sid).count == 0 ? $sid = 1 : $sid = $table_setup.select(:sid).order(:sid).last[:sid] + 1
      features.zip(values).each do |name, value|
        fid =  $table_dynamic.filter(:name => name).select(:id).single_value
        $table_setup.insert(:sid => $sid, :fid => fid, :value => value)
      end 
  end

  def update_measurement_db_tables
    cid = `cat cid.txt`.split[1] # read values from file
    $table_measurement.insert(:cid => cid, :sid => $sid, :time => $best_index) 
  end

end

## end of the test class
#######################################################################################################

def print_devices
  Dir.chdir($path + $script_dir)
  `gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -I../../../code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o dev.ref devices_info.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../ocl/common/lib_icl.c -I$OPENCL_ROOT/include -I../../ocl/common/ -L$OPENCL_ROOT/lib/x86_64 -lOpenCL`
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
  `gem install -i #{$lib_dir}gem sequel`   if !File.directory?("#{$lib_dir}/gem/gems/sequel-3.34.1/")
  `gem install -i #{$lib_dir}gem sqlite3 -- --with-sqlite3-dir=#{$lib_dir}/sqlite-latest/` if !File.directory?("#{$lib_dir}/gem/gems/sqlite3-1.3.5/")
  require 'colorize'
  require 'sequel'
end


def initialize_env
  host = `hostname`.strip
  if (host == "mc2" || host == "mc3" || host == "mc4") 
    $main_dir = '/software-local/insieme_build/code/driver/'
    $lib_dir =  '/software-local/insieme-libs/'
    ENV['OPENCL_ROOT'] = '/software/AMD/AMD-APP-SDK-v2.6-RC3-lnx64/'
    ENV['LD_LIBRARY_PATH'] = ["/software/AMD/AMD-APP-SDK-v2.6-RC3-lnx64/lib/x86_64/", ENV['LD_LIBRARY_PATH'], ].join(':')
  end

  if (host == "SandyBridge")
    $main_dir = '/home/sh4dow/insieme/build_all/code/driver/'
    $lib_dir =  '/home/sh4dow/libs/'
  end

 $path =  Dir.pwd.gsub!($script_dir, '')
  
  # set PATH and LD_LIBRARY_PATH
  ENV['LD_LIBRARY_PATH'] = [
    "#{$lib_dir}/gcc-latest/lib64",
    "#{$lib_dir}/mpfr-latest/lib",
    "#{$lib_dir}/mpc-latest/lib",
    "#{$lib_dir}/gmp-latest/lib",
    "#{$lib_dir}/cloog-gcc-latest/lib",
    "#{$lib_dir}/ppl-latest/lib",
    "#{$lib_dir}/sqlite-latest/lib",
    ENV['LD_LIBRARY_PATH'],
  ].join(':')

  ENV['PATH'] = [
    "#{$lib_dir}/gcc-latest/bin/", 
    "#{$lib_dir}/ruby-latest/bin/", 
    ENV['PATH'], 
  ].join(':')

  ENV['IRT_INST_WORKER_EVENT_LOGGING'] = "true"
   
  install_gems
end

=begin #debug print of all the tables
puts $table_static.all
puts
puts $table_code.all
puts
puts $table_dynamic.all
puts
puts $table_setup.all
puts
puts $table_measurement.all
=end
############################################################################################
initialize_env # add in this function the correct path for each machine
print_devices

all_2dev = ["1.0, 0.0", "0.9, 0.1", "0.8, 0.2",   # splits
           "0.7, 0.3", "0.6, 0.4", "0.5, 0.5",
           "0.4, 0.6", "0.3, 0.7", "0.2, 0.8",
           "0.1, 0.9", "0.0, 1.0"]

all_3dev = ["1.0, 0.0, 0.0", 
            "0.9, 0.1, 0.0", "0.9, 0.05, 0.05", "0.8, 0.2, 0.0", "0.8, 0.1, 0.1",
            "0.7, 0.3, 0.0", "0.7, 0.15, 0.15", "0.6, 0.4, 0.0", "0.6, 0.2, 0.2",
            "0.5, 0.5, 0.0", "0.5, 0.25, 0.25", "0.4, 0.6, 0.0", "0.4, 0.3, 0.3",
            "0.3, 0.7, 0.0", "0.3, 0.35, 0.35", "0.2, 0.8, 0.0", "0.2, 0.4, 0.4",
            "0.1, 0.9, 0.0", "0.1, 0.45, 0.45", "0.0, 1.0, 0.0", "0.0, 0.5, 0.5"]

power = (7..32).to_a.map{ |x| 2**x }

=begin
test_all = Test.new(2,					# devices
		all_2dev,				# splits
                ["0.4, 0.6", "0.6, 0.4"],		# checks
                ["simple", "vec_add", "mat_mul"],	# tests name 
                [128, 128000, 1280000],			# sizes  
                3					# iterations 
                )
test_all.print_conf
test_all.run
=end

=begin
test2 = Test.new(2,                                     # devices
                ["1.0, 0.0", "0.0, 1.0"],               # splits
                ["1.0, 0.0"],               			# check
                ["simple"],                 # tests name 
                [128],                  		# sizes  
                3                                       # iterations 
                )
=end
test2 = Test.new(3,                                     # devices
                all_3dev,               # splits
                ["0.3, 0.3, 0.4"],                                   # check
                ["n_body"],                 # tests name 
                power,                                  # sizes  
                5                                       # iterations 
                )
test2.print_conf
test2.run

