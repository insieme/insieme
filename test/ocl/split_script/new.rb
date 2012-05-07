$script_dir = 'split_script'
$db_ml_name = 'database.db'
$db_run_name = 'result.db'

# add simple statistics to array
module Enumerable
  def sum
    self.inject(0){|accum, i| accum + i }
  end

  def average
    self.sum/self.length.to_f
  end

  def median
    len = self.length
    sorted = self.sort
    len % 2 == 1 ? sorted[len/2] : (sorted[len/2 - 1] + sorted[len/2]).to_f / 2
  end

  def sample_variance
    m = self.average
    sum = self.inject(0){|accum, i| accum +(i-m)**2 }
    sum/(self.length - 1).to_f
  end

  def standard_deviation
    Math.sqrt(self.sample_variance).to_i
  end
end

def set_standard_path
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
end

def install_gems
  # needed ruby gems
  ENV['GEM_PATH'] = "#{$lib_dir}/gem/"
  ENV['R_HOME'] = "/usr/lib/R"
  ENV['LD_LIBRARY_PATH'] = ["RHOME/bin", ENV['LD_LIBRARY_PATH'], ].join(':')
  `mkdir #{$lib_dir}/gem/` if !File.directory?("#{$lib_dir}/gem/")
  gem_names = ["colorize", "sequel", "sqlite3", "rsruby"]
  gem_names.each do |name|
    if  Dir["#{$lib_dir}/gem/gems/#{name}*"] == []
      print "** Installing gem: #{name}"
      if name == "sqlite3"
        `gem install -i #{$lib_dir}gem sqlite3 -- --with-sqlite3-dir=#{$lib_dir}/sqlite-latest/ 2> file.tmp`
      elsif name == "rsruby"
        `gem install -i #{$lib_dir}gem rsruby -- --with-R-dir=/usr/lib/R --with-R-include=/usr/share/R/include 2> file.tmp`
      else
        `gem install -i #{$lib_dir}gem #{name} 2> file.tmp`
      end
      require 'colorize'
      err = `cat file.tmp`
      if err == ""
        puts "\t [" + "DONE".green + "]" #FIXME reuse
        `rm file.tmp`
      else
        puts "\t [" + "FAIL".red + "]"
        puts err
        `rm -r #{$lib_dir}/gem/gems/#{name}*`
        `rm file.tmp`
        exit
      end
    end
  end
  require 'colorize'
  require 'sequel'
  require 'rsruby'
end

def initialize_env
  host = `hostname`.strip

  if (host == "mc2" || host == "mc3" || host == "mc4")
    $main_dir = '/software-local/insieme_build/code/driver/'
    $lib_dir =  '/software-local/insieme-libs/'
    ENV['OPENCL_ROOT'] = '/software/AMD/AMD-APP-SDK-v2.6-RC3-lnx64/'
    ENV['LD_LIBRARY_PATH'] = ["/software/AMD/AMD-APP-SDK-v2.6-RC3-lnx64/lib/x86_64/", ENV['LD_LIBRARY_PATH'], ].join(':')
    set_standard_path
  end

  if (host == "mithril")
    $main_dir = '/home/sh4dow/insieme/build_all/code/driver/'
    $lib_dir =  '/home/sh4dow/libs/'
    ENV['OPENCL_ROOT'] = "#{$lib_dir}/opencl-latest/"
    ENV['LD_LIBRARY_PATH'] = ["#{$lib_dir}/opencl-latest/lib/x86_64/", ENV['LD_LIBRARY_PATH'], ].join(':')
    set_standard_path
  end
  
  ENV['IRT_INST_WORKER_EVENT_LOGGING'] = "true"
  $path =  Dir.pwd.gsub!($script_dir, '')
  install_gems
end

######################################################################
######################## Test Class ##################################
######################################################################

class Test
  attr_accessor :num_devs, :devices, :splits, :checks, :test_names, :sizes, :iterations

  def initialize(splits, checks, tests, sizes, iterations)
    @devices = get_devices
    @num_devs = @devices.size
    ENV['IRT_NUM_WORKERS'] = @num_devs.to_s;

    @splits = []; splits.each{|n| value = $split[n-1][@num_devs-1]; @splits << value if value != nil}
    @checks = []; checks.each{|n| value = $split[n-1][@num_devs-1]; @checks << value if value != nil}
    @test_names = []; tests.each{|n| value = $program[n-1]; @test_names << value if value != nil}
    @sizes = sizes
    @iterations = iterations
  end

  def info
    puts
    puts "#####################################"
    puts "### " +  "Devices present in the System".light_blue + " ###"
    puts "#####################################"
    @devices.each{|name| puts name.magenta}
    puts "#####################################"
    puts
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

  def compile
    puts "#####################################"
    puts "#####     " + "Compilation Phase".light_blue + "     #####"
    puts "#####################################"

    @test_names.each{ |test_name|
      Dir.chdir($path + test_name)
      File.delete("#{test_name}.insieme.ocl.c") if File.exist?("#{test_name}.insieme.ocl.c")
      puts " * #{test_name}".light_blue 
      puts " * Running Compiler => OCL..."
      cmd = "#{$main_dir}/main --std=c99 -I. -DINSIEME -I. -I../../ocl/common/ -I../../../code/frontend/test/inputs --opencl #{test_name}.c -b ocl:kernel.dat -o #{test_name}.insieme.ocl.c 2> file.tmp"
      `#{cmd}` 
      exist? "#{test_name}.insieme.ocl.c", cmd 

      File.delete("#{test_name}.ref") if File.exist?("#{test_name}.ref")
      puts " * Compiling C input..."
      cmd = "gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -o #{test_name}.ref #{test_name}.c -lm -lpthread -lrt -D_POSIX_C_SOURCE=199309 ../../ocl/common/lib_icl.c ../../ocl/common/lib_icl_ext.c ../../ocl/common/lib_icl_bmp.c -I$OPENCL_ROOT/include  -I../../ocl/common/ -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> file.tmp"
      `#{cmd}` 
      exist? "#{test_name}.ref", cmd

      File.delete("#{test_name}.ocl.test") if File.exist?("#{test_name}.ocl.test")
      puts " * Compiling generated OCL output..."
      cmd = "gcc -fshow-column -Wall -pipe -g --std=c99 -I. -I../../../code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o #{test_name}.ocl.test #{test_name}.insieme.ocl.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../ocl/common/lib_icl_ext.c ../../ocl/common/lib_icl_bmp.c -I$OPENCL_ROOT/include  -I../../ocl/common/ -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> file.tmp"
      `#{cmd}` 
      exist? "#{test_name}.ocl.test", cmd

      puts " * Running input program..."
      print_check correct? "#{test_name}.ref"

      puts " * Running OCL program..."
      print_check correct? "#{test_name}.ocl.test"
      puts
    }
  end

  def check
    puts "#####################################"
    puts "#####        " + "Check Phase".light_blue + "        #####"
    puts "#####################################"

    @test_names.each do |test_name|
      Dir.chdir($path + test_name)
      puts " * #{test_name}".light_blue
      @checks.each_index do |i|
        split_values = @checks[i]
        spaces = 20-split_values.size
        print " * Testing OpenCL program with splitting:  " + "#{split_values}" + (" " * spaces)
        correct = true;
        ENV['IRT_OCL_SPLIT_VALUES'] = split_values
        print_check correct? "#{test_name}.ocl.test -check"
      end
    end
    puts
  end

  def run
    puts "#####################################"
    puts "#####         " + "Run Phase".light_blue + "         #####"
    puts "#####################################"
    init_db_run
    datetime = DateTime.now
    @test_names.each do |test_name|
      Dir.chdir($path + test_name)
      @sizes.each do |size|
        @iterations.times do |n|
          @splits.each_index { |i| single_run datetime, test_name, size, n, i, 0 }
        end
      end
    end
    puts
  end

  def view
    puts "#####################################"
    puts "#####         " + "View Phase".light_blue + "        #####"
    puts "#####################################"
    init_db_run
    t_run = 0; c_run = 0; m_run = 0; n_run = 0; 
    @test_names.each do |test_name|
      @sizes.each do |size|
        puts " * #{test_name} - #{size}".light_blue
        best_split = 0; best_time = 0;
        @splits.each_index do |i|
          t_run += 1
          split_values = @splits[i]
          spaces = 20-split_values.size
          str = " * OpenCL program view with splitting:  #{split_values}" + (" " * spaces)
          time_array = $db_run[:runs].filter(:test_name => test_name, :size => size, :split => split_values).select(:time).all.map!{|n| n[:time]}
          ar_size = time_array.size
          if ar_size < @iterations
            puts ar_size != 0 ? str << "[" + "ONLY #{time_array.size} RUN".yellow + "]" : str << "[" + "NOT PRESENT".yellow + "]" 
            m_run += 1
          else
            if best_time > time_array.average || best_time == 0
              best_split = i
              best_time = time_array.average
            end
            str << "[" + (time_array.average/1_000_000_000.0).round(4).to_s + "]  "
            not_relevant = !t_test_correct?(time_array)
            str <<  "[" + "NOT RELEVANT".red + "]" if not_relevant
            puts str
            time_array.each_index{|i| puts " * " "#{i+1}: #{time_array[i]}".yellow } if not_relevant
            n_run += 1 if not_relevant
            c_run += 1 if !not_relevant
          end
        end
        spaces = 20-@splits[best_split].size
        puts " * "+ "Best Configuration".light_blue + " "*19 + "#{@splits[best_split]}".light_blue + (" " * spaces) +
             "[#{(best_time/1_000_000_000.0).round(4).to_s}]".light_blue
        puts
      end
    end
    puts
    puts "#####################################"
    puts "#####       " + "View Summary".light_blue + "        #####"
    puts "#####################################"
    puts " * Total Runs   \t [#{t_run}] \n * Correct Runs \t [#{c_run}] \n * Not Complete \t [#{m_run}] \n * Not Relevant \t [#{n_run}]"
    puts t_run == c_run ? " * -> " + "TEST COMPLETE".green : " * -> " + "TEST NOT COMPLETE".red 
    puts
  end

  def fix
    puts "#####################################"
    puts "#####         " + "Fix Phase".light_blue + "        #####"
    puts "#####################################"
    init_db_run
    datetime = DateTime.now
    @test_names.each do |test_name|
      Dir.chdir($path + test_name)
      @sizes.each do |size|
        puts " * #{test_name} - #{size}".light_blue
        @splits.each_index do |i|
          split_values = @splits[i]
          time_array = $db_run[:runs].filter(:test_name => test_name, :size => size, :split => split_values).select(:time).all.map!{|n| n[:time]}
          if time_array.size < @iterations
            (@iterations - time_array.size).times{|n| single_run datetime, test_name, size, n, i, 1}
          else
            if !t_test_correct?(time_array) 
              $db_run[:runs].filter(:test_name => test_name, :size => size, :split => split_values).delete
              @iterations.times{|n| single_run datetime, test_name, size, n, i, 1}
            end
          end
        end
      end
    end
    puts
  end

  def fake
    puts "#####################################"
    puts "#####         " + "Fake Phase".light_blue + "       #####"
    puts "#####################################"
    init_db_run
    datetime = DateTime.now
    @test_names.each do |test_name|
      @sizes.each do |size|
        @splits.each_index do |i|
          split_values = @splits[i]
          time_array = $db_run[:runs].filter(:test_name => test_name, :size => size, :split => split_values).select(:time).all.map!{|n| n[:time]}
          if !t_test_correct?(time_array)
            $db_run[:runs].filter(:test_name => test_name, :size => size, :split => split_values).delete
            time = time_array.median
            puts " * #{test_name}".light_blue + "  size #{size}  with splitting:  #{split_values}  => Inserting the median"
            @iterations.times{ time += 1; $db_run[:runs].insert(:test_name => test_name, :size => size, :split => split_values, :time => time, :timestamp => datetime) }
          end
        end
      end
    end
    puts
  end

  def collect
    puts "#####################################"
    puts "#####        " + "Collect Phase".light_blue + "      #####"
    puts "#####################################"
    init_db_run
    init_db_ml
    `mkdir #{$path}/database/` if !File.directory?("#{$path}/database/")
    @test_names.each do |test_name|
      puts " * Extracting the static Features from #{test_name} kernel..."
      Dir.chdir($path + test_name)
      # with -c create a clean database every time... change it
      cmd = "#{$main_dir}/genDB kernel.dat -c -u cid.txt -fSCF_NUM_integer_all_OPs_real -fSCF_NUM_integer_all_VEC_OPs_real -fSCF_NUM_real*_all_OPs_real -fSCF_NUM_real*_all_VEC_OPs_real -fSCF_NUM_externalFunction_lambda_real -fSCF_NUM_barrier_Calls_real -fSCF_IO_NUM_any_read/write_OPs_real -fSCF_COMP_localMemoryAccesses-allMemoryAccesses_real_ratio -fSCF_COMP_allOPs-memoryAccesses_real_2:1ratio -fSCF_COMP_scalarOPs-vectorOPs_real_sum -o #{$path}/database/#{$db_ml_name} 2> file.tmp"
      `#{cmd}`
      exist? "kernel.dat", cmd

      @sizes.each do |size|
        best_split = 0; best_time = 0;
        @splits.each_index do |i| 
          split_values = @splits[i]
          time_array = $db_run[:runs].filter(:test_name => test_name, :size => size, :split => split_values).select(:time).all.map!{|n| n[:time]}
          if best_time > time_array.average || best_time == 0
            best_split = i 
            best_time = time_array.average
          end 
        end 
        #update db
        update_features_db_ml size, best_split
      end
    end
  end

# utility functions
private
  def get_devices
    Dir.chdir($path + $script_dir)
    `gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -I../../../code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o dev.ref devices_info.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../ocl/common/lib_icl.c -I$OPENCL_ROOT/include -I../../ocl/common/ -L$OPENCL_ROOT/lib/x86_64 -lOpenCL`
    # execute the dev infos program and print information
    `./dev.ref`.split("\n")
  end

  def exist? file_name, cmd
    if File.exist?(file_name)
       puts " * ->" + " Success.".green
    else
      puts " * ->" + " Fail.".red;
      puts `cat file.tmp`;
      puts
      puts "*********** Command ************"
      puts cmd
      exit;
    end
  end

  def correct? (exe_name)
    `./#{exe_name}  > file.tmp`
    last = `tail -n 1 file.tmp`
    `rm file.tmp`
    last =~ /OK/
  end

  def print_check (flag)
    if flag
      puts " * ->" + " Success.".green
    else
      puts " * ->" + " Fail.".red; puts $cmd; exit;
    end
  end

  def get_result
    first = `cat worker_event_log.000* | grep WI | sort -k4 | head -n 1 | awk -v x=4 '{print $x }'`
    last = `cat worker_event_log.000* | grep WI | sort -k4 | tail -n 1 | awk -v x=4 '{print $x }'`
    last.to_i - first.to_i
  end

  def init_db_run
    if (!defined? $db_run)
      $db_run = Sequel.sqlite("#{$path}/database/#{$db_run_name}")
      if !$db_run.table_exists? :runs
        $db_run.create_table :runs do
          primary_key :id
          String   :test_name
          Fixnum   :size
          String   :split
          Bignum   :time
          DateTime :timestamp
        end
      end
    end
  end

  def init_db_ml
    if (!defined? $db_ml)
      $db_ml = Sequel.sqlite("#{$path}/database/#{$db_ml_name}")
      $table_dynamic = $db_ml[:dynamic_features]
      $table_setup = $db_ml[:setup]
      $table_measurement = $db_ml[:measurement]
      $table_static = $db_ml[:static_features]
      $table_code = $db_ml[:code]
    end
  end

  def update_features_db_ml size, best_split
    print " * Extracting the dynamic Features for size #{size} "
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

    features.each do |feature|
      if ($table_dynamic.filter(:name => feature).count == 0)
        $table_dynamic.insert(:name => feature)
      end
    end

    # read the dynamic features from dataToTransfer.txt
    values = `cat dataToTransfer.txt`.split # read values from file
    values[4] = size.to_s #size
    values.collect! do |value|
      tmp = value.gsub(/sizeof/, '').gsub(/size/, size.to_s).gsub(/[^+*-\/\d]/,'')
      eval("#{tmp}")
    end

    # find the static features SCF_COMP_scalarOPs-vectorOPs_real_sum and rewrite as a dynamic features
    cid = `cat cid.txt`.split[1] # read values from file
    fid = $table_static.filter(:name => "SCF_COMP_scalarOPs-vectorOPs_real_sum").select(:id).single_value
    op_value = $table_code.filter(:cid => cid, :fid => fid).select(:value).single_value
    values[5] = values[0]/op_value
    values[6] = values[1]/op_value
    values[7] = values[2]/op_value
    values[8] = values[3]/op_value
    
    # insert the dynamic features values in the 'setup' table
    $table_setup.select(:sid).count == 0 ? sid = 1 : sid = $table_setup.select(:sid).order(:sid).last[:sid] + 1
    features.zip(values).each do |name, value|
      fid =  $table_dynamic.filter(:name => name).select(:id).single_value
      $table_setup.insert(:sid => sid, :fid => fid, :value => value)
    end
    $table_measurement.insert(:cid => cid, :sid => sid, :time => best_split)
    puts "\t [" + "DONE".green + "]"
  end

 def single_run datetime, test_name, size, n, i, print
    `rm worker_event_log* 2> /dev/null` #FIXME maybe
    `rm ocl_event_log* 2> /dev/null`
     split_values = @splits[i]
     ENV['IRT_OCL_SPLIT_VALUES'] = split_values
     print "\r * #{test_name}".light_blue + "  size: #{size}  iteration [#{n+1}/#{@iterations}]  split [#{i+1}/#{@splits.size}]" if print == 0
     print "\r * #{test_name}".light_blue + "  size: #{size}  split [#{i+1}/#{@splits.size}]  iteration [#{n+1}/#{@iterations}]" if print == 1
     `./#{test_name}.ocl.test -size #{size}`
     time = get_result
     $db_run[:runs].insert(:test_name => test_name, :size => size, :split => split_values, :time => time, :timestamp => datetime)
  end

  def t_test_correct? array
    # stat analysis
    r = RSRuby.instance # R in ruby
    test = r.t_test(array)
    return test['p.value'] < 0.05
  end

end

######################################################################
# utility array
$split = [["1.0", "1.0, 0.0", "1.0,  0.0,  0.0"], # 1
          [  nil, "0.9, 0.1", "0.9,  0.1,  0.0"], # 2
          [  nil, 	 nil, "0.9, 0.05, 0.05"], # 3
          [  nil, "0.8, 0.2", "0.8,  0.2,  0.0"], # 4
          [  nil,        nil, "0.8,  0.1,  0.1"], # 5
          [  nil, "0.7, 0.3", "0.7,  0.3,  0.0"], # 6
          [  nil,        nil, "0.7, 0.15, 0.15"], # 7
          [  nil, "0.6, 0.4", "0.6,  0.4,  0.0"], # 8
          [  nil,        nil, "0.6,  0.2,  0.2"], # 9
          [  nil, "0.5, 0.5", "0.5,  0.5,  0.0"], # 10
          [  nil,        nil, "0.5, 0.25, 0.25"], # 11
          [  nil, "0.4, 0.6", "0.4,  0.6,  0.0"], # 12
          [  nil,        nil, "0.4,  0.3,  0.3"], # 13
          [  nil, "0.3, 0.7", "0.3,  0.7,  0.0"], # 14
          [  nil,        nil, "0.3, 0.35, 0.35"], # 15
          [  nil, "0.2, 0.8", "0.2,  0.8,  0.0"], # 16
          [  nil,        nil, "0.2,  0.4,  0.4"], # 17
          [  nil, "0.1, 0.9", "0.1,  0.9,  0.0"], # 18
          [  nil,        nil, "0.1, 0.45, 0.45"], # 19
          [  nil, "0.0, 1.0", "0.0,  1.0,  0.0"], # 20
          [  nil,        nil, "0.0,  0.5,  0.5"], # 21
	]

$program = ["simple",		# 1
            "vec_add", 		# 2
            "mat_mul", 		# 3
	    "n_body",  		# 4
            "sobel_filter",] 	# 5

######################################################################
# Test arguments
# 0 int array, split to run (index)
# 1 int array, check to run (index)
# 2 int array, test  to run (index)
# 3 int array, size to run
# 4 int value, number of iteration

# initiliaze the path, the global variable
initialize_env

# create a test
size2 = (15..22).to_a.map{ |x| 2**x }
split2 = (1..21).to_a.delete_if{|x| x%2 != 0 && x != 1}

#test = Test.new(split2, [1, 4], [2, 3, 4], size2, 3)
test = Test.new(split2, [6, 14], [5], size2, 5)

# run the test
test.info
test.compile
test.check
#test.run
#test.fix
#test.fake
#test.view
#test.collect
