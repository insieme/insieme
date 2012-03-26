require 'colorize'

script_dir = 'split_script' # don't change
main_dir = '~/insieme/build_all/code/driver/' # change :)

$num_devices = '2';
$test_name = 'simple';
$iteration = 3;


def get_result
	file_name = "worker_event_log.0000"
	first = `head -n 1 #{file_name} | awk -v x=4 '{print $x }'`
	last  = `tail -n 1 #{file_name} | awk -v x=4 '{print $x }'`
	last.to_i - first.to_i
end


def run_test (split_values)
	print " * Running OpenCL program with splitting: #{split_values}\t  "
	timer = 0;
	correct = true;
	$iteration.times{
		ENV['IRT_OCL_SPLIT_VALUES'] = split_values
		correct = false if !correct? "#{$test_name}.ocl.test"
		t = get_result
		timer += t
	}
	print "[" + ((timer/$iteration)/1_000_000_000.0).round(4).to_s + "]  "
	if correct
		puts "Success".green
	else
		puts "Fail".red
	end
end

def print_check (flag)
	if flag
		puts " * ->" + " Success.".green
	else
		puts " * ->" + " Fail.".red;
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



ENV['IRT_NUM_WORKERS'] = $num_devices;
$path =  Dir.pwd.gsub!(script_dir, '')

# inside script dir
Dir.chdir($path + script_dir)
`gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -I/home/sh4dow/insieme/code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o dev.ref devices_info.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../../code/backend/test/ocl_kernel/lib_icl.c -I$OPENCL_ROOT/include  -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> /dev/null`

# execute the dev infos program and print information
puts "#####################################"
puts "### Devices present in the System ###"
`./dev.ref`
puts "#####################################\n\n"

puts "#####################################"
puts "#####         Test Fase         #####"
puts "#####################################"
#inside the test dir
Dir.chdir($path + $test_name)
puts "### #{$test_name}"
File.delete("#{$test_name}.insieme.ocl.c") if File.exist?("#{$test_name}.insieme.ocl.c")
puts " * Running Compiler => OCL..."
`#{main_dir}/main --std=c99 -I. -DINSIEME -I. -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs --opencl #{$test_name}.c -b ocl -o #{$test_name}.insieme.ocl.c 2> /dev/null`
exist? "#{$test_name}.insieme.ocl.c"

File.delete("#{$test_name}.ref") if File.exist?("#{$test_name}.ref")
puts " * Compiling C input..."
`gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -o #{$test_name}.ref #{$test_name}.c -lm -lpthread -lrt -D_POSIX_C_SOURCE=199309 ../../../code/backend/test/ocl_kernel/lib_icl.c -I$OPENCL_ROOT/include  -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> /dev/null`
exist? "#{$test_name}.ref"

File.delete("#{$test_name}.ocl.test") if File.exist?("#{$test_name}.ocl.test")
puts " * Compiling generated OCL output..."
`gcc -fshow-column -Wall -pipe -O3 --std=c99 -I. -I/home/sh4dow/insieme/code/runtime/include -D_XOPEN_SOURCE=700 -DUSE_OPENCL=ON -D_GNU_SOURCE -o #{$test_name}.ocl.test #{$test_name}.insieme.ocl.c -lm -lpthread -ldl -lrt -lOpenCL -D_POSIX_C_SOURCE=199309 ../../../code/backend/test/ocl_kernel/lib_icl.c -I$OPENCL_ROOT/include  -I../../../code/backend/test/ocl_kernel -I../../../code/frontend/test/inputs -L$OPENCL_ROOT/lib/x86_64 -lOpenCL 2> /dev/null`
exist? "#{$test_name}.ocl.test"

puts " * Running input program..."
print_check correct? "#{$test_name}.ref"

puts " * Running OCL program..."
print_check correct? "#{$test_name}.ocl.test"

if $num_devices == '2'
	11.times{ |i|
		`rm worker_event_log*`
		v1 = i/10.round(1)
		v2 = (1.0 - v1).round(1)
		run_test("#{v1}, #{v2}")
		get_result
	}
end
