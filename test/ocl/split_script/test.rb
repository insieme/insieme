script_dir = 'split_script' # don't change

$num_devices = '2';
$test_name = 'vec_add';
$iteration = 3;

def get_result
	file_name = "worker_event_log.0000"
	first = `head -n 1 #{file_name} | awk -v x=4 '{print $x }'`
	last  = `tail -n 1 #{file_name} | awk -v x=4 '{print $x }'`
	last.to_i - first.to_i
end

def run_test (split_values)
	timer = 0;
	$iteration.times{
		ENV['IRT_OCL_SPLIT_VALUES'] = split_values
		result = `./#{$test_name}.ocl.test`
		#puts result
		t = get_result
		#puts t
		timer += t
	}
	puts "TIME: " + (timer/$iteration).to_s
end


ENV['IRT_NUM_WORKERS'] = $num_devices;
$path =  Dir.pwd.gsub!(script_dir, '')

#inside the test dir
Dir.chdir($path + $test_name); 

if $num_devices == '2'
	11.times{ |i|
		`rm worker_event_log*`
		v1 = i/10.round(1)
		v2 = (1.0 - v1).round(1)
		run_test("#{v1}, #{v2}")
		get_result
	}
end
