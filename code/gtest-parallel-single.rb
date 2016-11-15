#!/usr/bin/env ruby

# TODO:
# * merge gtest xml files?

require 'open3'
require 'io/wait'
require 'optparse'
require 'thread'

options = {}

optparse = OptionParser.new do |opts|
	opts.banner = "Usage: #{$PROGRAM_NAME} [options] gtest_binary"

	options[:workers] = 4
	opts.on('-w', '--workers N', 'Number of parallel workers N' ) do |n|
		n = 1 if n == 0
		options[:workers] = n
	end

	opts.on( '-h', '--help', 'Display this screen' ) do
		puts opts
		exit
	end
end

optparse.parse!

# get test case names
testcases=`#{ARGV.first} --gtest_list_tests`

list=testcases.split("\n").map{|x| x.split(" ")[0].strip }
# remove disabled tests if present
list.reject! {|t| t =~ /DISABLED.*/ }
# remove valgrind output if present
list.reject! {|t| t =~ /^==[0-9]+==.*/}

# remove gtest output header, merge test case names
list.shift(1)

# parse output format of gtest test case lister
cases=[]
prefix=""
list.each do |x|
	if (x.end_with? ".") then prefix = x;
	else cases << "#{prefix}#{x}" end
end
list=cases

# shuffle test cases reproducibly to balance load
list.shuffle!(random: Random.new(17))

number_of_workers = options[:workers].to_i
max_string_length = 0

# initialize everything
result = Hash.new
list.each do |y|
	result[y] = "NOT RUN"
	max_string_length = y.length if y.length > max_string_length
end

puts "Running #{result.length} tests in #{ARGV.first} using #{number_of_workers} threads"


# create a work queue
queue = Queue.new

# add work to the queue
list.each { |x| queue << x }

# process work in parallel
threads = []
number_of_workers.times do
	threads << Thread.new do
		case_name = ""
                status_string = ""
                time = ""
                old_case_name = ""
		until queue.empty?
			test = queue.pop(true) rescue nil
			if test
				selection = "#{test}"
				regex = /\[\s*([A-Z]+)\s*\]\s([a-zA-Z0-9_\.\/]+)(?:.*\(([0-9]+)\sms\))?/
				cmd = "#{ARGV.first} --gtest_filter=" + selection + " 2>/dev/null"
				Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thr|
					stdin.close
					stdout.each do |line|
						# grep for status string and case name - also matches "RUN" status lines
						# write output when "RUN" for next test case or "tear-down" of the entire suite occurs
						matched = regex.match(line)
						if(matched != nil)
							status_string = matched[1].strip
							case_name = matched[2].strip
							# not necessarily present
							time = matched[3].strip if matched[3] != nil

							result[case_name] = status_string
							# first matching line per test case is always "RUN"
							if(status_string == "RUN")
								if(old_case_name != "")
									# if there is no second one with "OK" for the previous test case, we assume that it failed
									if result[old_case_name] != "OK"
										result[old_case_name] = "FAILED" 
									end
									print "#{old_case_name.ljust(max_string_length)}\t#{result[old_case_name].rjust(8)} (#{time} ms)\n"
								end
								old_case_name = case_name
							end
						end
						# drop any gtest footer output
						# output last test case explicitely if it crashed
						matched = /tear-down/.match(line)
						break if matched != nil
					end
					# consume any leftover output
					stdout.read if stdout.ready?
			#		stdout.close
			#		stderr.close
					if(wait_thr.value != 0)
						if(wait_thr.value.signaled?)
							result[case_name] = "*** FAILED (signal #{wait_thr.value.termsig})"
						else
							result[case_name] = "*** FAILED (exit code #{wait_thr.value.exitstatus})"
						end
						puts "Failed command:"
						puts cmd
					end
					print "#{case_name.ljust(max_string_length)}\t#{result[case_name].rjust(8)} (#{time} ms)\n"
				end
			end
		end
	end
end

# wait
threads.each(&:join)

# summary
footer="\nFailed:\n"
failed_count = 0
result.sort.each do |k,v|
	if v != "OK"
		footer << "\t#{k.ljust(max_string_length)} #{v}\n"
		failed_count = failed_count + 1
	end
end

puts "\n#{result.length} tests total, of which #{failed_count} failed or were not run due to failures.\n"


puts footer if(failed_count > 0)

# return value is the number of failed test cases
exit failed_count