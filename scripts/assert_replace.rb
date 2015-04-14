i = 0
j = 0

op_lookup = {
	"<" => "lt",
	"<=" => "le",
	">" => "gt",
	">=" => "ge",
	"==" => "eq",
	"!=" => "ne",
}

replacements = {
/assert\s*\(\s*(?:false|0)\s*\)\s*;/ => lambda { "assert_fail();" }, 											# assert_fail
/assert\s*\(\s*(?:false|0)\s*&&\s*(\"([^"]|\\")*")\s*\)\s*;/ =>
lambda { $1.include?("not implemented") ? "assert_not_implemented();" : "assert_fail() << #{$1};" }, 				# assert_fail + message
/assert\s*\(\s*((?:[\w.,:\(\), \[\]]|->)*)\s*\)\s*;/ => lambda { "assert_true(#{$1.strip});" },								# assert(single_thing)
/assert\s*\(\s*!\s*((?:[\w.,:\(\), \[\]!]|->)*)\s*\)\s*;/ => lambda { "assert_false(#{$1.strip});" },							# assert(!single_thing)
/assert\s*\(\s*((?:[\w.,:\(\), \[\]]|->)*)\s*&&\s*(\"([^"]|\\")*")\s*\)\s*;/ => lambda { "assert_true(#{$1.strip}) << #{$2};" },		# assert(single_thing && "message")
/assert\s*\(\s*!\s*((?:[\w.,:\(\), \[\]!]|->)*)\s*&&\s*(\"([^"]|\\")*")\s*\)\s*;/ => lambda { "assert_false(#{$1.strip}) << #{$2};" },	# assert(!single_thing && "message")
/assert\s*\(\s*((?:[\w.,:\(\),+\- \[\]]|->)*)\s*(==|>=|<=|>|<|!=)\s*(-?\s*[\d.]+u?|(?:[\w.:(), \[\]]|->)*\.size\(\))\s*&&\s*(\"([^"]|\\")*")\s*\)\s*;/ => 
lambda { "assert_#{op_lookup[$2]}(#{$1.strip}, #{$3.strip}) << #{$4};" },										# assert(single_thing [BINOP] number && "message")
/assert\s*\(\s*((?:[\w.,:\(\),+\- \[\]]|->)*)\s*(==|>=|<=|>|<|!=)\s*(-?\s*[\d.]+u?|(?:[\w.:(), \[\]]|->)*\.size\(\))\s*\)\s*;/ => 
lambda { "assert_#{op_lookup[$2]}(#{$1.strip}, #{$3.strip});" },												# assert(single_thing [BINOP] number)
/assert\s*\(\s*((?:[\w.,:\(\), \[\]]|->)*)\s*(==|>=|<=|>|<|!=)\s*(.*?NT_.*?)\s*&&\s*(\"([^"]|\\")*")\s*\)\s*;/ => 
lambda { "assert_#{op_lookup[$2]}(#{$1.strip}, #{$3.strip}) << #{$4};" },										# assert(single_thing [BINOP] NT && "message")
/assert\s*\(\s*((?:[\w.,:\(\), \[\]]|->)*)\s*(==|>=|<=|>|<|!=)\s*(.*?NT_.*?)\s*\)\s*;/ => 
lambda { "assert_#{op_lookup[$2]}(#{$1.strip}, #{$3.strip});" },												# assert(single_thing [BINOP] NT)
/assert\s*\(\s*((?:.*?(?:&&|\|\|).*?)+)\s*&&\s*(\"(?:[^"]|\\")*")\s*\)\s*;/ =>
lambda { "assert_true(#{$1}) << #{$2.strip};" },														# assert(multi_exps && "message")
/assert\s*\(\s*((?:.*?(?:&&|\|\|).*?){2,})\s*\)\s*;/ =>
lambda { "assert_true(#{$1.strip});" },																# assert(multi_exps)
}

Dir["**/*.{h,cpp,cc}"].each do |fn|
	change = false
	lines = IO.readlines(fn).map do |line|
		if line =~ /assert\s*\(/
			puts "="*30
			puts line
		end
		res = line
		replacements.each do |lhs, rhs|
			if(line =~ lhs)
				next if $`.end_with?("_")
				res = line.sub($&, rhs.call)
				puts res
				change = true
				i+=1
				break
			end
		end
		res
	end
	
	if change
		j += 1
		File.open(fn, 'w') do |file|
			file.puts lines
		end
	end
end

puts "Fixed: #{i}"
puts "Files: #{j}"