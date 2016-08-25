require 'date'

INFO = <<EOS
licensor.rb
Replaces existing license, prepends if no license, no changes if license up to date
Usage:
- ruby licensor.rb [license file] [code file]
  rewrites [code file]
- ruby licensor.rb [license file] [code file] [output file]
  reads [code file], writes to [output file]
EOS

# The license recognition:
# A comment at the start of the file, which contains "COPYRIGHT"
LICENSE_REGEXP = /\A\s*(\/\*|\{-).*?COPYRIGHT.*?(\*\/|-\})/mi

if(ARGV.length < 2 || ARGV.length > 3)
	puts(INFO)
	exit(-1)
end

license = IO.read(ARGV[0])
# update license year to current
license.sub!(/20XX/, Date.today.year.to_s)

file = IO.read(ARGV[1])

match = LICENSE_REGEXP.match(file)

# check if any existing license
if(match)
	# if up-to-date, exit
	exit(1) if(match[0].strip == license.strip)
	# else, delete existing license
	file.sub!(match[0], "");
end

# prepend new license
output = license + file.lstrip

# write ouptput to code file, or output file if specified 
outfn = ARGV[ARGV.length-1]

File.open(outfn, "wb+") do |f|
	f.print(output)
end

exit(0)
