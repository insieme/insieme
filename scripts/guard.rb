PRAGMA = /#pragma\s+once\s/
GUARD = /\s*#pragma\s+once\s+#ifndef __GUARD_\w+/m

Dir["**/*.h"].each do |filename|
	text = ""
	puts "Guarding file #{filename}..."
	File.open(filename, "r") do |fi|
		text = fi.read()
		text.encode!('UTF-8', 'UTF-8', :invalid => :replace)
	end
	if PRAGMA =~ text && GUARD !~ text
		guard_string = "__GUARD_#{filename.upcase.gsub('/','_').gsub('.','_')}"
		text.sub!(PRAGMA, "#pragma once\n#ifndef #{guard_string}\n#define #{guard_string}\n")
		text += "\n\n#endif // ifndef #{guard_string}\n"
		File.open(filename, "w+") do |fi|
			fi.puts(text)
		end
	end
end


