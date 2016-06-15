#!/bin/env ruby

# This is a helper script for dealing with the issue that clang cannot parse many GCC intrinsics.
# It searches for standard header include files named *intrin.h, and copies forward declarations, 
# typedefs and enum declarations to the builtin_header directory of the Insieme source tree.
#
#                                              NOTE: 
#
# The script does NOT adhere to preprocessor directives, hence the output might require manual postprocessing

require 'set'

def wrap_and_print(text, outfile)
	header = "// Insieme builtin intrinsics hack\n" 
	File.open(outfile, "w") do |outfile|
		puts "Writing #{File.path(outfile)}"
		outfile.write("#pragma once")
		outfile.write("\n\n#{header}\n")
		outfile.write("#ifdef __clang__\n\n")
		outfile.write(text)
		outfile.write("\n#endif // __clang__ #{header}\n\n")
	end
end

function_pattern = /extern\s+__inline\s+([\w ]+)\s+(?:__attribute__\s*[_a-zA-Z0-9(,)\s+]*?)?\n(_[^\s]+)\s+\(([^\)]*?)\)/
enum_pattern = /(\w*)\s*enum\s+(\w*)([^}]+?})\s*(\w*);/

types_header_filename = "hacked_types.h"

# variants of these standard types are ignored
standard_types = %w(void char short int unsigned long float double enum)

if ARGV.length != 2
	puts "Usage: ./check-hack.rb <insieme_builtin_header_directory> <gcc_root_directory>"
	exit 1
end

builtin_dir_location = ARGV[0]
gcc_source_dir_location = ARGV[1]

types = Set.new
types_out = ""
enums = Hash.new

Dir.glob("#{gcc_source_dir_location}/include/**/*intrin.h").each do |infile|
	content = File.open(infile, "r").read
		
	function_match = content.scan(function_pattern)
	# if we actually found intrinsics, do work
	if function_match.size > 0
		decls_out = ""

		# handle functions, add types as they appear in parameters or as return types
		function_match.each do |func|
			# return type, identifier, paramter list
			decls_out += "extern #{func[0]} #{func[1]} (#{func[2]});\n"
			# add return type to set of types
			types.add("#{func[0]}")
			# add each parameter type to set of types
			func[2].split(",").each do |param|
				param_list = param.strip.split(" ")
				# remove identifier if present, keep only type
				param_list = param_list[0...-1] if param_list.size > 1
				types.add("#{param_list.join(" ")}")
			end
		end

		# save (possibly typedef'd) enums in hash
		# key is enum's concatenated identifier (before and after enumerator list)
		content.scan(enum_pattern).each do |x|
			enums["#{x[1]}#{x[3]}"] = "#{x[0]} enum #{x[1]} #{x[2]} #{x[3]};\n"
		end
		# remove types that are variants of standard types
		types.delete_if{ |x| standard_types.any? { |y| x.include?(y) } }
		# lookup remaining types
		types.each do |type|
			typedef_match = content.match(/typedef\s+(.*?#{type.strip}[^;]*;)/)
			if(typedef_match != nil)
				types_out += "#{typedef_match[0]}\n"
			end
		end
		# put function declarations in file copies with the same name, write include directive for single file holding types and enums
		wrap_and_print("#include <#{types_header_filename}>\n\n" + decls_out, "#{builtin_dir_location}/#{File.basename(infile)}")
	end
end

# concatenate enums
enums_out = enums.map { |k,v| v }.join("\n")

# print enums and typedefs to separate file included by all others
wrap_and_print(enums_out + "\n\n" + types_out, "#{builtin_dir_location}/#{types_header_filename}")

