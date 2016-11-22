#!/usr/bin/env ruby

require "fileutils"

@srcdir = ARGV[0]
@destdir = ARGV[1]

@incdir = "include"
@basedir = Dir.pwd
@verbose = true
@debugMode = false

@file_ext = "dl"
@decltoken = "decl"
@inittoken = "init"
@comptoken = "comp"
@membertoken = "member"
@usingtoken = "using"
@mangle = "D474L06"
@ir_dl = "entities/ir"

abort "Dough takes exactly two arguments!" if ARGV.length != 2
abort "Source dir (#{@srcdir}) does not exist!" unless File.directory?(@srcdir)
abort "Destination dir (#{@destdir}) does not exist!" unless File.directory?(@destdir)
abort "Invalid include dir (#{@incdir}): Does not exist in '#{@srcdir}'!" unless File.directory?(@srcdir + "/" + @incdir)

# "Normalize" src and dest dir to absolute paths
@srcdir = File.expand_path(@srcdir);
@destdir = File.expand_path(@destdir);

def extract_relation_name(name, input_arr)
  arr = input_arr.grep(/^\s*\.#{name}\s+/)
  arr.map! {|line| line.gsub(/^\s*\.#{name}\s+(\w+).*/, '\1') }
  return arr
end

def append_output_to_all_relations(name, input_arr)
  input_arr.map! {|line| line.gsub(/^(\s*\.#{name}\s+\w+\s*\([^\)]+\)\s*((?!input|output).)*)$/, '\1 output') }
end

def create_mangle(filename, decl)
  escpaed_filename = filename.gsub(/[\/\.]/, '_')
  return @mangle + "_" + escpaed_filename + "_" + decl
end

def camelcase_to_underscore(str)
  return str.gsub(/([^\/])([A-Z])/, '\1_\2').downcase
end

def expand_using_decls(lines, filename)
  # Get all using-declarations
  usings = {}
  lines.each do |line|
    next unless match = line.match(/^\s*\.#{@usingtoken}\s+(\w+)\s+from\s+(\w+(?:\/[\w]+)*)(?:\s+as\s+(\w+))?.*/)
    use, from, as = match.captures
    from = camelcase_to_underscore(from + "." + @file_ext)
    as = as ? as : use
    res = create_mangle(from, use)
    usings[as] = res
  end

  #Replace relations with their mangled names
  usings.each do |rel, new_rel|
    next if rel == "_"
    replace_count = 0

    lines.each do |line|
      next unless line.gsub!(/(^|\W)#{rel}(\s*\()/, '\1' + new_rel + '\2') or
                  line.gsub!(/(^|\W)(\.#{@inittoken}\s+\w+\s*=\s*)#{rel}(\s*)/, '\1\2' + new_rel + '\3')
      replace_count += 1
    end

    puts "\x1B[33mWarning:\033[0m Superfluous inclusion of '#{rel}' in file '#{filename}'." if replace_count == 0

  end
end

def convert_usings_to_includes(lines, filename)
  lines.unshift ".#{@usingtoken} _ from " + @ir_dl
  lines.each do |line|
    line.gsub!(/^\s*\.#{@usingtoken}\s+\w+\s+from\s+(\w+(?:\/[\w]+)*)(?:\s+as\s+\w+)?.*/) do
      target_filename = camelcase_to_underscore(Regexp.last_match[1]) + "." + @file_ext
      target_filename_fullpath = @destdir + "/" + @incdir + "/" + target_filename
      puts "\x1B[1;31mError:\033[0m Target file '#{target_filename}' does not exist!" unless File.file? target_filename_fullpath
      '#include "' + target_filename + '"'
    end
  end
end


# Step 1: Clean up dest dir and copy files
FileUtils.rm_rf(Dir.glob(@destdir + "/*"))

Dir.chdir @srcdir
source_files = Dir.glob("*." + @file_ext)
FileUtils.cp(source_files, @destdir)

Dir.chdir @incdir
include_files = Dir.glob("**/*." + @file_ext)
FileUtils.mkdir_p(@destdir + "/" + @incdir)
FileUtils.cp_r(".", @destdir + "/" + @incdir)

Dir.chdir @basedir


# Step 2: Name-mangle declarations in header files
Dir.chdir(@destdir + "/" + @incdir)
include_files.each do |filename|

  puts "Header file: #{filename}" if @verbose

  lines = File.read(filename).split("\n")

  # Skip mangling for ir.dl
  unless filename == @ir_dl + "." + @file_ext then

    # Get all decls, inits and comps in this file
    decls = extract_relation_name(@decltoken, lines)
    inits = extract_relation_name(@inittoken, lines)
    comps = extract_relation_name(@comptoken, lines)

    decls.concat(comps)

    # Create mangles
    decls.each do |decl|
      new_decl = create_mangle(filename, decl)
      lines.map! {|line| line.gsub(/(^|[\W])#{decl}([^\w]+)/, '\1' + new_decl + '\2') }
    end

    inits.each do |init|
      new_init = create_mangle(filename, init)
      lines.map! {|line| line.gsub(/(\.#{@inittoken}\s+)#{init}(\s*=\s*\w+)/, '\1' + new_init + '\2') }
      lines.map! {|line| line.gsub(/(^|[\W])#{init}(\.\w+\s*\()/, '\1' + new_init + '\2') }
    end

  end

  # Replace relation names according to using-declarations
  expand_using_decls(lines, filename)
  convert_usings_to_includes(lines, filename)

  # Change 'member' declarations back to decl and add pragma once
  lines.map! {|line| line.gsub(/(\s*\.)#{@membertoken}([^\w]+)/, '\1decl\2') }
  lines.unshift "#pragma once"

  # Convert all relations to output-relations
  append_output_to_all_relations(@decltoken, lines) if @debugMode

  # Done. Write new content to file
  File.open(filename, "w") do |file|
    file.puts(lines)
  end

end
Dir.chdir(@basedir)


#Step 3: Name-mangle using-declarations in source files
Dir.chdir(@destdir)
source_files.each do |filename|

  puts "Source file: #{filename}" if @verbose

  lines = File.read(filename).split("\n")

  # Replace relation names according to using-declarations
  expand_using_decls(lines, filename)
  convert_usings_to_includes(lines, filename)

  # Convert all relations to output-relations
  append_output_to_all_relations(@decltoken, lines) if @debugMode

  # Done. Write new content to file
  File.open(filename, "w") do |file|
    file.puts(lines)
  end

end
Dir.chdir(@basedir)

