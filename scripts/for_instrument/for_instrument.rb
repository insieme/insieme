HEADER = <<EOS
#include <stdio.h>
#include <stdbool.h>
unsigned long long __ins_g_counts[100000];
bool __ins_end(const char* file, unsigned line) {
	static bool inited = false;
	FILE *f;
	if(!inited) {
		f = fopen("ins_counts.log", "w");
		inited = true;
	} else {
		f = fopen("ins_counts.log", "a");
	}
	fprintf(f, "NUM_ITS(% 64s:% 8u):% 16llu\\n", file, line, __ins_g_counts[line]);
	fclose(f);
	__ins_g_counts[line] = 0;
	return false;
}

EOS

class String
  def scan2(regexp)
    names = regexp.names
    scan(regexp) do |match|
      yield Hash[names.zip(match)]
    end
  end
end

# matching parens
MP = '(?<mp>[^();]*(\(([^();]*\g<mp>*[^();]*)*\))*[^();]*)'
REGEX = /(?<all>for\s*\((?<init>#{MP}+);(?<cond>\g<mp>+);(?<inc>\g<mp>+)\))/m


Dir["**/*.c"].each do |filename|
	file = IO.read(filename)
	result = file.clone

	i = 0
	file.scan2(REGEX) do |m|
		cond = "#{m["cond"]} || __ins_end(__FILE__, __LINE__)"
		inc = "#{m["inc"]}, __ins_g_counts[__LINE__]++"
		result.gsub!(m["all"], "\nfor(#{m["init"]}; #{cond}; #{inc})") 
	end


	File.open(filename, "w+") do |f|
		f.print(HEADER + result)
	end

end

