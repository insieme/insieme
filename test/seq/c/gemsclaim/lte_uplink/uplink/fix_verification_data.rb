FOUT = "verification_data_out.h"
FIN = "verification_data.h"

v = Array.new(100000)

File.open(FOUT, "w+") do |outf|
	IO.foreach(FIN) do |line|
		if line =~ /short ver(\d+) = (\d+);/
			v[$1.to_i] = $2.to_i
		elsif line =~ /ver_data\[(\d+)\] = ver(\d+);/
			outf.puts("ver_data[#{$1}] = #{v[$1.to_i]};")
		else
			outf.puts(line)
		end		
	end
end