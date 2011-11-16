
class IRNode
	attr_accessor :name, :parent, :abstract, :category, :children, :strings, :ints
	
	def initialize(name, parent, abstract, category, children, strings, ints)
		@name = name
		@parent = parent
		@abstract = abstract
		@category = category
		@children = children
		@strings = strings
		@ints = ints
	end
	
	def forward_decls()
		"class #{name} : public class #{parent};\n" +
		"typedef Pointer<#{name}> #{name}Ptr;\n" +
		"typedef Address<#{name}> #{name}Address;"
	end
	
	def header_code()
	"class #{name} : public class #{parent} {\n"
	"	public:\n" +
	
	"};"
	end
	
	def impl_code()
	end
	
	def accessor_fun_decls()
		ret  = ""
		@children.each do |c|
			upcaseName = c[0][0].upcase + c[0][1..-1]
			type = c[1]
			ret += "#{type} get#{upcaseName}();"
			if(type ~= /list<(.*)>/) # list type
			end
			upcaseName = c[0][0].upcase + c[0][1..-1]
			ret += "#{c[1]} get#{upcaseName}();"
			if(upcaseName[-1] == "s") ret += "#{c[1]} get#{upcaseName}();"
		end
		ret
	end
end

nodes = []

node_def = IO.read("ir_nodes.def")

node_def.split("\n").each do |line|
	line.gsub!(/#.*/, "")
	line.strip!
	next if(line.empty?)
	
	parts = line.split(";").map { |p| p.strip }
	class_name = parts[0]
	parent_name = parts[1]
	abstract = (parts[2] == "true")
	category_id = parts[3]
	children = parts[4][1..-2].split(",").map { |c| c.strip.split(":") }
	string_members = parts[5][1..-2].split(",")
	int_members = parts[6][1..-2].split(",")
	nodes << IRNode.new(class_name, parent_name, abstract, category_id, children, string_members, int_members)
end

nodes.each do |node|
	puts node.forward_decls
end