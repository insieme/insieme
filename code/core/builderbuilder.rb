signatures = []

simpleGetterExp = /static (.*?) get\((.*?)& manager *?\)/
getterExp = /static (.*?) get\((.*?)& manager, ?([^{};]*)\)/

Dir["**/ir_*.h"].each { |header|
	code = IO.read(header)
	# strip comments
	code.gsub!(/\/\/.*?$/, "")
	code.gsub!(/\/\*.*?\*\//, "")
	# find signatures
	code.gsub(simpleGetterExp) { |m| 
		ptrTypeName = $1.strip
		managerTypeName = $2.strip
		params = nil
		#puts("#{ptrTypeName} -- #{managerTypeName} -- #{params}")
		if ptrTypeName !~ /##/		
			signatures << [ptrTypeName, managerTypeName, params]
		end
	}
	code.gsub(getterExp) { |m| 
		ptrTypeName = $1.strip
		managerTypeName = $2.strip
		params = $3.gsub("\n", "").strip
		#puts("#{ptrTypeName} -- #{managerTypeName} -- #{params}")
		if ptrTypeName !~ /##/		
			signatures << [ptrTypeName, managerTypeName, params]
		end	
	}
}

signatures = signatures.sort_by { |s| s[0] }

sigcache = ""
if File.exists?("signatures.cache")
	sigcache = IO.read("signatures.cache").strip
end
newsigs = signatures.to_s.strip

if sigcache == newsigs
	# puts "Builderbuilder.rb : no change in signatures detected, skipping file generation."
else 
	File.open("signatures.cache", "w+") { |sigfile| sigfile.puts(newsigs) }

	File.open("include/insieme/core/ir_builder.inl", "w+") { |inlFile|
	File.open("src/ir_builder_impl.inl", "w+") { |inlFileImpl|
		signatures.each { |sig|
			typeName = sig[0][0..-4]
			params = sig[2]
			funDecl = "#{typeName.sub(/[A-Z]/) {|c| c.downcase}}(#{params})"
			inlFile.puts("#{sig[0].ljust(30)} #{funDecl} const;")
			if(params)
				# remove default param assignments
				params = params.split(",").map { |param| param.sub(/=.*/,"").strip }.join(", ")
				funDecl = "#{typeName.sub(/[A-Z]/) {|c| c.downcase}}(#{params})"
				paramNames = params.split(",").map { |param| param[/\w* *\z/].strip }.join(", ")
				inlFileImpl.puts("#{sig[0].ljust(18)} IRBuilder::#{funDecl} const { return #{typeName}::get(manager, #{paramNames}); }")
			else
				inlFileImpl.puts("#{sig[0].ljust(18)} IRBuilder::#{funDecl} const { return #{typeName}::get(manager); }")
			end
		}
	} 
	}
end

# vector<std::pair<Identifier, TypePtr>> Entries
# vector<TypePtr> ElementTypeList
