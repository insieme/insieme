signatures = []

simpleGetterExp = /static (.*?) get\((.*?)& manager *?\)/
getterExp = /static (.*?) get\((.*?)& manager, (.*?)\)/

Dir["**/*.h"].each { |header|
	code = IO.read(header)
	# strip comments
	code.gsub!(/\/\/.*?$/, "")
	code.gsub!(/\/\*.*?\*\//, "")
	# find signatures
	code.gsub(simpleGetterExp) { |m| 
		ptrTypeName = $1
		managerTypeName = $2
		params = nil
		#puts("#{ptrTypeName} -- #{managerTypeName} -- #{params}")
		signatures << [ptrTypeName, managerTypeName, params]
	}
	code.gsub(getterExp) { |m| 
		ptrTypeName = $1
		managerTypeName = $2
		params = $3
		#puts("#{ptrTypeName} -- #{managerTypeName} -- #{params}")
		signatures << [ptrTypeName, managerTypeName, params]
	}
}

signatures = signatures.sort_by { |s| s[0] }

File.open("include/ast_builder.inl", "w+") { |inlFile|
File.open("src/ast_builder_impl.inl", "w+") { |inlFileImpl|
File.open("include/ast_builder_decls.inl", "w+") { |inlDeclFile|
	signatures.each { |sig|
		typeName = sig[0][0..-4]
		params = sig[2]
		funDecl = "#{typeName.sub(/[A-Z]/) {|c| c.downcase}}(#{params})"
		inlFile.puts("#{sig[0]} #{funDecl};")
		if(params)
			# remove default param assignments
			params = params.split(",").map { |param| param.sub(/=.*/,"") }.join(", ")
			funDecl = "#{typeName.sub(/[A-Z]/) {|c| c.downcase}}(#{params})"
			paramNames = params.split(",").map { |param| param[/\w* *\z/] }.join(", ")
			inlFileImpl.puts("#{sig[0]} ASTBuilder::#{funDecl} { return #{typeName}::get(manager, #{paramNames}); }")
		else
			inlFileImpl.puts("#{sig[0]} ASTBuilder::#{funDecl} { return #{typeName}::get(manager); }")
		end
		inlDeclFile.puts("class #{typeName}; typedef AnnotatedPtr<const #{typeName}> #{sig[0]};")
	}
} 
} 
}

# vector<std::pair<Identifier, TypePtr>> Entries
# vector<TypePtr> ElementTypeList