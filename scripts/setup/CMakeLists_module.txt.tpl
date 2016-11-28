find_package(Boost ${Boost_VERSION} EXACT REQUIRED COMPONENTS filesystem system)

glob_sources(%MODULE%_srcs src)
add_library(%MODULE% ${%MODULE%_srcs})
target_include_directories(%MODULE% PUBLIC include)
target_include_directories(%MODULE% SYSTEM PUBLIC ${Boost_INCLUDE_DIR})
target_link_libraries(%MODULE% ${Boost_LIBRARIES})

msvc_header_completion(%MODULE%_headers include)

glob_executables(%MODULE%_exes src)
foreach(exe ${%MODULE%_exes})
	get_filename_component(exe_name ${exe} NAME_WE)
	add_executable(%MODULE%_${exe_name} ${exe})
	target_link_libraries(%MODULE%_${exe_name} %MODULE%)
endforeach(exe)

glob_tests(%MODULE%_tests test)
foreach(test ${%MODULE%_tests})
	add_unittest(%MODULE% ${test})
endforeach(test)
