add_module_library(%MODULE%)

target_link_libraries(%MODULE% ${Boost_LIBRARIES})
target_include_directories(%MODULE% SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

glob_executables(%MODULE%_exes src)
foreach(exe ${%MODULE%_exes})
	add_module_executable(%MODULE% ${exe})
endforeach(exe)

glob_tests(%MODULE%_tests test)
foreach(test ${%MODULE%_tests})
	add_module_unittest(%MODULE% ${test})
endforeach(test)
