add_module_library(backend)

target_link_libraries(backend annotations)
target_link_libraries(backend common)
target_link_libraries(backend core)
target_link_libraries(backend transform)
target_link_libraries(backend utils)

# Boost
target_link_libraries(backend ${Boost_LIBRARIES})
target_include_directories(backend SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

glob_tests(backend_tests test)
foreach(test ${backend_tests})
	string(REPLACE "${CMAKE_CURRENT_SOURCE_DIR}/test/" "" test_relpath ${test})
	if(test_relpath MATCHES "snippets")
		add_module_unittest(backend ${test} PARALLEL)
	else()
		add_module_unittest(backend ${test})
	endif()
endforeach(test)
