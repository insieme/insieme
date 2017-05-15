add_module_library(transform)

target_link_libraries(transform core)
target_link_libraries(transform utils)

# Boost
target_link_libraries(transform ${Boost_LIBRARIES})
target_include_directories(transform SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

glob_tests(transform_tests test)
foreach(test ${transform_tests})
	add_module_unittest(transform ${test})
endforeach(test)
