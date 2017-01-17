add_module_library(annotations)

target_link_libraries(annotations common)
target_link_libraries(annotations core)
target_link_libraries(annotations utils)

# Boost
find_package(Boost ${BOOST_VERSION} EXACT REQUIRED)
target_link_libraries(annotations ${Boost_LIBRARIES})
target_include_directories(annotations SYSTEM PUBLIC ${Boost_INCLUDE_DIR})

glob_tests(annotations_tests test)
foreach(test ${annotations_tests})
	add_module_unittest(annotations ${test})
endforeach(test)
