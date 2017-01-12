add_module_library(utils)

target_link_libraries(utils common)

# Boost
find_package(Boost ${Boost_VERSION} EXACT REQUIRED COMPONENTS filesystem system)
target_link_libraries(utils ${Boost_LIBRARIES})
target_include_directories(utils SYSTEM PUBLIC ${Boost_INCLUDE_DIRS})

# LuaJIT
find_package(LuaJIT REQUIRED)
target_link_libraries(utils ${LuaJIT_LIBRARIES})
target_include_directories(utils SYSTEM PUBLIC ${LuaJIT_INCLUDE_DIRS})

glob_tests(utils_tests test)
foreach(test ${utils_tests})
	add_module_unittest(utils ${test})
endforeach(test)
