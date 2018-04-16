add_module_library(utils)

target_link_libraries(utils common)

# Boost
target_link_libraries(utils ${Boost_LIBRARIES})
target_include_directories(utils SYSTEM PUBLIC ${Boost_INCLUDE_DIRS})

# LuaJIT
target_link_libraries(utils ${LUAJIT_LIBRARY} m ${CMAKE_DL_LIBS})
target_include_directories(utils SYSTEM PUBLIC ${LUAJIT_INCLUDE_DIR})

glob_tests(utils_tests test)
foreach(test ${utils_tests})
	add_module_unittest(utils ${test})
endforeach(test)
