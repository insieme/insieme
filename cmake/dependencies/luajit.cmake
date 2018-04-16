find_package(LuaJIT)

# use regular Lua as fallback
if(NOT LUAJIT_FOUND)
	find_package(Lua REQUIRED)
	set(LUAJIT_LIBRARY ${LUA_LIBRARIES})
	set(LUAJIT_INCLUDE_DIR ${LUA_INCLUDE_DIR})
endif()
