insieme_find_package(NAME LLVM SUPERBUILD TRUE)

include(ProcessorCount)
ProcessorCount(SLOTS)

if(NOT LLVMFOUND)

	message(STATUS "we install LLVM (Version ${LLVM_VERSION})")
	#download url expects version without dots...
	string(REPLACE "." "_" VERSION_STRING ${LLVM_VERSION})

	include(ProcessorCount)
	ProcessorCount(SLOT)

	#>= 3.5.0 tar.xz is used, below tar.gz
	if(${LLVM_VERSION} VERSION_LESS "3.5.0")
		set(ARCHIVE "tar.gz")
	else()
		set(ARCHIVE "tar.xz")
	endif()

	#clang
	if(${LLVM_VERSION} VERSION_GREATER "3.4")
		set(CFE_NAME "cfe")
	else()
		set(CFE_NAME "clang")
	endif()

	#download and unpack llvm
	ExternalProject_Add(llvm
		URL http://llvm.org/releases/${LLVM_VERSION}/llvm-${LLVM_VERSION}.src.${ARCHIVE} 
		CONFIGURE_COMMAND ""
		BUILD_COMMAND ""
		INSTALL_COMMAND ""
		LOG_DOWNLOAD 1
	)

	#get source_dir of llvm - clangsrc is unpacked into ${source-dir}/tools/clang/
	ExternalProject_Get_Property(llvm SOURCE_DIR) 

	#download and unpack clang into llvm-dir, patch clang, configure, make clang-only, make install
	ExternalProject_Add(clang
		#clang sources have to be in ${LLVM_SOURCE_DIR}/tools/clang
		SOURCE_DIR ${SOURCE_DIR}/tools/clang
		DEPENDS			llvm
		URL http://llvm.org/releases/${LLVM_VERSION}/${CFE_NAME}-${LLVM_VERSION}.src.${ARCHIVE}
		#patch is relative to LLVM_SOURCE_DIR 
		PATCH_COMMAND	cd ../.. && patch -p1  < ${CMAKE_CURRENT_LIST_DIR}/patches/insieme-clang-${LLVM_VERSION}.patch

		CONFIGURE_COMMAND 
			env "CFLAGS=-O3"
				"CXXFLAGS=-O3 -std=c++11" 
			${SOURCE_DIR}/configure --prefix=${THIRD_PARTY_LIBS_HOME}/llvm-${LLVM_VERSION} 
				--enable-shared=yes 
				--enable-assert=yes 
				--enable-debug-runtime=no 
				--enable-debug-symbols=no 
				--enable-optimized=yes
		BUILD_COMMAND	make REQUIRES_RTTI=1 clang-only -j${SLOTS}

		#build everything in llvm source dir -- everything is relative to llvm sourcedir
		BINARY_DIR	${SOURCE_DIR}	

		INSTALL_COMMAND make clang-only install
		LOG_DOWNLOAD 1
		LOG_UPDATE 1 
		LOG_CONFIGURE 1
		LOG_INSTALL 1
	)

endif()
