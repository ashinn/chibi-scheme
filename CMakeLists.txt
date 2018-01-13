#
# FIXME: This CMakeLists.txt is only for Win32 platforms for now
#

cmake_minimum_required(VERSION 2.8.7)
project(chibi-scheme)

include(CheckIncludeFile)

if(APPLE)
    message(FATAL_ERROR 
        "DYLD platforms are not supported with this CMakeLists.txt. Use Makefile instead.")
endif()

if(UNIX)
    message(FATAL_ERROR 
        "UNIX platforms are not supported with this CMakeLists.txt. Use Makefile instead.")
endif()

#
# Features
#

check_include_file(poll.h HAVE_POLL_H)
check_include_file(stdint.h HAVE_STDINT_H)
# option(CHIBI_SCHEME_USE_DL "Use dynamic loading" ON)
set(CHIBI_SCHEME_USE_DL OFF)

if(CHIBI_SCHEME_USE_DL)
    add_definitions(-DSEXP_USE_DL=1)
else()
    add_definitions(-DSEXP_USE_DL=0)
endif()

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    if(MSVC)
        # On MSVC, SEXP_64_BIT is not supported for now (#438)
        add_definitions(-DSEXP_64_BIT=0)
    else()
        add_definitions(-DSEXP_64_BIT=1)
    endif()
elseif(CMAKE_SIZEOF_VOID_P EQUAL 4)
    add_definitions(-DSEXP_64_BIT=0)
endif()

if(HAVE_STDINT_H)
    add_definitions(-DSEXP_USE_INTTYPES=1)
endif()

if(NOT HAVE_POLL_H)
    # Disable green threads: It depends on non-blocking I/O
    add_definitions(-DSEXP_USE_GREEN_THREADS=0)
endif()

set(chibi-scheme-exclude-modules)
if(WIN32)
    add_definitions(-DBUILDING_DLL)
    set(chibi-scheme-exclude-modules
        # Following modules are not compatible with Win32
        lib/chibi/net.sld
        lib/chibi/process.sld
        lib/chibi/stty.sld
        lib/chibi/system.sld
        lib/chibi/time.sld)
endif()

#
# Sources
#

set(chibi-scheme-srcs
    # SEXP
    gc.c
    sexp.c
    bignum.c
    gc_heap.c

    # Eval
    opcodes.c
    vm.c
    eval.c
    simplify.c)

include_directories(
    include
    ${CMAKE_CURRENT_BINARY_DIR}/include)

#
# Bootstrap
#

add_executable(chibi-scheme-bootstrap
    ${chibi-scheme-srcs}
    main.c)

if(WIN32)
    target_link_libraries(chibi-scheme-bootstrap ws2_32)
endif()

if(CYGWIN OR WIN32)
    set(soext ".dll")
else()
    set(soext ".so")
endif()

#
# Generate modules
#

# FIXME: Currently, it depends on GLOB thus we have to re-run CMake
#        when we've gotten additional/removed library 

file(GLOB_RECURSE stubs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/lib 
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/*.stub)
file(GLOB_RECURSE slds RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} 
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/*.sld)
list(REMOVE_ITEM slds ${chibi-scheme-exclude-modules})

set(chibi-ffi ${CMAKE_CURRENT_SOURCE_DIR}/tools/chibi-ffi)
set(chibi-genstatic ${CMAKE_CURRENT_SOURCE_DIR}/tools/chibi-genstatic)

set(stuboutdir ${CMAKE_CURRENT_BINARY_DIR}/stubs/lib)
foreach(e ${stubs})
    get_filename_component(stubdir ${e} PATH)
    get_filename_component(basename ${e} NAME_WE)
    set(stubfile ${CMAKE_CURRENT_SOURCE_DIR}/lib/${e})
    set(stubdir ${stuboutdir}/${stubdir})
    set(stubout ${stubdir}/${basename}.c)
    file(MAKE_DIRECTORY ${stubdir})
    add_custom_command(OUTPUT ${stubout}
        COMMAND chibi-scheme-bootstrap
        ${chibi-ffi} ${stubfile} ${stubout}
        DEPENDS ${stubfile} ${chibi-ffi}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    list(APPEND stubouts ${stubout})
endforeach()
add_custom_target(chibi-scheme-stubs DEPENDS ${stubouts})

#
# Generate clib.c for SEXP_USE_STATIC_LIBS
#

string(REPLACE ";" "\n" genstatic-input "${slds}")
set(clibin ${CMAKE_CURRENT_BINARY_DIR}/clib-in.txt)
set(clibout ${CMAKE_CURRENT_BINARY_DIR}/clib.c)
set(genstatic-helper 
    ${CMAKE_CURRENT_LIST_DIR}/contrib/chibi-genstatic-helper.cmake)
file(WRITE ${clibin} "${genstatic-input}")

add_custom_command(OUTPUT ${clibout}
    COMMAND 
    ${CMAKE_COMMAND} 
    -DEXEC=$<TARGET_FILE:chibi-scheme-bootstrap>
    -DGENSTATIC=${chibi-genstatic}
    -DSTUBS=${clibin}
    -DOUT=${clibout}
    -P ${genstatic-helper}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS 
    chibi-scheme-bootstrap
    ${chibi-genstatic}
    ${genstatic-helper}
    ${slds})

#
# Interpreter
#

include_directories(
    .
    ${stuboutdir}/..)
add_executable(chibi-scheme
    ${chibi-scheme-srcs}
    ${clibout}
    main.c)

set_target_properties(chibi-scheme
    PROPERTIES COMPILE_DEFINITIONS "SEXP_USE_STATIC_LIBS=1")

add_dependencies(chibi-scheme chibi-scheme-stubs)

if(WIN32)
    target_link_libraries(chibi-scheme ws2_32)
endif()

#
# Generate "chibi/install.h" 
#

if(CYGWIN OR WIN32)
    set(thePrefix "bin")
else()
    set(thePrefix "lib")
endif()

if(WIN32)
    set(pathsep "\\;")
else()
    set(pathsep ":")
endif()

if(WIN32)
    set(platform "windows")
else()
    set(platform "unknown")
endif()

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/RELEASE release)
string(STRIP ${release} release)

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/VERSION version)
string(STRIP ${version} version)
set(version "${version}-cmake")

set(default_module_path
    ""
    #"${CMAKE_INSTALL_PREFIX}/${thePrefix}${pathsep}${CMAKE_INSTALL_PREFIX}/bin"
    )

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/chibi)

file(WRITE
    ${CMAKE_CURRENT_BINARY_DIR}/include/chibi/install.h
    "#define sexp_so_extension \"${soext}\"
#define sexp_default_module_path \"${default_module_path}\"
#define sexp_platform \"${platform}\"
#define sexp_version \"\"
#define sexp_release_name \"${release}\"")

# 
# Testing
#

enable_testing()

set(chibi-scheme-tests
    r7rs-tests
    ## Not connected
    #division-tests
    #r5rs-tests
    #syntax-tests
    #unicode-tests
    ## Require threads
    # lib-tests
    )

foreach(e ${chibi-scheme-tests})
    add_test(NAME "${e}"
        COMMAND chibi-scheme tests/${e}.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

file(GLOB_RECURSE srfi_tests RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/lib
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/srfi/*/test.sld)

file(GLOB_RECURSE chibi_scheme_tests RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/lib
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/chibi/*-test.sld)

set(testexcludes
    # Excluded tests
    chibi/filesystem-test
    chibi/memoize-test
    chibi/term/ansi-test
    chibi/weak-test

    # Not ported to Win32
    srfi/18/test # Threading
    chibi/doc-test # Depends (chibi time)
    chibi/log-test
    chibi/system-test
    chibi/tar-test # Depends (chibi system)
    chibi/process-test # Not applicable
    )

set(testlibs)
foreach(e ${srfi_tests} ${chibi_scheme_tests})
    get_filename_component(pth ${e} PATH)
    get_filename_component(nam ${e} NAME_WE)
    list(APPEND testlibs ${pth}/${nam})
endforeach()
list(REMOVE_ITEM testlibs ${testexcludes})

foreach(e ${testlibs})
    string(REGEX REPLACE "/" "_" testname ${e})
    string(REGEX REPLACE "/" " " form ${e})
    add_test(NAME "lib_${testname}"
        COMMAND chibi-scheme -e "(import (${form}))"
        -e "(run-tests)"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

