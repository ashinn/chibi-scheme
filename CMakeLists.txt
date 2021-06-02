#
# FIXME: This CMakeLists.txt is only for Win32 platforms for now
#

cmake_minimum_required(VERSION 3.12)

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/VERSION version)
string(STRIP ${version} version)

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/RELEASE release)
string(STRIP ${release} release)

project(chibi-scheme LANGUAGES C VERSION ${version}
    DESCRIPTION "Chibi-Scheme: minimal r7rs implementation, release: ${release}")

include(CheckIncludeFile)
include(CheckSymbolExists)
include(GNUInstallDirs)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

SET(CMAKE_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING
    "Build type: None, Debug, Release, RelWithDebInfo, MinSizeRel, or Sanitizer." FORCE)

if (NOT EXISTS ${CMAKE_BINARY_DIR}/CMakeCache.txt AND NOT CMAKE_BUILD_TYPE)
    # CMake doesn't have a default build type, so set one manually
    set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "" FORCE)
endif()


#
# Features
#

check_include_file(poll.h HAVE_POLL_H)
check_symbol_exists(ntp_gettime sys/timex.h HAVE_NTP_GETTIME)
check_symbol_exists(int_least8_t inttypes.h HAVE_STDINT_H)

if (WIN32 AND NOT CYGWIN)
    set(DEFAULT_SHARED_LIBS OFF)
else()
    set(DEFAULT_SHARED_LIBS ON)
endif()

option(BUILD_SHARED_LIBS "Build chibi-scheme as a shared library" ${DEFAULT_SHARED_LIBS})
option(SEXP_USE_BOEHM "Use Boehm garbage collection library" OFF)

if(SEXP_USE_BOEHM)
    find_library(BOEHMGC gc REQUIRED)
    find_path(BOEHMGC_INCLUDE NAMES gc/gc.h)
endif()

set(chibi-scheme-exclude-modules)
if(WIN32)
    set(chibi-scheme-exclude-modules
        # Following modules are not compatible with Win32
        lib/chibi/net.sld
        lib/chibi/process.sld
        lib/chibi/stty.sld
        lib/chibi/system.sld
        lib/chibi/time.sld
        lib/chibi/pty.sld)
endif()

#
# Default settings for all targets. We use an interface library here to not
# pollute/mutate global settings.
#

add_library(libchibi-common
    INTERFACE)

target_compile_definitions(libchibi-common
    INTERFACE
    SEXP_STATIC_LIBRARY=$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>
    BUILDING_DLL=$<AND:$<PLATFORM_ID:Windows>,$<BOOL:${BUILD_SHARED_LIBS}>>
    SEXP_USE_DL=$<BOOL:${BUILD_SHARED_LIBS}>
    SEXP_USE_INTTYPES=$<BOOL:${HAVE_STDINT_H}>
    SEXP_USE_NTPGETTIME=$<BOOL:HAVE_NTP_GETTIME>
    SEXP_USE_GREEN_THREADS=$<BOOL:HAVE_POLL_H>
    SEXP_USE_STRING_STREAMS=$<NOT:$<BOOL:${CYGWIN}>>
    SEXP_USE_BOEHM=$<BOOL:${SEXP_USE_BOEHM}>)

target_compile_options(libchibi-common
    INTERFACE
    $<$<C_COMPILER_ID:GNU>:-Wall>
    $<$<OR:$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:Clang>>:-Wall>
    $<$<CONFIG:SANITIZER>:-g
    -fsanitize=address,undefined,integer-divide-by-zero,float-divide-by-zero,float-cast-overflow,return
    -fno-omit-frame-pointer>)

target_include_directories(libchibi-common
    INTERFACE
    ${BOEHMGC_INCLUDE}
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_link_libraries(libchibi-common INTERFACE
    ${BOEHMGC}
    $<$<CONFIG:SANITIZER>:-fsanitize=address,undefined>
    $<$<PLATFORM_ID:Windows>:ws2_32>)

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

#
# Bootstrap
#

add_executable(chibi-scheme-bootstrap
    EXCLUDE_FROM_ALL
    ${chibi-scheme-srcs}
    main.c)

target_link_libraries(chibi-scheme-bootstrap PRIVATE libchibi-common)


#
# Core library
#

add_library(libchibi-scheme
    ${chibi-scheme-srcs})

target_link_libraries(libchibi-scheme
    PUBLIC libchibi-common)

set_target_properties(libchibi-scheme
    PROPERTIES
    SOVERSION ${CMAKE_PROJECT_VERSION_MAJOR}
    VERSION ${CMAKE_PROJECT_VERSION})


#
# Generate modules
#

file(GLOB_RECURSE slds RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    CONFIGURE_DEPENDS lib/*.sld)
if (chibi-scheme-exclude-modules)
    # CMake doesn't complain anymore about an empty 2nd argument, but 3.12 does. When we require a
    # more recent version, the if-guard should go.
    list(REMOVE_ITEM slds ${chibi-scheme-exclude-modules})
endif()

set(chibi-ffi ${CMAKE_CURRENT_SOURCE_DIR}/tools/chibi-ffi)
set(chibi-genstatic ${CMAKE_CURRENT_SOURCE_DIR}/tools/chibi-genstatic)

function(add_compiled_library cfile)
    if (NOT BUILD_SHARED_LIBS)
        return()
    endif()

    get_filename_component(basename ${cfile} NAME_WE)
    get_filename_component(libdir ${cfile} DIRECTORY)

    if(NOT IS_ABSOLUTE ${libdir})
        set(libdir ${CMAKE_CURRENT_BINARY_DIR}/${libdir})
    endif()

    file(RELATIVE_PATH libname ${CMAKE_CURRENT_BINARY_DIR} ${libdir}/${basename})
    string(REPLACE "/" "-" libname ${libname})

    add_library(${libname} ${cfile})
    target_link_libraries(${libname} PRIVATE libchibi-scheme)

    set_target_properties(${libname} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ${libdir}
        LIBRARY_OUTPUT_NAME ${basename}
        PREFIX "")
endfunction()

function(add_stubs_library stub)
    get_filename_component(stubdir ${stub} PATH)
    get_filename_component(basename ${stub} NAME_WE)
    set(stubfile ${CMAKE_CURRENT_SOURCE_DIR}/${stub})
    set(stubdir ${CMAKE_CURRENT_BINARY_DIR}/${stubdir})
    set(stubout ${stubdir}/${basename}.c)
    set(stubouts ${stubouts} ${stubout} PARENT_SCOPE)

    file(MAKE_DIRECTORY ${stubdir})
    add_custom_command(OUTPUT ${stubout}
        COMMAND chibi-scheme-bootstrap ${chibi-ffi} ${stubfile} ${stubout}
        DEPENDS ${stubfile} ${chibi-ffi}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    add_compiled_library(${stubout})
endfunction()

add_stubs_library(lib/chibi/crypto/crypto.stub)
add_stubs_library(lib/chibi/emscripten.stub)
add_stubs_library(lib/chibi/filesystem.stub)
add_stubs_library(lib/chibi/io/io.stub)
add_stubs_library(lib/scheme/bytevector.stub)
add_stubs_library(lib/srfi/144/math.stub)
add_stubs_library(lib/srfi/160/uvprims.stub)

if(NOT WIN32)
    add_stubs_library(lib/chibi/net.stub)
    add_stubs_library(lib/chibi/process.stub)
    add_stubs_library(lib/chibi/pty.stub)
    add_stubs_library(lib/chibi/stty.stub)
    add_stubs_library(lib/chibi/system.stub)
    add_stubs_library(lib/chibi/time.stub)
else()
    add_stubs_library(lib/chibi/win32/process-win32.stub)
endif()

add_custom_target(chibi-scheme-stubs DEPENDS ${stubouts})
add_dependencies(libchibi-scheme chibi-scheme-stubs)

add_compiled_library(lib/chibi/weak.c)
add_compiled_library(lib/chibi/heap-stats.c)
add_compiled_library(lib/chibi/disasm.c)
add_compiled_library(lib/chibi/ast.c)
add_compiled_library(lib/chibi/json.c)
add_compiled_library(lib/srfi/18/threads.c)
add_compiled_library(lib/chibi/optimize/rest.c)
add_compiled_library(lib/chibi/optimize/profile.c)
add_compiled_library(lib/srfi/27/rand.c)
add_compiled_library(lib/srfi/151/bit.c)
add_compiled_library(lib/srfi/39/param.c)
add_compiled_library(lib/srfi/69/hash.c)
add_compiled_library(lib/srfi/95/qsort.c)
add_compiled_library(lib/srfi/98/env.c)
add_compiled_library(lib/scheme/time.c)

#
# Generate clib.c for SEXP_USE_STATIC_LIBS
#

if (NOT BUILD_SHARED_LIBS)
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

    target_compile_definitions(libchibi-scheme
        PUBLIC
        SEXP_USE_STATIC_LIBS=1)

    target_sources(libchibi-scheme
        PUBLIC
        ${clibout})

    target_include_directories(libchibi-common
        INTERFACE
        $<BUILD_INTERFACE:${stuboutdir}/..>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
endif()

#
# Interpreter
#

add_executable(chibi-scheme
    main.c)

target_link_libraries(chibi-scheme
    PRIVATE libchibi-scheme)

#
# Generate "chibi/install.h"
#

if(WIN32)
    set(platform "windows")
elseif(CYGWIN)
    set(platform "cygwin")
elseif(APPLE)
    set(platform "macosx")
elseif(CMAKE_SYSTEM MATCHES "[Bb][Ss][Dd]")
    set(platform "bsd")
elseif(CMAKE_SYSTEM MATCHES "[Aa]ndroid")
    set(platform "android")
elseif(CMAKE_SYSTEM MATCHES "[Ss]un[Oo][Ss]")
    set(platform "solaris")
elseif (CMAKE_SYSTEM MATCHES "[Ll]inux")
    set(platform "linux")
else()
    set(platform "unix")
endif()


string(JOIN ":" default_module_path
    ${CMAKE_INSTALL_FULL_DATAROOTDIR}/chibi
    ${CMAKE_INSTALL_FULL_LIBDIR}/chibi
    ${CMAKE_INSTALL_FULL_DATAROOTDIR}/snow
    ${CMAKE_INSTALL_FULL_LIBDIR}/snow)

configure_file(include/chibi/install.h.in include/chibi/install.h)

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
        COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib tests/${e}.scm
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
    chibi/pty-test # Depends (chibi pty)
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
        COMMAND chibi-scheme -I ${CMAKE_CURRENT_BINARY_DIR}/lib
        -e "(import (${form}))"
        -e "(run-tests)"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

#
# Testing (embedding)
#

add_executable(test-foreign-apply-loop
    tests/foreign/apply-loop.c)

target_link_libraries(test-foreign-apply-loop
    PRIVATE libchibi-scheme)

add_test(NAME "foreign-apply-loop"
    COMMAND test-foreign-apply-loop
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_executable(test-foreign-typeid
    tests/foreign/typeid.c)

target_link_libraries(test-foreign-typeid
    PRIVATE libchibi-scheme)

add_test(NAME "foreign-typeid"
    COMMAND test-foreign-typeid
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
