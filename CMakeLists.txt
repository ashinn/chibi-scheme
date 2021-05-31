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


endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

#
# Features
#

check_include_file(poll.h HAVE_POLL_H)
check_include_file(stdint.h HAVE_STDINT_H)

if (WIN32 AND NOT CYGWIN)
    set(DEFAULT_SHARED_LIBS OFF)
else()
    set(DEFAULT_SHARED_LIBS ON)
endif()

option(BUILD_SHARED_LIBS "Build chibi-scheme as a shared library" ${DEFAULT_SHARED_LIBS})

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

if (NOT BUILD_SHARED_LIBS)
    target_compile_definitions(libchibi-common INTERFACE SEXP_STATIC_LIBRARY=1)
endif()

target_include_directories(libchibi-common
    INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

if (NOT BUILD_SHARED_LIBS)
    target_compile_definitions(libchibi-common INTERFACE SEXP_USE_DL=0)
else()
    target_compile_definitions(libchibi-common INTERFACE SEXP_USE_DL=1)
endif()

if(HAVE_STDINT_H)
    target_compile_definitions(libchibi-common INTERFACE SEXP_USE_INTTYPES)
endif()

if(HAVE_NTP_GETTIME)
    target_compile_definitions(libchibi-common INTERFACE SEXP_USE_NTPGETTIME)
endif()

if(NOT HAVE_POLL_H)
    # Disable green threads: It depends on non-blocking I/O
    target_compile_definitions(libchibi-common INTERFACE SEXP_USE_GREEN_THREADS=0)
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

#
# Bootstrap
#

add_executable(chibi-scheme-bootstrap
    EXCLUDE_FROM_ALL
    ${chibi-scheme-srcs}
    main.c)

if(WIN32)
    target_link_libraries(chibi-scheme-bootstrap
        PRIVATE ws2_32 libchibi-common)
else()
    target_link_libraries(chibi-scheme-bootstrap
        PRIVATE libchibi-common)
endif()


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




add_dependencies(libchibi-scheme chibi-scheme-stubs)

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
elseif(WIN32)
    target_link_libraries(libchibi-scheme ws2_32)
    target_compile_definitions(libchibi-scheme PUBLIC BUILDING_DLL=1)
endif()

function(bless_chibi_scheme_executable tgt)
    target_link_libraries(${tgt} libchibi-scheme)
    if(WIN32 AND NOT ${BUILD_SHARED_LIBS})
        target_link_libraries(${tgt} ws2_32)
    endif()
endfunction()

#
# Interpreter
#

add_executable(chibi-scheme
    main.c)

bless_chibi_scheme_executable(chibi-scheme)

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


set(default_module_path
    ""
    #"${CMAKE_INSTALL_PREFIX}/${thePrefix}${pathsep}${CMAKE_INSTALL_PREFIX}/bin"
    )

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
        COMMAND chibi-scheme -e "(import (${form}))"
        -e "(run-tests)"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

#
# Testing (embedding)
#

add_executable(test-foreign-apply-loop
    tests/foreign/apply-loop.c)

bless_chibi_scheme_executable(test-foreign-apply-loop)

add_test(NAME "foreign-apply-loop"
    COMMAND test-foreign-apply-loop
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_executable(test-foreign-typeid
    tests/foreign/typeid.c)

bless_chibi_scheme_executable(test-foreign-typeid)

add_test(NAME "foreign-typeid"
    COMMAND test-foreign-typeid
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
